## DIFFERENCE IN DIFFERENCES FOR BACI DATA ############################################
#######################################################################################

# This script gathers the pre-trends representation, tests for parallel trends, and DiD.

## PACKAGES ####################################################################

# Uncomment packages installation if necessary
install.packages(c("dplyr","readr","stringr","ggplot2", "scales"))
install.packages("broom")
# Libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(broom)
library(tidyr)


## DATA PROCESSING ############################################################

## DATA (as defined in the script 'vulnerability in trade') 
# this is just all flows coming from India
baci_indian_pov <- read_csv("~/work/baci_indian_pov.csv")

# Getting a filtered version with just the exposed products
# HS codes communicated by the EU commission
hs_prefixes <- c("25", "76", "31", "28", "29")
# Filtering the data to keep products with code starting with the mentioned prefix
# this is flows of exposed products coming from India
baci_indian_exposed <- baci_indian_pov %>%
  filter(str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))


# getting a filtered version with just europe
# List of EU countries
eu_members <- c(
  276, 40, 56, 100, 196, 191, 208, 724, 233, 246,
  251, 300, 348, 372, 380, 428, 440, 442, 470, 528,
  616, 620, 203, 642, 703, 705, 752
)

# Filter data to get only exports towards from India to the EU
baci_indian_pov_eurozone <- baci_indian_pov %>%
  filter(importer %in% eu_members)


################################################################################
## PART 1 - EXPOSED VS NON EXPOSED FROM INDIA TO EUROPE ########################



## A) Representing the trends with base 100 ####

# exposed and non-exposed products from India to EU, base 100 is 2013 

# flows of exposed products from INDIA to EUROPE
baci_indian_exposed_eurozone <- baci_indian_pov_eurozone %>%
  filter(str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))

# total value of exposed flow to europe each year
total_value_exposed_by_year_eurozone <- baci_indian_exposed_eurozone %>%
  group_by(year) %>%
  summarise(total_exposed_value = sum(value, na.rm = TRUE))

# total flows not exposed from india to europe
baci_indian_nonexposed_eurozone <- baci_indian_pov_eurozone %>%
  filter(!str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))

total_value_nonexposed_by_year_eurozone <- baci_indian_nonexposed_eurozone %>%
  group_by(year) %>%
  summarise(total_nonexposed_value = sum(value, na.rm = TRUE))

# merge data
plot_data_eurozone <- full_join(total_value_exposed_by_year_eurozone, total_value_nonexposed_by_year_eurozone, by = "year")

# NORMALISATION BASE 100 (année 2013)
base_year <- 2013
base_exposed <- plot_data_eurozone %>%
  filter(year == base_year) %>%
  pull(total_exposed_value)

base_nonexposed <- plot_data_eurozone %>%
  filter(year == base_year) %>%
  pull(total_nonexposed_value)

plot_data_eurozone_norm <- plot_data_eurozone %>%
  mutate(
    exposed_index = (total_exposed_value / base_exposed) * 100,
    nonexposed_index = (total_nonexposed_value / base_nonexposed) * 100
  ) %>%
  select(year, exposed_index, nonexposed_index) %>%
  pivot_longer(cols = c(exposed_index, nonexposed_index),
               names_to = "product_type", values_to = "index_value")

# plot normalized trends
ggplot(plot_data_eurozone_norm, aes(x = factor(year), y = index_value, color = product_type, group = product_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Normalized Export Index to Europe (Base 2013 = 100)",
       x = "Year",
       y = "Index (Base 2013 = 100)",
       color = "Product Type") +
  scale_color_manual(values = c("exposed_index" = "orange", "nonexposed_index" = "pink"),
                     labels = c("Exposed Products", "Non-Exposed Products")) +
  theme_minimal()



## B) Doing the test for parallelism


# filter normalized data 2013 to 2019
plot_data_eurozone_norm_2013_2019 <- plot_data_eurozone_norm %>%
  filter(year >= 2013, year <= 2019)

# regression for each group of products
lm_exposed_norm <- lm(index_value ~ year, data = filter(plot_data_eurozone_norm_2013_2019, product_type == "exposed_index"))
lm_nonexposed_norm <- lm(index_value ~ year, data = filter(plot_data_eurozone_norm_2013_2019, product_type == "nonexposed_index"))

# model summary
summary_lm_exposed_norm <- tidy(lm_exposed_norm)
summary_lm_nonexposed_norm <- tidy(lm_nonexposed_norm)

# print results
print("Regression on exposed products (normalized, 2013-2019):")
print(summary_lm_exposed_norm)

print("Regression on non-exposed products (normalized, 2013-2019):")
print(summary_lm_nonexposed_norm)

# combined model with interaction for trend similarity
combined_norm_data <- plot_data_eurozone_norm_2013_2019 %>%
  mutate(product_type = ifelse(product_type == "exposed_index", "Exposed", "Non-Exposed"))

lm_combined_norm <- lm(index_value ~ year * product_type, data = combined_norm_data)
summary_lm_combined_norm <- tidy(lm_combined_norm)

# combined model summary
print("Combined regression with interaction (test de tendance parallèle):")
print(summary(lm_combined_norm))



## C) interpretation ?

# over the period, significant growth of exposed exports. 
# not significant for the non-exposed products.
# the combined model suggests that we cannot exclude the parallel trends hypothesis.


## D) Difference in differences


# we know about parallel trends over 2013-2019.
# we will exclude the Covid shock from 2020.
# because as seen in the macro stats, overall quick growth rebound.

## D.1 just 2021 compared to previous trend

# i - prepare data (exclude 2020 to avoid confounded effects)
plot_data_did <- plot_data_eurozone_norm %>%
  filter(year %in% c(2013:2019, 2021)) %>%
  mutate(
    treated = ifelse(product_type == "exposed_index", 1, 0),
    post = ifelse(year >= 2021, 1, 0),
    did = treated * post
  )

# ii - DiD regression
lm_did <- lm(index_value ~ treated + post + did, data = plot_data_did)
summary_did <- summary(lm_did)

# iii - print results
print("Difference-in-Differences estimation (excluding 2020):")
print(summary_did)

# iv - interpretation
# did coefficient is est causal effect of treatment over exposed products (2021 law)
# 28% more than non exposed but only 0.2 p value so not significant.


## D.2 something cleaner going up until 2023

# filter to exclude 2020 and go to 2023
plot_data_did <- plot_data_eurozone_norm %>%
  filter(year %in% c(2013:2019, 2021:2023)) %>%
  mutate(
    treated = ifelse(product_type == "exposed_index", 1, 0),
    post = ifelse(year >= 2021, 1, 0),
    did = treated * post
  )

# regression did
lm_did <- lm(index_value ~ treated + post + did, data = plot_data_did)
summary_did <- summary(lm_did)

# print results
print("Difference-in-Differences estimation (excluding 2020):")
print(summary_did)

# interpretation? positive
# did coefficient significant at 5% level !
# after the announcement in 2021, actually contemporary to a stronger growth of volumes exported for exposed products.


# plot
library(ggplot2)

ggplot(plot_data_did, aes(x = year, y = index_value, color = product_type, group = product_type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Visualization of the DiD",
       x = "Year",
       y = "Index value",
       color = "Product type") +
  scale_color_manual(values = c("exposed_index" = "orange", "nonexposed_index" = "blue")) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) + 
  theme_minimal() +
  theme(legend.position = "top")




# FIXED EFFECTS DID #############

## Now, we can try to do a more precise regression, including fixed effects.

## not including 2020?
# Fixed effects for year and products
install.packages("fixest")
library(fixest)

plot_data_did <- plot_data_did %>%
  mutate(
    year = as.factor(year),
    product_type = as.factor(product_type)
  )
# regression with the fixed effects
did_fe <- feols(index_value ~ did | year + product_type, data = plot_data_did)
summary(did_fe)
# very significant, stronger growth for exposed products...


## including 2020?
plot_data_did_fe <- plot_data_eurozone_norm %>%
  filter(year >= 2013, year <= 2023) %>%  # On garde tout, y compris 2020
  mutate(
    treated = ifelse(product_type == "exposed_index", 1, 0),
    post = ifelse(year >= 2021, 1, 0),
    did = treated * post
  )


library(fixest)

did_fe <- feols(index_value ~ did | year + product_type, 
                data = plot_data_did_fe, 
                cluster = ~year)
summary(did_fe)
# less strong effect but just as significant.
# the result is robust despite the shock that happened with covid.


# but this is with a not very convincing control group.


## changing the control group to get something actually meaningful.

unique(plot_data_eurozone_norm$product_type)







