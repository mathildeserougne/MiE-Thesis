## BACI - FINDING RIGHT CONTROLS ######################################################

# Based on the EU commission communication, we have HS codes for "exposed" products.
# We already used those codes to assess the amount of exposure India has to face.

# In this script, we use the codes to compare the pre-trends (before treatment)
# and doing so, we prepare our diff-in-diff regression.

## PACKAGES

# Uncomment packages installation if necessary
#install.packages(c("dplyr","readr","stringr","ggplot2", "scales"))
# install.packages("broom")
# Libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(broom)



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


### COMPARING PRE-TRENDS ########################################

# we want to check if, prior to the policy, the commercial trends are the same.
# we can do it comparing exposed product



## 1) ALL EXPOSED PRODUCTS ALTOGETHER TO THE WHOLE WORLD

# We compare the evolution of exports of exposed products and exports of OTHER products

# Calculate total value of exports for exposed products each year
total_value_exposed_by_year <- baci_indian_exposed %>%
  group_by(year) %>%
  summarise(total_exposed_value = sum(value, na.rm = TRUE))

# Calculate total value of exports for non-exposed products each year
baci_indian_nonexposed <- baci_indian_pov %>%
  filter(!str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))

total_value_nonexposed_by_year <- baci_indian_nonexposed %>%
  group_by(year) %>%
  summarise(total_nonexposed_value = sum(value, na.rm = TRUE))

# Merge the data for plotting
plot_data <- full_join(total_value_exposed_by_year, total_value_nonexposed_by_year, by = "year")

# Plot the evolution of exports for exposed and non-exposed products
ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = total_exposed_value, color = "Exposed Products"), size = 1) +
  geom_line(aes(y = total_nonexposed_value, color = "Non-Exposed Products"), size = 1) +
  labs(title = "Evolution of Exports: Exposed vs Non-Exposed Products",
       x = "Year",
       y = "Export Volume",
       color = "Product Type") +
  scale_color_manual(values = c("Exposed Products" = "orange", "Non-Exposed Products" = "pink")) +
  scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# STACKED BAR DIAGRAM OMG

install.packages("tidyr")
library(tidyr)

# merging data
plot_data <- full_join(total_value_exposed_by_year, total_value_nonexposed_by_year, by = "year") %>%
  mutate(total_value = total_exposed_value + total_nonexposed_value) %>%
  pivot_longer(cols = c(total_exposed_value, total_nonexposed_value),
               names_to = "product_type", values_to = "value")

# stacked bar chart
ggplot(plot_data, aes(x = factor(year), y = value, fill = product_type)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "Export Volume: Exposed vs Non-Exposed Products",
       x = "Year",
       y = "Export Volume",
       fill = "Product Type") +
  scale_fill_manual(values = c("total_exposed_value" = "orange", "total_nonexposed_value" = "pink")) +
  theme_minimal()






## 2) COMPARING EXPOSED AND NON-EXPOSED FLOWS FROM INDIA TO EUROPE

# PLOTTING THE TRENDS

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

# merge data for the plot
plot_data_eurozone <- full_join(total_value_exposed_by_year_eurozone, total_value_nonexposed_by_year_eurozone, by = "year")

# stacked bar diagram to europe
plot_data_eurozone <- plot_data_eurozone %>%
  mutate(total_value = total_exposed_value + total_nonexposed_value) %>%
  pivot_longer(cols = c(total_exposed_value, total_nonexposed_value),
               names_to = "product_type", values_to = "value")

# stacked bar diagram to Europe
ggplot(plot_data_eurozone, aes(x = factor(year), y = value, fill = product_type)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "Export Volume to Europe: Exposed vs Non-Exposed Products",
       x = "Year",
       y = "Export Volume",
       fill = "Product Type") +
  scale_fill_manual(values = c("total_exposed_value" = "orange", "total_nonexposed_value" = "pink")) +
  theme_minimal()



## DOING A REGRESSION ON BOTH GROUPS TO SAY THAT COMPARABLE
# we just want to prove rigorously that trends over 2013-2019 are comparable

# filter data on pre-treatment period 2013-2019
plot_data_eurozone_2013_2019 <- plot_data_eurozone %>%
  filter(year <= 2019)

# linear model on exposed products
lm_exposed <- lm(value ~ year, data = filter(plot_data_eurozone_2013_2019, product_type == "total_exposed_value"))
summary_lm_exposed <- tidy(lm_exposed)

# linear model on non-exposed products
lm_nonexposed <- lm(value ~ year, data = filter(plot_data_eurozone_2013_2019, product_type == "total_nonexposed_value"))
summary_lm_nonexposed <- tidy(lm_nonexposed)

# regression results:
summary_lm_exposed
summary_lm_nonexposed

# Chow test to compare the slopes, coefficients of the regressions.
# We use a combined model to test the interaction and compare if parallel.
combined_data <- plot_data_eurozone_2013_2019 %>%
  mutate(product_type = ifelse(product_type == "total_exposed_value", "Exposed", "Non-Exposed"))

lm_combined <- lm(value ~ year * product_type, data = combined_data)
summary_lm_combined <- tidy(lm_combined)

# Combined linear model
summary(lm_combined)

# For parallel trends, we need significant interaction.
# If interaction term (year:product_typeNon-Exposed) not significant, suggests that slopes are parallel.

# WORKS.




### 3) COMPARING EXPOSED PRODUCTS, EUROPE VS REST OF THE WORLD
# so region as control

# we just plot the evolution of flows of exposed products to EU and to rest of the world


# DATA part

# exposed flows to eu
baci_indian_exposed_eurozone <- baci_indian_pov %>%
  filter(importer %in% eu_members) %>%
  filter(str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))

# exposed flows to the rest of the world
baci_indian_exposed_rest_of_world <- baci_indian_pov %>%
  filter(!importer %in% eu_members) %>%
  filter(str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))

# exports of exposed products to eu each year
total_value_exposed_eurozone_by_year <- baci_indian_exposed_eurozone %>%
  group_by(year) %>%
  summarise(total_exposed_value_eurozone = sum(value, na.rm = TRUE))

# total exports of exposed products to the rest of the world
total_value_exposed_rest_of_world_by_year <- baci_indian_exposed_rest_of_world %>%
  group_by(year) %>%
  summarise(total_exposed_value_rest_of_world = sum(value, na.rm = TRUE))

# merge data for plot
plot_data_comparison <- full_join(total_value_exposed_eurozone_by_year, total_value_exposed_rest_of_world_by_year, by = "year")

# data for stacked bar diagram
plot_data_comparison_long <- plot_data_comparison %>%
  pivot_longer(cols = c(total_exposed_value_eurozone, total_exposed_value_rest_of_world),
               names_to = "destination", values_to = "value")


# diagram
ggplot(plot_data_comparison_long, aes(x = factor(year), y = value, fill = destination)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "Export Volume of Exposed Products: Europe vs Rest of the World",
       x = "Year",
       y = "Export Volume",
       fill = "Destination") +
  scale_fill_manual(values = c("total_exposed_value_eurozone" = "blue", "total_exposed_value_rest_of_world" = "purple")) +
  theme_minimal()


## LINEAR REGRESSIONS PART

# lm for exposed products to eu
lm_exposed_eurozone <- lm(value ~ year, data = filter(plot_data_comparison_long, destination == "total_exposed_value_eurozone"))
summary_lm_exposed_eurozone <- tidy(lm_exposed_eurozone)

# lm for exposed products to world
lm_exposed_rest_of_world <- lm(value ~ year, data = filter(plot_data_comparison_long, destination == "total_exposed_value_rest_of_world"))
summary_lm_exposed_rest_of_world <- tidy(lm_exposed_rest_of_world)

# reg results
summary_lm_exposed_eurozone
summary_lm_exposed_rest_of_world

# chow to compare slopes
combined_data_comparison <- plot_data_comparison_long %>%
  mutate(destination = ifelse(destination == "total_exposed_value_eurozone", "Europe", "Rest of the World"))

lm_combined_comparison <- lm(value ~ year * destination, data = combined_data_comparison)
summary(lm_combined_comparison)


# NOT PARALLEL.


















