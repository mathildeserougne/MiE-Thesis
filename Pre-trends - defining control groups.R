## BACI - FINDING RIGHT CONTROLS ######################################################

# Based on the EU commission communication, we have HS codes for "exposed" products.
# We already used those codes to assess the amount of exposure India has to face.

# In this script, we use the codes to compare the pre-trends (before treatment)
# and doing so, we prepare our diff-in-diff regression.

## PACKAGES

# Uncomment packages installation if necessary
# install.packages(c("dplyr","readr","stringr","ggplot2", "scales"))
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
# means that we have to find a more accurate control: either a country or a synthetic one.




## Finding a better control to Europe than "rest-of-the-world"

# Listing the main importers of Indian-exposed-products: 

# exposed flows from india
baci_indian_exposed <- baci_indian_pov %>%
  filter(str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))
# sum of the values for each importer
top_importers <- baci_indian_exposed %>%
  group_by(importer, importer_name) %>%  # Assurez-vous que importer_name est dans vos données
  summarise(total_import_value = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_import_value))
# display main importers
print(top_importers)



# top ones: US, China, Korea, Brazil, Türkiye

# Let's plot the trends for thos top countries and compare it to Europe.
# step of VISUALISATION

# Interest countries.
countries_of_interest <- c("USA", "China", "Rep. of Korea", "Brazil", "TÃ¼rkiye")

# Filter data for exposed products.
baci_indian_exposed <- baci_indian_pov %>%
  filter(str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))

# Filter data for countries of interest and the EU.
baci_indian_exposed_filtered <- baci_indian_exposed %>%
  filter(importer_name %in% c(countries_of_interest, "European Union") | importer %in% eu_members)

# Sum of export values for each year and each region/country of interest.
baci_indian_exposed_filtered <- baci_indian_exposed_filtered %>%
  mutate(region = ifelse(importer %in% eu_members, "European Union", importer_name)) %>%
  group_by(year, region) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(baci_indian_exposed_filtered, aes(x = year, y = total_value, color = region)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Evolution of Exposed Products Exports from India",
       x = "Year",
       y = "Export Volume",
       color = "Region") +
  scale_color_manual(
    values = c("European Union" = "blue",
               "USA" = "red",
               "China" = "green",
               "Rep. of Korea" = "purple",
               "Brazil" = "orange",
               "TÃ¼rkiye" = "brown"),
    #labels = c("European Union", "USA", "China", "South Korea", "Brazil", "Türkiye")
  ) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


# Visual candidates: best fits would seem to be US and China and Korea.
# Next step are then the statistical tests: how significant are the differences? Can we keep a parallel trends hypothesis?


## first version: not correct because estimates on 2013-2023 (so tests parallelism before and after the treatment)

# We just create a function to do the Chow test between two regions
perform_chow_test <- function(data, region1, region2) {
  # filter data for the regions
  data_filtered <- data %>%
    filter(region %in% c(region1, region2))
  
  # lm for region1
  lm_region1 <- lm(total_value ~ year, data = filter(data_filtered, region == region1))
  summary_lm_region1 <- tidy(lm_region1)
  
  # lm for region2
  lm_region2 <- lm(total_value ~ year, data = filter(data_filtered, region == region2))
  summary_lm_region2 <- tidy(lm_region2)
  
  # display regression results
  print(paste("Regression for", region1))
  print(summary_lm_region1)
  print(paste("Regression for", region2))
  print(summary_lm_region2)
  
  # chow test to compare significant or not differences
  combined_data <- data_filtered %>%
    mutate(region = ifelse(region == region1, region1, region2))
  
  lm_combined <- lm(total_value ~ year * region, data = combined_data)
  summary_lm_combined <- tidy(lm_combined)
  
  # print combined model with interaction
  print("Combined Model with Interaction")
  print(summary(lm_combined))
}


# PARALLEL TRENDS BETWEEN EU AND USA?
perform_chow_test(baci_indian_exposed_filtered, "European Union", "USA")
# The export trends to EU and USA are not signif. different
# Non significant interaction term: good signal for parallel trends.


# PARALLEL TRENDS BETWEEN EU AND CHINA?
perform_chow_test(baci_indian_exposed_filtered, "European Union", "China")
# Significant difference
# Suggests non-parallel trends


# PARALLEL TRENDS BETWEEN EU AND KOREA?
perform_chow_test(baci_indian_exposed_filtered, "European Union", "Rep. of Korea")
# not parallel !



## second version: tests if parallel BEFORE the treatment

# chow test between 2 regions over the right period
perform_chow_test <- function(data, region1, region2, start_year, end_year) {
  # filter data for right period
  data_filtered <- data %>%
    filter(year >= start_year & year <= end_year) %>%
    filter(region %in% c(region1, region2))
  
  # lm for region1
  lm_region1 <- lm(total_value ~ year, data = filter(data_filtered, region == region1))
  summary_lm_region1 <- tidy(lm_region1)
  
  # lm for region2
  lm_region2 <- lm(total_value ~ year, data = filter(data_filtered, region == region2))
  summary_lm_region2 <- tidy(lm_region2)
  
  # lm results
  print(paste("Regression for", region1))
  print(summary_lm_region1)
  print(paste("Regression for", region2))
  print(summary_lm_region2)
  
  # chow test
  combined_data <- data_filtered %>%
    mutate(region = ifelse(region == region1, region1, region2))
  
  lm_combined <- lm(total_value ~ year * region, data = combined_data)
  summary_lm_combined <- summary(lm_combined)
  
  # combined model output
  print("Combined Model with Interaction")
  print(summary_lm_combined)
}

# parallelism EU-USA 2013-2020?
perform_chow_test(baci_indian_exposed_filtered, "European Union", "USA", 2013, 2020)
# good for parallel trends!

# parallelism EU-China 2013-2020?
perform_chow_test(baci_indian_exposed_filtered, "European Union", "China", 2013, 2020)
# good for parallel trends on that period!

# parallelism EU-Korea 2013-2020?
perform_chow_test(baci_indian_exposed_filtered, "European Union", "Rep. of Korea", 2013, 2020)
# works fine


## maybe i could build a synthetic country out of the three?




### SYNTHETIC CONTROL ATTEMPT 1  ####
## FAIL SKIP THE FOLLOWING BLOCKS UNTIL 'GOOD' 


# so weight each country by its level of output, and average it to get the trend of the synthetic us x chine x korea


# Créer la ligne synthétique avec la somme pondérée de value par année
synthetic_exports <- baci_indian_exposed %>%
  filter(importer_name %in% c("China", "USA", "Rep. of Korea")) %>%
  group_by(year) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    importer = 999,
    importer_name = "synthetic",
    .groups = "drop"
  )

# Ajouter à la base existante
baci_indian_exposed <- bind_rows(baci_indian_exposed, synthetic_exports)



# maintenant je vais comparer ça aux pays européens
#voir si le contrôle artificiel contrôle bien

# Créer un dataframe avec les trois groupes
df_plot <- baci_indian_exposed %>%
  mutate(group = case_when(
    importer == 999 ~ "Synthetic",
    importer_name %in% c("China", "USA", "Rep. of Korea") ~ importer_name,
    importer %in% eu_members ~ "European Union",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group)) %>%
  group_by(year, group) %>%
  summarise(exports = sum(value, na.rm = TRUE), .groups = "drop")

# Graphique
ggplot(df_plot, aes(x = year, y = exports, color = group)) +
  geom_line(size = 1) +
  labs(
    title = "Évolution des exportations indiennes (2013–2023)",
    x = "Année",
    y = "Exportations (valeur totale)",
    color = "Destination"
  ) +
  theme_minimal()



## test statistique !


# Fonction test de tendances parallèles entre EU et le pays synthétique
perform_parallel_trends_test <- function(data, start_year, end_year) {
  # Préparer les données
  eu_members <- c(
    276, 40, 56, 100, 196, 191, 208, 724, 233, 246,
    251, 300, 348, 372, 380, 428, 440, 442, 470, 528,
    616, 620, 203, 642, 703, 705, 752
  )
  
  data_test <- data %>%
    filter(year >= start_year, year <= end_year) %>%
    mutate(region = case_when(
      importer == 999 ~ "Synthetic",
      importer %in% eu_members ~ "European Union",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(region)) %>%
    group_by(year, region) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")
  
  # Régressions séparées
  lm_eu <- lm(total_value ~ year, data = filter(data_test, region == "European Union"))
  lm_syn <- lm(total_value ~ year, data = filter(data_test, region == "Synthetic"))
  
  cat("\n Régression EU :\n")
  print(tidy(lm_eu))
  
  cat("\n Régression Synthetic :\n")
  print(tidy(lm_syn))
  
  # Modèle avec interaction pour test de tendance parallèle
  lm_interact <- lm(total_value ~ year * region, data = data_test)
  
  cat("\n Modèle combiné avec interaction :\n")
  print(summary(lm_interact))
  
  cat("\n Interprétation : si l’interaction 'year:regionSynthetic' n’est pas significative,\n")
  cat("→ alors les tendances sont parallèles.\n")
}

# Test de tendance parallèle EU vs Synthetic entre 2013 et 2020
perform_parallel_trends_test(baci_indian_exposed, start_year = 2013, end_year = 2020)



####



## SYNTHETIC COUNTRY ATTEMPT 2 ##
# GOOD 



# data for our three countries of interest (China, USA, Korea)
group_3 <- baci_indian_exposed %>%
  filter(importer_name %in% c("China", "USA", "Rep. of Korea")) %>%
  group_by(year, importer_name) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  summarise(exports = mean(value), group = "Synthetic (mean of 3)", .groups = "drop")

# data for eu and three countries
group_other <- baci_indian_exposed %>%
  mutate(group = case_when(
    importer_name %in% c("China", "USA", "Rep. of Korea") ~ importer_name,
    importer %in% eu_members ~ "European Union",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group)) %>%
  group_by(year, group) %>%
  summarise(exports = sum(value, na.rm = TRUE), .groups = "drop")

# combine the two
df_plot <- bind_rows(group_3, group_other)

# plot
library(ggplot2)
ggplot(df_plot, aes(x = year, y = exports, color = group)) +
  geom_line(size = 1) +
  labs(
    title = "Exports of exposed products from India",
    x = "Year",
    y = "Exported value",
    color = "Destination"
  ) +
  theme_minimal()



# stat tests

perform_chow_test_synthetic_vs_eu <- function(data, start_year, end_year) {
  # eu country codes
  eu_members <- c(
    276, 40, 56, 100, 196, 191, 208, 724, 233, 246,
    251, 300, 348, 372, 380, 428, 440, 442, 470, 528,
    616, 620, 203, 642, 703, 705, 752
  )
  
  # sum over eu countries
  eu_data <- data %>%
    filter(importer %in% eu_members, year >= start_year, year <= end_year) %>%
    group_by(year) %>%
    summarise(total_value = sum(value, na.rm = TRUE), region = "European Union", .groups = "drop")
  
  # synthetic country (mean of the three)
  syn_data <- data %>%
    filter(importer_name %in% c("China", "USA", "Rep. of Korea"), year >= start_year, year <= end_year) %>%
    group_by(year, importer_name) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    summarise(total_value = mean(value), region = "Synthetic", .groups = "drop")
  
  # merge
  data_filtered <- bind_rows(eu_data, syn_data)
  
  # linear models of the regions
  lm_region1 <- lm(total_value ~ year, data = filter(data_filtered, region == "European Union"))
  lm_region2 <- lm(total_value ~ year, data = filter(data_filtered, region == "Synthetic"))
  
  print("Regression for European Union")
  print(tidy(lm_region1))
  
  print("Regression for Synthetic")
  print(tidy(lm_region2))
  
  # combined model with interaction
  lm_combined <- lm(total_value ~ year * region, data = data_filtered)
  print("Combined Model with Interaction")
  print(summary(lm_combined))
}



perform_chow_test_synthetic_vs_eu(baci_indian_exposed, 2013, 2020)
# SLAYYYYYY






###############################################################################
#### ANOTHER REPRESENTATION OF PARALLEL TRENDS: NORMALISED ONE (2013=100)
###############################################################################


library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


### exposed and non-exposed products from India to EU, base 100 is 2013 ##########

## A) Representing the trends with base 100

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



## B) Re-doing the test for parallelism

library(broom)

# Filter normalised data 2013 et 2019
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



## 3) interpretation ?

# over the period, significant growth of exposed exports. 
# not significant for the non-exposed products.
# the combined model suggests that we cannot exclude the parallel trends hypothesis.




## D) Looking for better controls (normalised)

## Parallel trends for Indian exports to the EU: finding a more accurate control.
# we find that the trends do not seem that parallel between exposed and non exposed.
# let's do better.

# hs groups to compare
baci_indian_pov <- baci_indian_pov %>%
  mutate(hs_2 = str_sub(as.character(product), 1, 2))

baci_indian_eu <- baci_indian_pov %>%
  filter(importer %in% eu_members)

# Agrégation par groupe hs_2 et année
hs_yearly <- baci_indian_eu %>%
  group_by(hs_2, year) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")

# Normalisation (base 100 en 2013) pour chaque groupe hs_2
hs_index <- hs_yearly %>%
  group_by(hs_2) %>%
  mutate(
    base_value = total_value[year == 2013],
    index_value = (total_value / base_value) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(index_value))


# Plot de l'évolution normalisée pour chaque groupe HS
ggplot(hs_index, aes(x = factor(year), y = index_value, color = hs_2, group = hs_2)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    title = "Évolution normalisée des exportations indiennes vers l'UE par groupe HS (Base 2013 = 100)",
    x = "Année",
    y = "Indice (Base 2013 = 100)",
    color = "Groupe HS"
  ) +
  theme_minimal() +
  theme(legend.position = "right")



# this obviously does not work.
# let us instead work with quadratic distance...

library(dplyr)

# 1) Identifier les groupes HS exposés (CBAM) : 
hs_exposed <- hs_prefixes

# Convertir hs_prefixes en vecteur character pour comparer facilement
hs_exposed_char <- as.character(hs_exposed)

# 2) Calculer la trajectoire moyenne exposée (moyenne des groupes exposés)
exposed_trends <- hs_index %>%
  filter(hs_2 %in% hs_exposed_char, year >= 2013, year <= 2018) %>%
  group_by(year) %>%
  summarise(mean_index = mean(index_value, na.rm = TRUE)) %>%
  arrange(year)

# 3) Fonction pour calculer la distance quadratique moyenne avec la trajectoire exposée
calculate_mse <- function(df_group, exposed_df) {
  merged <- merge(df_group, exposed_df, by = "year", all = FALSE)
  mse <- mean((merged$index_value - merged$mean_index)^2, na.rm = TRUE)
  return(mse)
}

# 4) Calculer le MSE pour chaque groupe HS (non exposé)
hs_distances <- hs_index %>%
  filter(year >= 2013, year <= 2018) %>%
  group_by(hs_2) %>%
  summarise(mse = calculate_mse(cur_data(), exposed_trends)) %>%
  ungroup()

# 5) Retirer les groupes exposés de la liste (car ce sont les traités)
hs_distances_controls <- hs_distances %>%
  filter(!hs_2 %in% hs_exposed_char) %>%
  arrange(mse)

# 6) Afficher les meilleurs contrôles (groupes HS avec tendance la plus proche)
print("Groupes HS avec tendances les plus proches des exposés (potentiels contrôles) :")
print(hs_distances_controls)


# result: 
#[1] "Groupes HS avec tendances les plus proches des exposés (potentiels contrôles) :"
#> print(hs_distances_controls)
## A tibble: 91 × 2
#hs_2    mse
#<chr> <dbl>
#  1 14     78.1
#2 11    215. 
#3 56    309. 
#4 91    339. 
#5 84    442. 
#6 90    444. 
#7 92    532. 
#8 95    607. 
#9 19    780. 
#10 58    832. 
# ℹ 81 more rows
# ℹ Use `print(n = ...)` to see more rows

# visualisation

top_controls <- hs_distances_controls %>% slice_head(n = 10)

print(top_controls)

# Optionnel : visualiser leur tendance normalisée
hs_index %>%
  filter(hs_2 %in% top_controls$hs_2, year >= 2013, year <= 2018) %>%
  ggplot(aes(x = year, y = index_value, color = hs_2, group = hs_2)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Top 10 groupes HS non exposés avec tendances proches des exposés",
       x = "Année",
       y = "Indice (base 2013 = 100)") +
  theme_minimal()

# one with the cbam exposed to compare...
# 1) Tendance agrégée exposés (2013-2018)
exposed_trends_subset <- hs_index %>%
  filter(hs_2 %in% hs_exposed_char, year >= 2013, year <= 2018) %>%
  group_by(year) %>%
  summarise(index_value = mean(index_value, na.rm = TRUE)) %>%
  mutate(hs_2 = "Exposés Agrégés")

# 2) Tendances groupes contrôle sélectionnés (top k)
k <- 5
selected_controls <- hs_distances_controls %>% slice_head(n = k) %>% pull(hs_2)

controls_trends <- hs_index %>%
  filter(hs_2 %in% selected_controls, year >= 2013, year <= 2018)

# 3) Fusionner exposés et contrôles
plot_data <- bind_rows(exposed_trends_subset, controls_trends)

# 4) Plot
ggplot(plot_data, aes(x = year, y = index_value, color = hs_2, group = hs_2)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = paste0("Tendances normalisées : Exposés agrégés vs top ", k, " contrôles"),
    x = "Année",
    y = "Indice (base 2013 = 100)",
    color = "Groupe HS"
  ) +
  theme_minimal()

