## BACI - FINDING RIGHT CONTROLS ######################################################

# Based on the EU commission communication, we have HS codes for "exposed" products.
# We already used those codes to assess the amount of exposure India has to face.

# In this script, we use the codes to compare the pre-trends (before treatment)
# and doing so, we prepare our diff-in-diff regression.

## PACKAGES

# Uncomment packages installation if necessary
install.packages(c("dplyr","readr","stringr","ggplot2", "scales"))
# Libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)

## DATA (as defined in the script 'vulnerability in trade') 
baci_indian_pov <- read_csv("~/work/baci_indian_pov.csv")

# Getting a filtered version with just the exposed products
# HS codes communicated by the EU commission
hs_prefixes <- c("25", "76", "31", "28", "29")
# Filtering the data to keep products with code starting with the mentioned prefix
baci_indian_exposed <- baci_indian_pov %>%
  filter(str_detect(as.character(product), paste(paste0("^", hs_prefixes), collapse = "|")))


# getting a filtered version with just europe
# List of EU countries
eu_members <- c(
  276, 40, 56, 100, 196, 191, 208, 724, 233, 246,
  251, 300, 348, 372, 380, 428, 440, 442, 470, 528,
  616, 620, 203, 642, 703, 705, 752
)

# Filter data to get only exports towards the EU
baci_indian_pov_eurozone <- baci_indian_pov %>%
  filter(importer %in% eu_members)


### COMPARING PRE-TRENDS ########################################

# we want to check if, prior to the policy, the commercial trends are the same.
# we can do it comparing exposed product


## LOOKING AT THE SAME PRODUCTS, REGION AS A CONTROL ###

## 1) ALL EXPOSED PRODUCTS ALTOGETHER
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

# seems pretty parallel to me but idk



















## LOOKING AT THE SAME REGION, PRODUCT AS A CONTROL

