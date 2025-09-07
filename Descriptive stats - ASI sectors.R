#### PACKAGES

# install.packages("zoo")
# install.packages("writexl")
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
library(writexl)
library(scales)
library(zoo)

###############################################################################
################### DESCRIPTIVE STATISTICS ####################################
###############################################################################



## DATA ########################################################################


annual_series <- read_excel("~/work/MiE-Thesis/ASI_clean_data/ASI_annual_series.xlsx")
asi_all_years <- read_excel ("~/work/MiE-Thesis/ASI_clean_data/ASI_all_years10-23.xlsx")
head(asi_all_years)
unique(asi_all_years$Description)
colnames(asi_all_years)

# we need to identify the exposed and non-exposed groups
# cement, aluminium, fertilizers, inorganic chemicals, organic basic chemicals

# The groups that will be exposed: 
# BASIC METALS (for alu etc)
# OTHER NON-METALLIC MINERAL PRODUCTS (for cement)
# CHEMICALS AND CHEMICAL PRODUCTS (for fertilisers and chemicals)

# electricity generation not included in this dataset

exposed_groups <- c("BASIC METALS", "OTHER NON-METALLIC MINERAL PRODUCTS", "CHEMICALS AND CHEMICAL PRODUCTS")



### MAIN INFORMATION ABOUT EXPOSED SECTORS

# annual growth?
# capital intensity?
# wage share?


# Annual growth of a variable? (function)
calculate_growth <- function(data, sector) {
  sector_data <- data %>%
    filter(Description == sector) %>%
    arrange(Year) %>%
    mutate(growth = (`Total Output` - lag(`Total Output`)) / lag(`Total Output`)) %>%
    drop_na(growth)  
  return(sector_data)
}

# Capital intensity (function) (Fixed Capital / Workers)
calculate_capital_intensity <- function(data, sector) {
  sector_data <- data %>%
    filter(Description == sector) %>%
    mutate(Capital_Intensity = `Fixed Capital` / Workers) %>%
    drop_na(Capital_Intensity)  
  return(sector_data)
}

# Wage share (function) (Wages to Workers / Total Output)
calculate_wage_share <- function(data, sector) {
  sector_data <- data %>%
    filter(Description == sector) %>%
    mutate(Wage_Share = `Wages to Workers` / `Total Output`) %>%
    drop_na(Wage_Share)  
  return(sector_data)
}


# Fonction pour tracer les tendances (version optimisée pour les pourcentages)
plot_trends <- function(data, sector, y_var, y_label, title) {
  ggplot(data, aes(x = Year, y = !!sym(y_var), group = 1)) +
    geom_line(na.rm = TRUE, size = 0.7) +
    geom_point(na.rm = TRUE, size = 2.5) +
    labs(
      title = paste(title, "-", sector),
      x = "Year",
      y = y_label
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "gray90", linetype = "dashed"),  # Quadrillage principal
      panel.grid.minor = element_line(color = "gray95", linetype = "dotted"),  # Quadrillage secondaire
      panel.background = element_rect(fill = "white", color = "gray50")       # Fond propre
    ) +
    scale_x_continuous(
      breaks = unique(data$Year)  # Toutes les années affichées
    ) +
    scale_y_continuous(
      labels = scales::percent  # Affiche les valeurs en pourcentages (ex: 0.12 → 12%)
    )
}




# Loop on each exposed sector
for (sector in exposed_groups) {
  # 1. Annual growth
  growth_data <- calculate_growth(asi_all_years, sector)
  avg_growth <- mean(growth_data$growth, na.rm = TRUE)
  print(paste("Average growth rate for ", sector, ":", round(avg_growth * 100, 2), "%"))
  
  p1 <- plot_trends(growth_data, sector, "growth", "Growth Rate", "Annual Growth Rate")
  print(p1)
  
  # 2. Capital intensity
  capital_intensity_data <- calculate_capital_intensity(asi_all_years, sector)
  p2 <- plot_trends(capital_intensity_data, sector, "Capital_Intensity", "Capital Intensity", "Capital Intensity Trend")
  print(p2)
  
  # 3. Wage share
  wage_share_data <- calculate_wage_share(asi_all_years, sector)
  p3 <- plot_trends(wage_share_data, sector, "Wage_Share", "Wage Share", "Wage Share Trend")
  print(p3)
}


### comparing the annual growth rate with other sectors? ###

# Comparison sectors (sectors India is famous for + all india)
comparison_groups <- c("TEXTILES", "RUBBER AND PLASTICS PRODUCTS", "All India", "ELECTRICAL EQUIPMENT")

for (sector in comparison_groups) {
  # annual growth
  growth_data <- calculate_growth(asi_all_years, sector)
  avg_growth <- mean(growth_data$growth, na.rm = TRUE)
  print(paste("Average growth rate for ", sector, ":", round(avg_growth * 100, 2), "%"))
  
  p1 <- plot_trends(growth_data, sector, "growth", "Growth Rate", "Annual Growth Rate")
  print(p1)
}


### Comparison plot ############################

# data
comparison_data <- asi_all_years %>%
  filter(Description %in% c(exposed_groups, comparison_groups)) %>%
  arrange(Description, Year) %>%
  group_by(Description) %>%
  mutate(growth = (`Total Output` - lag(`Total Output`)) / lag(`Total Output`)) %>%
  drop_na(growth) %>%
  ungroup()

# comparative plot
ggplot(comparison_data, aes(x = Year, y = growth, color = Description, group = Description)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  labs(title = "Comparison of the annual growth of sectors",
       x = "Year",
       y = "Growth Rate") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()





## CAPITAL INTENSITY MEASURES 

# mesurer l'intensité capitalistique sur le début de la période (2012-2016)
# mesurer l'intensité capitalistique sur les dernières années (2018-2023)
# dessiner intensité capitalistique de all india à titre de comparaison


# calculating capital intensity on a given period
calculate_avg_capital_intensity <- function(data, sector, start_year, end_year) {
  avg_capital_intensity <- data %>%
    filter(Description == sector, Year >= start_year, Year <= end_year) %>%
    mutate(Capital_Intensity = `Fixed Capital` / Workers) %>%
    drop_na(Capital_Intensity) %>%
    summarise(Average_Capital_Intensity = mean(Capital_Intensity))
  return(avg_capital_intensity$Average_Capital_Intensity)
}

# computing it for exposed sectors
for (sector in exposed_groups) {
  avg_early <- calculate_avg_capital_intensity(asi_all_years, sector, 2012, 2016)
  avg_late <- calculate_avg_capital_intensity(asi_all_years, sector, 2018, 2023)
  print(paste("Average capital intensity for", sector, "over 2012-2016 :", round(avg_early, 2)))
  print(paste("Average capital intensity for", sector, "over 2018-2023 :", round(avg_late, 2)))
  print(paste("Variation :", round(avg_late - avg_early, 2)))
  cat("\n")
}



# calculating average capital intensity for "All India"
avg_early_all_india <- calculate_avg_capital_intensity(asi_all_years, "All India", 2012, 2016)
avg_late_all_india <- calculate_avg_capital_intensity(asi_all_years, "All India", 2018, 2023)
print(paste("Average capital intensity for All India over 2012-2016 :", round(avg_early_all_india, 2)))
print(paste("Average capital intensity for All India over 2018-2023 :", round(avg_late_all_india, 2)))
print(paste("Variation :", round(avg_late_all_india - avg_early_all_india, 2)))
cat("\n")



# PLOTS

# plotting capital intensity for all india to compare
all_india_capital_intensity_data <- calculate_capital_intensity(asi_all_years, "All India")
p_all_india_capital_intensity <- plot_trends(all_india_capital_intensity_data, "All India", "Capital_Intensity", "Capital Intensity", "Capital Intensity Trend - All India")
print(p_all_india_capital_intensity)

# comparative plots (exposed sectors vs all india)
comparison_capital_intensity_data <- asi_all_years %>%
  filter(Description %in% c(exposed_groups, "All India")) %>%
  mutate(Capital_Intensity = `Fixed Capital` / Workers) %>%
  drop_na(Capital_Intensity)

ggplot(comparison_capital_intensity_data, aes(x = Year, y = Capital_Intensity, color = Description, group = Description)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  labs(title = "Comparison of the capital intensity of expsed sectors",
       x = "Year",
       y = "Capital Intensity") +
  theme_minimal()


