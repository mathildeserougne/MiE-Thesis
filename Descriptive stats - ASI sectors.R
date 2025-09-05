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


# Fonction pour calculer la croissance annuelle d'une variable
calculate_growth <- function(data, sector) {
  sector_data <- data %>%
    filter(Description == sector) %>%
    arrange(Year) %>%
    mutate(growth = (`Total Output` - lag(`Total Output`)) / lag(`Total Output`)) %>%
    drop_na(growth)  # Supprime les lignes avec NA dans la colonne 'growth'
  return(sector_data)
}

# Fonction pour calculer l'intensité capitalistique (Fixed Capital / Workers)
calculate_capital_intensity <- function(data, sector) {
  sector_data <- data %>%
    filter(Description == sector) %>%
    mutate(Capital_Intensity = `Fixed Capital` / Workers) %>%
    drop_na(Capital_Intensity)  # Supprime les lignes avec NA dans 'Capital_Intensity'
  return(sector_data)
}

# Fonction pour calculer la part des salaires (Wages to Workers / Total Output)
calculate_wage_share <- function(data, sector) {
  sector_data <- data %>%
    filter(Description == sector) %>%
    mutate(Wage_Share = `Wages to Workers` / `Total Output`) %>%
    drop_na(Wage_Share)  # Supprime les lignes avec NA dans 'Wage_Share'
  return(sector_data)
}

# Fonction pour tracer les tendances
plot_trends <- function(data, sector, y_var, y_label, title) {
  ggplot(data, aes(x = Year, y = !!sym(y_var), group = 1)) +
    geom_line(na.rm = TRUE) +  # Ignore les NA pour geom_line
    geom_point(na.rm = TRUE) + # Ignore les NA pour geom_point
    labs(title = paste(title, "-", sector),
         x = "Year",
         y = y_label) +
    theme_minimal()
}

# Boucle sur chaque secteur exposé
for (sector in exposed_groups) {
  # 1. Croissance annuelle
  growth_data <- calculate_growth(asi_all_years, sector)
  avg_growth <- mean(growth_data$growth, na.rm = TRUE)
  print(paste("Taux de croissance moyen pour", sector, ":", round(avg_growth * 100, 2), "%"))
  
  p1 <- plot_trends(growth_data, sector, "growth", "Growth Rate", "Annual Growth Rate")
  print(p1)
  
  # 2. Intensité capitalistique
  capital_intensity_data <- calculate_capital_intensity(asi_all_years, sector)
  p2 <- plot_trends(capital_intensity_data, sector, "Capital_Intensity", "Capital Intensity", "Capital Intensity Trend")
  print(p2)
  
  # 3. Part des salaires
  wage_share_data <- calculate_wage_share(asi_all_years, sector)
  p3 <- plot_trends(wage_share_data, sector, "Wage_Share", "Wage Share", "Wage Share Trend")
  print(p3)
}











