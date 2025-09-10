## DID STRATEGY ASI

## LIBRARIES 

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
library(writexl)
library(scales)
library(zoo)


## DATA

annual_series <- read_excel("~/work/MiE-Thesis/ASI_clean_data/ASI_annual_series.xlsx")
asi_all_years <- read_excel ("~/work/MiE-Thesis/ASI_clean_data/ASI_all_years10-23.xlsx")

exposed_groups <- c("BASIC METALS", "OTHER NON-METALLIC MINERAL PRODUCTS", "CHEMICALS AND CHEMICAL PRODUCTS")


## ADDING VARIABLES
## ADDING VARIABLES
asi_data <- asi_all_years %>%
  mutate(
    wage_share = (`Wages to Workers` / `Net Value Added`),
    capital_intensity = (`Fixed Capital` / Workers),
    productivity = (`Net Value Added` / Workers),
    # Calcul du taux d'investissement (variation annuelle)
    invested_capital_lag = lag(`Invested Capital`, 1),
    fixed_capital_lag = lag(`Fixed Capital`, 1),
    investment_rate = ifelse(
      !is.na(fixed_capital_lag) & fixed_capital_lag != 0,
      ((`Invested Capital` - invested_capital_lag) / fixed_capital_lag),
      NA
    )
  ) %>%
  select(-invested_capital_lag, -fixed_capital_lag)  # Supprimer les colonnes temporaires

# Performance indices
asi_data <- asi_data %>%
  arrange(Year) %>%
  group_by(`NIC-2008`) %>%
  mutate(
    # Croissance de la production (log-difference)
    output_growth = log(`Total Output`) - lag(log(`Total Output`), 1),
    # Croissance de l'emploi (log-difference)
    employment_growth = log(Workers) - lag(log(Workers), 1),
    # Marge bénéficiaire
    profit_margin = (`Net Value Added` - `Total Emoluments`) / `Total Output`
  ) %>%
  ungroup()

## DEFINITION DES GROUPES DE CONTRÔLE
controls_heavy <- c(
  "MACHINERY AND EQUIPMENT",
  "MOTOR VEHICLES",
  "ELECTRICAL EQUIPMENT"
)

controls_manufacture <- c(
  "TEXTILES",
  "RUBBER AND PLASTICS",
  "FABRICATED METAL PRODUCTS"
)

controls_light <- c(
  "FOOD PRODUCTS",
  "PHARMACEUTICALS, MEDICINAL CHEMICAL AND BOTANICAL PRODUCTS",
  "COMPUTER, ELECTRONIC AND OPTICAL PRODUCTS"
)


