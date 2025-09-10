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

# vulnerability indices
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



# performance indices
asi_data <- asi_data %>%
  arrange(Year) %>%  # Assurez-vous que les données sont triées par année
  group_by(`NIC-2008`) %>%  # Si vous voulez calculer par secteur (optionnel)
  mutate(
    # Croissance de la production (log-difference)
    output_growth = log(`Total Output`) - lag(log(`Total Output`), 1),
    # Croissance de l'emploi (log-difference)
    employment_growth = log(Workers) - lag(log(Workers), 1),
    # Marge bénéficiaire
    profit_margin = (`Net Value Added` - `Total Emoluments`) / `Total Output`
  ) %>%
  ungroup()  # Retirer le groupement si utilisé

colnames(asi_data)


## BUILDING THE CONTROLS 

# three groups, as heavy as the exposed sector

controls_heavy <- c("MACHINERY AND EQUIPMENT","MOTOR VEHICLES","ELECTRICAL EQUIPMENT")
controls_manufacture <- c("TEXTILES","RUBBER AND PLASTICS","FABRICATED METAL PRODUCTS")
controls_light <- c("FOOD PRODUCTS","PHARMACEUTICALS, MEDICINAL CHEMICAL AND BOTANICAL PRODUCTS","COMPUTER, ELECTRONIC AND OPTICAL PRODUCTS")



## BUILDING THE REGRESSIONS

#install.packages("plm")
#install.packages("fixest")
library(plm)
library(fixest)

# did on vulnerability ?
# we make an average of the industries counted in the controls vectors
# we add fixed effects for the year


# Function:
run_did <- function(data, treated_group, control_groups, outcome_var) {
  data <- data %>%
    mutate(
      treated = ifelse(`Description` %in% treated_group, 1, 0),
      control = ifelse(`Description` %in% control_groups, 1, 0),
      post = ifelse(Year >= 2020, 1, 0)  
    ) %>%
    filter(!is.na(treated) & !is.na(control) & treated + control == 1)  # Garde seulement traités et contrôles
  
  # Estimation DiD avec effets fixes par secteur et année
  did_model <- feols(
    reformulate(outcome_var, response = outcome_var),
    ~ treated * post | `NIC-2008` + Year,
    data = data,
    cluster = ~ `NIC-2008`
  )
  
  return(tidy(did_model))
}





