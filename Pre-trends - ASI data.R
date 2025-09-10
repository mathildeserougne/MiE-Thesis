## PRE TRENDS AND CONTROL GROUPS FOR ASI DATA###################################
################################################################################

## PACKAGES ####################################################################

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
library(tidyr)
library(readxl)


## DATA ########################################################################


annual_series <- read_excel("~/work/MiE-Thesis/ASI_clean_data/ASI_annual_series.xlsx")
asi_all_years <- read_excel ("~/work/MiE-Thesis/ASI_clean_data/ASI_all_years10-23.xlsx")
head(asi_all_years)


# we need to identify the exposed and non-exposed groups
# cement, aluminium, fertilizers, inorganic chemicals, organic basic chemicals

# here?
valeurs_uniques <- unique(asi_all_years$Description)
print(valeurs_uniques)

# The groups that will be exposed: 
# BASIC METALS (for alu etc)
# OTHER NON-METALLIC MINERAL PRODUCTS (for cement)
# CHEMICALS AND CHEMICAL PRODUCTS (for fertilisers and chemicals)

exposed_groups <- c("BASIC METALS", "OTHER NON-METALLIC MINERAL PRODUCTS", "CHEMICALS AND CHEMICAL PRODUCTS")


########### PRE TRENDS TO COMPARE PARALLELISM #################################

####################### ignorer output c'est nul ###############################

# 1) output (finalement c'est nul comme idée)

# 1.1. output of exposed groups

asi_filtered <- asi_all_years %>%
  filter(Description %in% exposed_groups)
# plot
ggplot(asi_filtered, aes(x = Year, y = `Total Output`, color = Description, group = Description)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Evolution of output by industrial group",
       x = "Année",
       y = "Total Output",
       color = "Groupe Industriel") +
  scale_color_manual(values = c("BASIC METALS" = "purple",
                                "OTHER NON-METALLIC MINERAL PRODUCTS" = "violet",
                                "CHEMICALS AND CHEMICAL PRODUCTS" = "pink")) +
  scale_x_continuous(breaks = seq(min(asi_filtered$Year), max(asi_filtered$Year), by = 1)) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))



# 1.2. output of different groups.

# Filtrer les données pour les groupes industriels d'intérêt
selected_groups <- c("CHEMICALS AND CHEMICAL PRODUCTS", "RUBBER AND PLASTICS PRODUCTS",
                     "OTHER MINING AND QUARRYING", "PAPER AND PAPER PRODUCTS",
                     "WEARING APPAREL", "ELECTRICAL EQUIPMENT", "BASIC METALS","OTHER NON-METALLIC MINERAL PRODUCTS")

asi_filtered <- asi_all_years %>%
  filter(Description %in% selected_groups)

# Créer le graphique
ggplot(asi_filtered, aes(x = Year, y = `Total Output`, color = Description, group = Description)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Évolution de l'Output pour les Groupes Industriels Sélectionnés",
       x = "Année",
       y = "Total Output",
       color = "Groupe Industriel") +
  scale_x_continuous(breaks = seq(min(asi_filtered$Year), max(asi_filtered$Year), by = 1)) +
  scale_color_manual(values = c(
    "CHEMICALS AND CHEMICAL PRODUCTS" = "blue",
    "RUBBER AND PLASTICS PRODUCTS" = "orange",
    "OTHER MINING AND QUARRYING" = "green",
    "PAPER AND PAPER PRODUCTS" = "purple",
    "WEARING APPAREL" = "pink",
    "ELECTRICAL EQUIPMENT" = "brown",
    "BASIC METALS" = "red",
    "OTHER NON-METALLIC MINERAL PRODUCTS" = "yellow"
  )) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  )


# rubber and electrical do not seem to be too bad??

# test


control_groups <- c("RUBBER AND PLASTICS PRODUCTS", "ELECTRICAL EQUIPMENT")

# Ajouter une variable "group" : Exposed ou Control
asi_grouped <- asi_all_years %>%
  filter(Description %in% c(exposed_groups, control_groups)) %>%
  mutate(group = case_when(
    Description %in% exposed_groups ~ "Exposed",
    Description %in% control_groups ~ "Control"
  ))

# Agréger par groupe et année
asi_summary <- asi_grouped %>%
  group_by(group, Year) %>%
  summarise(total_output = sum(`Total Output`, na.rm = TRUE), .groups = "drop")

# Centrer les données (optionnel mais utile pour le graphe)
asi_summary <- asi_summary %>%
  group_by(group) %>%
  mutate(index_output = total_output / first(total_output) * 100) %>%
  ungroup()

# Ajouter la variable binaire "treated"
asi_summary <- asi_summary %>%
  mutate(treated = ifelse(group == "Exposed", 1, 0))

# Conserver uniquement les années pré-traitement (ex. < 2020)
asi_pre2020 <- asi_summary %>% filter(Year < 2020)

# Régression pour test de tendance parallèle : interaction treated * Year
model_pretrend <- lm(index_output ~ Year * treated, data = asi_pre2020)

# Résumé du modèle
summary(model_pretrend)



## parallel trends okay!
## representing the parallel pre-trends

# 2. Créer une variable "treated"
asi_all_years <- asi_all_years %>%
  mutate(treated = case_when(
    Description %in% exposed_groups ~ "Exposed",
    Description %in% control_groups ~ "Control",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(treated))  # enlever les autres secteurs

# 3. Agréger le total output par groupe (treated) et année
asi_aggregated <- asi_all_years %>%
  group_by(Year, treated) %>%
  summarise(Total_Output = sum(`Total Output`, na.rm = TRUE), .groups = "drop")

# 4. Graphe
ggplot(asi_aggregated, aes(x = Year, y = Total_Output, color = treated, group = treated)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Évolution du Total Output : Exposés vs Contrôle",
       x = "Année",
       y = "Total Output Agrégé",
       color = "Groupe") +
  theme_minimal() +
  scale_color_manual(values = c("Exposed" = "red", "Control" = "blue")) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))



##################################################################################
##################################################################################


## ADDING VARIABLES

asi_data <- asi_all_years %>%
  arrange(`NIC-2008`, Year) %>%  # Tri par secteur puis par année
  group_by(`NIC-2008`) %>%     # Regrouper par secteur
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
      NA_real_
    ),
    # Croissance de la production (log-difference)
    output_growth = ifelse(
      !is.na(lag(`Total Output`, 1)) & lag(`Total Output`, 1) > 0 & `Total Output` > 0,
      log(`Total Output`) - log(lag(`Total Output`, 1)),
      NA_real_
    ),
    # Croissance de l'emploi (log-difference)
    employment_growth = ifelse(
      !is.na(lag(Workers, 1)) & lag(Workers, 1) > 0 & Workers > 0,
      log(Workers) - log(lag(Workers, 1)),
      NA_real_
    ),
    # Marge bénéficiaire
    profit_margin = ifelse(
      `Total Output` != 0,
      (`Net Value Added` - `Total Emoluments`) / `Total Output`,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-invested_capital_lag, -fixed_capital_lag)  # Supprimer les colonnes temporaires










# PARALLEL TRENDS DE INVESTMENT RATE ##########################################
