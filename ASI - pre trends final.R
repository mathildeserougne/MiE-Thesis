# pre trends de zéro


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




## ADDING USEFUL VARIABLES

# Calcul des indicateurs de vulnérabilité et de performance
asi_all_years <- asi_all_years %>%
  group_by(`NIC-2008`, Description) %>%
  arrange(Year) %>%
  mutate(
    # Indicateurs de vulnérabilité
    wage_share = (`Wages to Workers` / `Net Value Added`),
    capital_intensity = (`Fixed Capital` / Workers),
    productivity = (`Net Value Added` / Workers),
    investment_rate = ifelse(`Fixed Capital` != 0,
                             (`Invested Capital` / `Fixed Capital`), NA),
    
    # Indicateurs de performance (avec lag pour les taux de croissance)
    output_growth = log(`Total Output`) - lag(log(`Total Output`)),
    employment_growth = log(Workers) - lag(log(Workers)),
    profit_margin = (`Net Value Added` - `Total Emoluments`) / `Total Output`
  ) %>%
  ungroup()

# Affichage des premières lignes pour vérifier
head(asi_all_years)




## INVESTMENT RATE #######################
# on n'obtient rien de concluant.

## LOOKING FOR CONTROLS, TESTING TRENDS
## DOING THE DID


# date de choc?
pre_period <- asi_combined %>%
  filter(Year < 2021)

pre_period_year <- 2021

# Fonction pour calculer les corrélations avec un secteur exposé, en alignant les années
get_correlations <- function(exposed_sector, data, pre_period_year) {
  # Filtrer les données avant le choc
  pre_data <- data %>%
    filter(Year < pre_period_year) %>%
    select(Year, Description, investment_rate) %>%
    drop_na(investment_rate)
  
  # Extraire les données du secteur exposé
  exposed_data <- pre_data %>%
    filter(Description == exposed_sector) %>%
    arrange(Year)
  
  # Liste des secteurs potentiels (tous sauf le secteur exposé)
  control_sectors <- unique(pre_data$Description)
  control_sectors <- control_sectors[control_sectors != exposed_sector]
  
  # Calculer la corrélation avec chaque secteur de contrôle
  correlations <- lapply(control_sectors, function(control_sector) {
    control_data <- pre_data %>%
      filter(Description == control_sector) %>%
      arrange(Year)
    
    # Fusionner les données par année pour s'assurer de l'alignement
    merged_data <- merge(exposed_data, control_data, by = "Year", suffixes = c("_exposed", "_control"))
    
    # Calculer la corrélation si assez de données
    if (nrow(merged_data) > 1) {
      cor(merged_data$investment_rate_exposed, merged_data$investment_rate_control, use = "complete.obs")
    } else {
      NA
    }
  })
  
  # Créer un data frame avec les résultats
  result <- data.frame(
    Secteur = control_sectors,
    Correlation = unlist(correlations)
  ) %>%
    arrange(desc(Correlation)) %>%
    na.omit() %>%
    head(5)  # Top 5 secteurs les plus corrélés
  
  return(result)
}

# Appliquer pour chaque secteur exposé
exposed_groups <- c("BASIC METALS", "OTHER NON-METALLIC MINERAL PRODUCTS", "CHEMICALS AND CHEMICAL PRODUCTS")

for (sector in exposed_groups) {
  cat("\nTop 5 secteurs corrélés avec", sector, "avant", pre_period_year, ":\n")
  print(get_correlations(sector, asi_all_years, pre_period_year))
}


# listes formées

# Liste des paires secteur exposé / secteurs de contrôle
pairs_list <- list(
  # BASIC METALS
  list(exposed = "BASIC METALS",
       controls = c("OTHER TRANSPORT EQUIPMENT",
                    "FOOD PRODUCTS",
                    "REPAIR AND INSTALLATION OF MACHINERY AND EQUIPMENT")),
  
  # OTHER NON-METALLIC MINERAL PRODUCTS
  list(exposed = "OTHER NON-METALLIC MINERAL PRODUCTS",
       controls = c("CROP & ANIMAL PRODUCTION, HUNTING & RELATED SERVICE ACTIVITIES",
                    "WEARING APPAREL",
                    "COTTON GINNING, CLEANING AND BAILING (01632); SEED PROCESSING FOR PROPAGATION (01640)",
                    "PUBLISHING ACTIVITIES",
                    "ELECTRICAL EQUIPMENT")),
  
  # CHEMICALS AND CHEMICAL PRODUCTS
  list(exposed = "CHEMICALS AND CHEMICAL PRODUCTS",
       controls = c("COKE AND REFINED PETROLEUM PRODUCTS",
                    "REPAIR AND INSTALLATION OF MACHINERY AND EQUIPMENT",
                    "FOOD PRODUCTS",
                    "FABRICATED METAL PRODUCTS, EXCEPT MACHINERY AND EQUIPMENT"))
)



# test de différence de tendances

# Fonction pour tester l'égalité des tendances (pentes) avant 2021
test_parallel_trends <- function(exposed_sector, control_sector, data, pre_year) {
  pre_data <- data %>%
    filter(Description %in% c(exposed_sector, control_sector) & Year < pre_year) %>%
    drop_na(investment_rate) %>%
    mutate(Description = factor(Description, levels = c(exposed_sector, control_sector)))
  
  # Régression avec interaction année × secteur
  model <- lm(investment_rate ~ Year * Description, data = pre_data)
  
  # Extraire la p-value de l'interaction (pente différente ?)
  tidy_model <- tidy(model, conf.int = TRUE)
  p_value <- tidy_model %>%
    filter(str_detect(term, "Year:Description")) %>%
    pull(p.value) %>%
    first()
  
  return(p_value)
}

# Appliquer le test à toutes les paires
results <- map(pairs_list, ~ {
  exposed <- .x$exposed
  controls <- .x$controls
  
  map(controls, ~ {
    p_value <- test_parallel_trends(exposed, .x, asi_all_years, 2021)
    data.frame(
      Exposed = exposed,
      Control = .x,
      P_value = p_value,
      Parallel_Trends = ifelse(p_value > 0.05, "Oui", "Non")
    )
  }) %>%
    bind_rows()
}) %>%
  bind_rows()

# Afficher les résultats
print(results)




## final selection of controls saved: 


# Secteurs exposés
exposed_basic_metals <- "BASIC METALS"
exposed_minerals <- "OTHER NON-METALLIC MINERAL PRODUCTS"
exposed_chemicals <- "CHEMICALS AND CHEMICAL PRODUCTS"

# Vecteurs des secteurs de contrôle validés
controls_basic_metals <- c("OTHER TRANSPORT EQUIPMENT")

controls_minerals <- c(
  "WEARING APPAREL",
  #"COTTON GINNING, CLEANING AND BAILING (01632); SEED PROCESSING FOR PROPAGATION (01640)",
  "PUBLISHING ACTIVITIES",
  "ELECTRICAL EQUIPMENT"
)

controls_chemicals <- c(
  "REPAIR AND INSTALLATION OF MACHINERY AND EQUIPMENT",
  "FOOD PRODUCTS",
  "FABRICATED METAL PRODUCTS, EXCEPT MACHINERY AND EQUIPMENT"
)

# Liste des vecteurs pour un accès facile
control_groups <- list(
  BasicMetals = list(exposed = exposed_basic_metals, controls = controls_basic_metals),
  Minerals = list(exposed = exposed_minerals, controls = controls_minerals),
  Chemicals = list(exposed = exposed_chemicals, controls = controls_chemicals)
)



## did for investment rate! ############

## for chemicals ##

# Préparation des données pour la DiD
did_data_chemicals <- asi_all_years %>%
  filter(Description %in% c(exposed_chemicals, controls_chemicals)) %>%
  mutate(
    Exposed = ifelse(Description == exposed_chemicals, 1, 0),
    Post_2021 = ifelse(Year >= 2021, 1, 0),
    Sector = factor(Description, levels = c(exposed_chemicals, controls_chemicals))
  )

# Estimation du modèle DiD
model_chemicals <- lm(
  investment_rate ~ Exposed * Post_2021 + factor(Year) + factor(Description),
  data = did_data_chemicals
)

# Résultats du modèle
tidied_results <- tidy(model_chemicals, conf.int = TRUE)
did_effect <- tidied_results %>%
  filter(term == "Exposed:Post_2021") %>%
  select(term, estimate, p.value)

print(did_effect)



# visualisation

# Année de référence pour la normalisation (ex: 2015)
ref_year <- 2012

# Calcul des moyennes par groupe et par année, puis normalisation
trends_chemicals <- did_data_chemicals %>%
  group_by(Description, Year) %>%
  summarise(mean_investment_rate = mean(investment_rate, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  # Normalisation : (valeur / valeur en 2015) * 100
  group_by(Description) %>%
  mutate(
    ref_value = mean_investment_rate[Year == ref_year],
    normalized_rate = (mean_investment_rate / ref_value) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(normalized_rate))  # Supprimer les NA si 2015 n'existe pas pour un secteur

# Définition des couleurs
sector_colors <- c(
  "CHEMICALS AND CHEMICAL PRODUCTS" = "red",
  "REPAIR AND INSTALLATION OF MACHINERY AND EQUIPMENT" = "blue",
  "FOOD PRODUCTS" = "green",
  "FABRICATED METAL PRODUCTS, EXCEPT MACHINERY AND EQUIPMENT" = "orange"
)

# Tracé des tendances normalisées
ggplot(trends_chemicals, aes(x = Year, y = normalized_rate, color = Description)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Évolution normalisée de l'investment_rate (2015 = 100) : CHEMICALS AND CHEMICAL PRODUCTS vs. Contrôles",
    x = "Année",
    y = "Taux d'investissement (2015 = 100)",
    color = "Secteur"
  ) +
  scale_color_manual(values = sector_colors) +
  theme_minimal() +
  theme(legend.position = "bottom")


## bref pas concluant pour celui-là



## MINERALS

# Préparation des données pour la DiD
did_data_minerals <- asi_all_years %>%
  filter(Description %in% c(exposed_minerals, controls_minerals)) %>%
  mutate(
    Exposed = ifelse(Description == exposed_minerals, 1, 0),
    Post_2021 = ifelse(Year >= 2021, 1, 0),
    Sector = factor(Description, levels = c(exposed_minerals, controls_minerals))
  )

# Estimation du modèle DiD
model_minerals <- lm(
  investment_rate ~ Exposed * Post_2021 + factor(Year) + factor(Description),
  data = did_data_minerals
)

# Résultats du modèle
tidied_results_minerals <- tidy(model_minerals, conf.int = TRUE)
did_effect_minerals <- tidied_results_minerals %>%
  filter(term == "Exposed:Post_2021") %>%
  select(term, estimate, p.value)

# Affichage des résultats
print(did_effect_minerals)

# Année de référence pour la normalisation
ref_year <- 2012

# Calcul des moyennes par groupe et par année, puis normalisation
trends_minerals <- did_data_minerals %>%
  group_by(Description, Year) %>%
  summarise(mean_investment_rate = mean(investment_rate, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  group_by(Description) %>%
  mutate(
    ref_value = mean_investment_rate[Year == ref_year],
    normalized_rate = (mean_investment_rate / ref_value) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(normalized_rate))  # Supprimer les NA si 2012 n'existe pas pour un secteur

# Définition des couleurs pour chaque secteur
sector_colors_minerals <- c(
  "OTHER NON-METALLIC MINERAL PRODUCTS" = "red",
  "WEARING APPAREL" = "blue",
  # "COTTON GINNING, CLEANING AND BAILING (01632); SEED PROCESSING FOR PROPAGATION (01640)" = "green",
  "PUBLISHING ACTIVITIES" = "purple",
  "ELECTRICAL EQUIPMENT" = "orange"
)

# Tracé des tendances normalisées
ggplot(trends_minerals, aes(x = Year, y = normalized_rate, color = Description)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Évolution normalisée de l'investment_rate (2012 = 100) : OTHER NON-METALLIC MINERAL PRODUCTS vs. Contrôles",
    x = "Année",
    y = "Taux d'investissement (2012 = 100)",
    color = "Secteur"
  ) +
  scale_color_manual(values = sector_colors_minerals) +
  theme_minimal() +
  theme(legend.position = "bottom")

# bah non



## metals...
# ouais non




#####################

# profit margins


# selecting controls per correlation
get_correlations_profit_margin <- function(exposed_sector, data, pre_period_year) {
  pre_data <- data %>%
    filter(Year < pre_period_year) %>%
    select(Year, Description, profit_margin) %>%
    drop_na(profit_margin)
  
  exposed_data <- pre_data %>%
    filter(Description == exposed_sector) %>%
    arrange(Year)
  
  control_sectors <- unique(pre_data$Description)
  control_sectors <- control_sectors[control_sectors != exposed_sector]
  
  correlations <- lapply(control_sectors, function(control_sector) {
    control_data <- pre_data %>%
      filter(Description == control_sector) %>%
      arrange(Year)
    
    merged_data <- merge(exposed_data, control_data, by = "Year", suffixes = c("_exposed", "_control"))
    
    if (nrow(merged_data) > 1) {
      cor(merged_data$profit_margin_exposed, merged_data$profit_margin_control, use = "complete.obs")
    } else {
      NA
    }
  })
  
  result <- data.frame(
    Secteur = control_sectors,
    Correlation = unlist(correlations)
  ) %>%
    arrange(desc(Correlation)) %>%
    na.omit() %>%
    head(5)
  
  return(result)
}


exposed_groups <- c("BASIC METALS", "OTHER NON-METALLIC MINERAL PRODUCTS", "CHEMICALS AND CHEMICAL PRODUCTS")
pre_period_year <- 2021

for (sector in exposed_groups) {
  cat("\nTop 5 secteurs corrélés avec", sector, "avant", pre_period_year, "pour profit_margin:\n")
  print(get_correlations_profit_margin(sector, asi_all_years, pre_period_year))
}


# test de tendances parallèles

# Fonction pour tester l'égalité des tendances (pentes) avant 2021
test_parallel_trends_profit_margin <- function(exposed_sector, control_sector, data, pre_year) {
  pre_data <- data %>%
    filter(Description %in% c(exposed_sector, control_sector) & Year < pre_year) %>%
    drop_na(profit_margin)
  
  # Vérifier qu'il y a assez de données pour les deux secteurs
  if (n_distinct(pre_data$Description) < 2 || n_distinct(pre_data$Year) < 2) {
    return(NA)
  }
  
  pre_data <- pre_data %>%
    mutate(Description = factor(Description, levels = c(exposed_sector, control_sector)))
  
  model <- try(lm(profit_margin ~ Year * Description, data = pre_data), silent = TRUE)
  
  if (inherits(model, "try-error")) {
    return(NA)
  }
  
  tidy_model <- tidy(model, conf.int = TRUE)
  p_value <- tidy_model %>%
    filter(str_detect(term, "Year:Description")) %>%
    pull(p.value) %>%
    first()
  
  return(p_value)
}

# Liste des paires secteur exposé/secteurs de contrôle (top 5 corrélés)
pairs_list_profit_margin <- list(
  list(exposed = "BASIC METALS",
       controls = c(
         "OTHER NON-METALLIC MINERAL PRODUCTS",
         "PHARMACEUTICALS, MEDICINAL CHEMICAL AND BOTANICAL PRODUCTS",
         "FOOD PRODUCTS",
         "MACHINERY AND EQUIPMET N.E.C.",
         "COTTON GINNING, CLEANING AND BAILING (01632); SEED PROCESSING FOR PROPAGATION (01640")
  ),
  list(exposed = "OTHER NON-METALLIC MINERAL PRODUCTS",
       controls = c(
         "FOOD PRODUCTS",
         "BASIC METALS",
         "MACHINERY AND EQUIPMET N.E.C.",
         "BEVERAGES",
         "FABRICATED METAL PRODUCTS, EXCEPT MACHINERY AND EQUIPMENT")
  ),
  list(exposed = "CHEMICALS AND CHEMICAL PRODUCTS",
       controls = c(
         "FOOD PRODUCTS",
         "LEATHER AND RELATED PRODUCTS",
         "OTHER MINING AND QUARRYING",
         "PHARMACEUTICALS, MEDICINAL CHEMICAL AND BOTANICAL PRODUCTS",
         "All India")
  )
)

# Appliquer le test à toutes les paires
results_profit_margin <- map(pairs_list_profit_margin, ~ {
  exposed <- .x$exposed
  controls <- .x$controls
  
  map(controls, ~ {
    p_value <- test_parallel_trends_profit_margin(exposed, .x, asi_all_years, 2021)
    data.frame(
      Exposed = exposed,
      Control = .x,
      P_value = p_value,
      Parallel_Trends = ifelse(!is.na(p_value) & p_value > 0.05, "Oui", "Non")
    )
  }) %>%
    bind_rows()
}) %>%
  bind_rows()

# Afficher les résultats
print(results_profit_margin)


# keeping the final selection: 

# Secteurs exposés
exposed_basic_metals <- "BASIC METALS"
exposed_minerals <- "OTHER NON-METALLIC MINERAL PRODUCTS"
exposed_chemicals <- "CHEMICALS AND CHEMICAL PRODUCTS"

# Contrôles validés pour BASIC METALS (p > 0.05)
controls_basic_metals_profit <- c(
  "PHARMACEUTICALS, MEDICINAL CHEMICAL AND BOTANICAL PRODUCTS",
  "FOOD PRODUCTS"
)

# Contrôles validés pour OTHER NON-METALLIC MINERAL PRODUCTS (p > 0.05)
controls_minerals_profit <- c(
  "FOOD PRODUCTS",
  "MACHINERY AND EQUIPMET N.E.C.",
  "BEVERAGES",
  "FABRICATED METAL PRODUCTS, EXCEPT MACHINERY AND EQUIPMENT"
)

# Contrôles validés pour CHEMICALS AND CHEMICAL PRODUCTS (p > 0.05)
controls_chemicals_profit <- c(
  "FOOD PRODUCTS",
  "LEATHER AND RELATED PRODUCTS",
  "PHARMACEUTICALS, MEDICINAL CHEMICAL AND BOTANICAL PRODUCTS"
)


# Liste des groupes pour un accès facile
control_groups_profit <- list(
  BasicMetals = list(exposed = exposed_basic_metals, controls = controls_basic_metals_profit),
  Minerals = list(exposed = exposed_minerals, controls = controls_minerals_profit),
  Chemicals = list(exposed = exposed_chemicals, controls = controls_chemicals_profit)
)


## did

estimate_did_profit_margin <- function(exposed_sector, control_sectors, data) {
  # Préparation des données
  did_data <- data %>%
    filter(Description %in% c(exposed_sector, control_sectors)) %>%
    drop_na(profit_margin) %>%
    mutate(
      Exposed = ifelse(Description == exposed_sector, 1, 0),
      Post_2021 = ifelse(Year >= 2021, 1, 0),
      Description = factor(Description, levels = c(exposed_sector, control_sectors))  # Définir l'ordre des niveaux
    )
  
  # Estimation du modèle DiD
  model <- lm(
    profit_margin ~ Exposed * Post_2021 + factor(Year) + factor(Description),
    data = did_data
  )
  
  # Résultats du modèle
  tidied_results <- tidy(model, conf.int = TRUE)
  did_effect <- tidied_results %>%
    filter(term == "Exposed:Post_2021") %>%
    select(term, estimate, p.value)
  
  # Affichage des résultats
  cat("\n--- Résultats DiD pour", exposed_sector, "---\n")
  print(did_effect)
  
  # Calcul des moyennes par groupe et par année
  trends_data <- did_data %>%
    group_by(Description, Year) %>%
    summarise(mean_profit_margin = mean(profit_margin), .groups = "drop") %>%
    ungroup()
  
  # Normalisation par rapport à la première année disponible
  first_year <- min(trends_data$Year)
  trends_data <- trends_data %>%
    group_by(Description) %>%
    mutate(
      ref_value = mean_profit_margin[Year == first_year],
      normalized_profit_margin = (mean_profit_margin / ref_value) * 100
    ) %>%
    ungroup() %>%
    filter(Year >= first_year)
  
  # Définition des couleurs (le secteur exposé est toujours en premier)
  sector_colors <- c(exposed_sector = "red")
  control_colors <- c("blue", "green", "purple")
  names(control_colors) <- control_sectors
  sector_colors <- c(sector_colors, control_colors)
  
  # Tracé des tendances normalisées
  p <- ggplot(trends_data, aes(x = Year, y = normalized_profit_margin, color = Description, group = Description)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2021, linetype = "dashed", color = "black", size = 1) +
    labs(
      title = paste("Évolution normalisée de profit_margin (", first_year, "= 100):", exposed_sector),
      x = "Année",
      y = "Marge bénéficiaire normalisée",
      color = "Secteur"
    ) +
    scale_color_manual(values = sector_colors) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(model = model, results = did_effect, plot = p, data = trends_data))
}

# Exécution pour BASIC METALS
results_basic_metals <- estimate_did_profit_margin(
  exposed_sector = control_groups_profit$BasicMetals$exposed,
  control_sectors = control_groups_profit$BasicMetals$controls,
  data = asi_all_years
)

# Affichage du graphique
print(results_basic_metals$plot)








# BASIC METALS
results_basic_metals <- estimate_did_profit_margin(
  exposed_sector = control_groups_profit$BasicMetals$exposed,
  control_sectors = control_groups_profit$BasicMetals$controls,
  data = asi_all_years
)
if (!is.null(results_basic_metals$plot)) print(results_basic_metals$plot)

# YES



# OTHER NON-METALLIC MINERAL PRODUCTS
results_minerals <- estimate_did_profit_margin(
  exposed_sector = control_groups_profit$Minerals$exposed,
  control_sectors = control_groups_profit$Minerals$controls,
  data = asi_all_years
)
if (!is.null(results_minerals$plot)) print(results_minerals$plot)

# CHEMICALS AND CHEMICAL PRODUCTS
results_chemicals <- estimate_did_profit_margin(
  exposed_sector = control_groups_profit$Chemicals$exposed,
  control_sectors = control_groups_profit$Chemicals$controls,
  data = asi_all_years
)
if (!is.null(results_chemicals$plot)) print(results_chemicals$plot)


