# Chargement des bibliothèques
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(broom)
library(lmtest)
library(sandwich)

# Chargement des données
baci_indian_pov <- read_csv("~/work/baci_indian_pov.csv")

# Liste des pays de l'UE et des groupes HS exposés/contrôles
eu_members <- c(276, 40, 56, 100, 196, 191, 208, 724, 233, 246, 251, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 203, 642, 703, 705, 752)
hs_exposed <- c("25", "76", "31", "28", "29")  # Groupes exposés
hs_controls <- c("74", "75", "38", "26", "27")  # Groupes contrôles

# Filtrage des données pour les groupes HS et pays de l'UE sélectionnés
baci_filtered <- baci_indian_pov %>%
  filter(
    importer %in% eu_members,
    str_detect(as.character(product), paste0("^", paste(hs_exposed, collapse = "|"))) |
      str_detect(as.character(product), paste0("^", paste(hs_controls, collapse = "|")))
  ) %>%
  mutate(
    year = t,  # Utilisation de 't' comme année
    exposed = ifelse(
      str_detect(as.character(product), paste0("^", paste(hs_exposed, collapse = "|"))),
      1, 0
    ),  # 1 = exposé, 0 = contrôle
    importer = as.factor(importer),
    product = as.factor(product),
    hs_group = substr(as.character(product), 1, 2)  # Extraction des 2 premiers chiffres du code HS
  ) %>%
  arrange(product, importer, year) %>%
  group_by(product, importer) %>%
  mutate(
    lag_export_value = lag(value, 1),
    log_lag_value = log(lag_export_value + 1)
  ) %>%
  ungroup()

# Agrégation par famille HS (2 chiffres)
baci_agg <- baci_filtered %>%
  group_by(hs_group, year, importer) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    quantity = sum(quantity, na.rm = TRUE),  # Somme des quantités pour les contrôles
    .groups = "drop"
  ) %>%
  arrange(hs_group, importer, year) %>%
  group_by(hs_group, importer) %>%
  mutate(
    lag_export_value = lag(value, 1),
    log_lag_value = log(lag_export_value + 1)
  ) %>%
  ungroup() %>%
  mutate(
    exposed = ifelse(hs_group %in% hs_exposed, 1, 0),
    post = ifelse(year >= 2021, 1, 0),  # Traitement à partir de 2021
    log_quantity = log(quantity + 1)  # Contrôle pour la quantité
  )

# Vérification des parallel trends (période pré-2021)
pre_period <- baci_agg %>%
  filter(year < 2021)

model_pre <- lm(
  log(value + 1) ~ exposed * year + log_lag_value + log_quantity + factor(year) + factor(importer),
  data = pre_period
)

# Résumé du test des parallel trends
summary(model_pre)



# Estimation de l'effet DiD (période complète)
did_model <- lm(
  log(value + 1) ~ exposed * post + log_lag_value + log_quantity + factor(year) + factor(importer),
  data = baci_agg
)

# Résumé du modèle DiD
summary(did_model)

# Résumé avec erreurs standards clusterisées
coeftest(did_model, vcov = vcovCL, cluster = ~ hs_group)
