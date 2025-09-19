# EVENT STUDY ON ALUMINIUM

# donc sur les produits hs76

# =============================================
# Event Study complet à partir de baci_indian_pov
# =============================================
library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)
library(kableExtra)  


# 1. Préparation des données ============================
event_study_data <- baci_indian_pov %>%
  # Filtrer pays UE et produits pertinents
  filter(
    importer %in% c(276, 40, 56, 100, 196, 191, 208, 724, 233, 246, 251, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 203, 642, 703, 705, 752),
    str_detect(as.character(product), "^76|^74|^75")  # HS76 (Alu) + contrôles
  ) %>%
  mutate(
    year = t,
    hs2 = substr(as.character(product), 1, 2),
    exposed = ifelse(hs2 == "76", 1, 0),
    hs_group = factor(hs2, levels = c("74", "75", "76"), labels = c("Cuivre", "Nickel", "Aluminium"))
  ) %>%
  group_by(hs2, product, importer, year, exposed, hs_group) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(product, importer, year) %>%
  group_by(product, importer) %>%
  mutate(lag_value = lag(value, 1)) %>%
  ungroup() %>%
  mutate(
    log_value = log(value + 1),
    log_lag_value = log(lag_value + 1),
    event_year = year - 2020,
    post_2023 = ifelse(year >= 2021, 1, 0)
  ) %>%
  group_by(hs2, year, exposed, hs_group) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    quantity = sum(quantity, na.rm = TRUE),
    log_value = log(value + 1),
    log_quantity = log(quantity + 1),
    .groups = "drop"
  )

# 2. Vérification des parallel trends ===================
cat("\n=== Vérification des tendances parallèles (2018-2020) ===\n")

# Tableau des statistiques descriptives
pre_trends_table <- event_study_data %>%
  filter(year <= 2020) %>%
  group_by(hs_group, year) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    n_obs = n()
  ) %>%
  pivot_wider(names_from = year, values_from = c(mean_value, sd_value, n_obs))

print(pre_trends_table)

# Graphique des tendances
pre_trends_plot <- ggplot(filter(event_study_data, year <= 2020),
                          aes(x = year, y = value, color = hs_group)) +
  geom_line(linewidth = 1) +
  labs(title = "Tendances pré-traitement (2018-2020)",
       y = "Valeur des exportations",
       x = "Année",
       color = "Groupe") +
  theme_minimal()
print(pre_trends_plot)

# Tests statistiques
pre_trend_model <- lm(log_value ~ exposed * year,
                      data = filter(event_study_data, year <= 2020))
pre_trend_test <- tidy(pre_trend_model) %>%
  filter(term == "exposed:year")

cat("\nTest d'interaction exposé × année (2018-2020):\n")
print(pre_trend_test)

colnames(event_study_data)
######### jusqu'ici ça fonctionne 





# 1. Préparation des données (avec vos colonnes)
event_data <- event_study_data %>%
  mutate(
    event_time = year - 2021,  # 0 = 2021
    rel_log_value = log_value - mean(log_value[year <= 2020], na.rm = TRUE)  # Détendance 2018-2020
  ) %>%
  filter(!is.infinite(rel_log_value) & year >= 2018 & year <= 2023)  # Fenêtre temporelle propre

# 2. Box plots par groupe HS2 (Aluminium vs Contrôles)
event_boxplot <- ggplot(event_data, aes(x = factor(event_time), y = rel_log_value, fill = hs_group)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  facet_wrap(~ hs_group, ncol = 1) +
  labs(
    title = "Impact relatif de l'événement (2021) par groupe HS2",
    x = "Temps relatif à 2021 (0 = année du traitement)",
    y = "Log(Valeur) - Moyenne pré-traitement (2018-2020)",
    fill = "Groupe"
  ) +
  theme_minimal() +
  scale_x_discrete(limits = as.character(-2:2))  # Force l'affichage de -2 à +2

print(event_boxplot)

# 3. Régression DiD dynamique (version simplifiée sans felm)
# Modèle avec interactions et effets fixes par produit/pays
did_model <- lm(
  rel_log_value ~ exposed * factor(event_time) + factor(hs2) + factor(year),
  data = filter(event_data, event_time >= -2 & event_time <= 2)
)

# Extraction des effets dynamiques
did_coefs <- tidy(did_model) %>%
  filter(str_detect(term, "exposed:factor\\(event_time\\)")) %>%
  mutate(
    event_period = as.numeric(gsub("exposed:factor\\(event_time\\)", "", term)),
    term = factor(term, levels = rev(term))
  )

# Visualisation des coefficients
coef_plot <- ggplot(did_coefs, aes(x = event_period, y = estimate,
                                   ymin = estimate - 1.96 * std.error,
                                   ymax = estimate + 1.96 * std.error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Effets dynamiques sur l'Aluminium (vs Contrôles)",
    x = "Années relatives à 2021",
    y = "Effet estimé [IC 95%]"
  ) +
  theme_minimal()

print(coef_plot)

# 4. Test de pré-trends (avant 2021)
pre_trend_test <- lm(
  rel_log_value ~ exposed * factor(event_time),
  data = filter(event_data, event_time < 0)
) %>%
  summary()
cat("\n=== Test de pré-trends (p-value pour interaction) ===\n")
print(pre_trend_test$coefficients["exposed:factor(event_time)1", ])
