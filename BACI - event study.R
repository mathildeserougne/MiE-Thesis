# event study

# Préparation des données pour l'Event Study
baci_agg <- baci_agg %>%
  mutate(event_year = year - 2020)  # 2020 = année de référence (avant traitement)

# Estimation du modèle Event Study
event_model <- lm(
  log(value + 1) ~
    # Interaction entre "exposed" et chaque année relative à 2020
    exposed * factor(event_year) +
    log_lag_value + log_quantity +
    factor(year) + factor(importer),
  data = baci_agg
)

# Extraction et nettoyage des coefficients
event_coefs <- tidy(event_model) %>%
  filter(str_detect(term, "exposed:factor\\(event_year\\)")) %>%
  mutate(
    event_year = as.numeric(gsub("exposed:factor\\(event_year\\)(-?[0-9]+)", "\\1", term)),
    term = gsub("exposed:factor\\(event_year\\)(-?[0-9]+)", "exposed:year\\1", term)
  )

# Visualisation des résultats
ggplot(event_coefs, aes(x = event_year, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Event Study : Effet du CBAM sur les exportations indiennes vers l'UE",
    subtitle = "Coefficients de l'interaction exposed × année relative à 2020",
    x = "Années depuis 2020 (0 = 2020, 1 = 2021, etc.)",
    y = "Effet estimé (log(value + 1))",
    caption = "Intervalles de confiance à 95%"
  ) +
  scale_x_continuous(breaks = -3:3) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor.x = element_blank()
  )

# Tableau des coefficients
event_coefs %>%
  select(term, estimate, std.error, p.value) %>%
  arrange(event_year) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  )

# Version avec erreurs standards clusterisées
event_model_cluster <- lm(
  log(value + 1) ~ exposed * factor(event_year) + log_lag_value + log_quantity + factor(year) + factor(importer),
  data = baci_agg
)

event_coefs_cluster <- coeftest(event_model_cluster, vcov = vcovCL, cluster = ~ hs_group) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(str_detect(term, "exposed:factor\\(event_year\\)")) %>%
  mutate(
    event_year = as.numeric(gsub("exposed:factor\\(event_year\\)(-?[0-9]+)", "\\1", term)),
    term = gsub("exposed:factor\\(event_year\\)(-?[0-9]+)", "exposed:year\\1", term)
  )

# Visualisation avec erreurs standards clusterisées
ggplot(event_coefs_cluster, aes(x = event_year, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Event Study avec erreurs standards clusterisées (par famille HS)",
    subtitle = "Effet du CBAM sur les exportations indiennes vers l'UE",
    x = "Années depuis 2020 (0 = 2020, 1 = 2021, etc.)",
    y = "Effet estimé (log(value + 1))",
    caption = "Intervalles de confiance à 95% (clusterisés par famille HS)"
  ) +
  scale_x_continuous(breaks = -3:3) +
  theme_minimal()






baci_agg_post2021 <- baci_agg %>%
  filter(year >= 2022) %>%
  mutate(post = 1)

did_model_post2021 <- lm(
  log(value + 1) ~ exposed * post + log_lag_value + log_quantity +
    factor(year) + factor(importer),
  data = baci_agg_post2021
)
summary(did_model_post2021)
