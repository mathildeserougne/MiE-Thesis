## DID AND CONTROL RESEARCH PER HS EXPOSED TYPE ##############################


library(dplyr)
library(ggplot2)

# hs_exposed_char : vecteur des codes HS exposés
# hs_index : dataframe avec colonnes hs_2, year, index_value

# Fonction pour calculer le MSE entre la trajectoire d'un groupe exposé et un autre groupe
calculate_mse <- function(df1, df2) {
  merged <- merge(df1, df2, by = "year", all = FALSE)
  mse <- mean((merged$index_value.x - merged$index_value.y)^2, na.rm = TRUE)
  return(mse)
}

# Pour chaque groupe exposé, chercher ses 5 meilleurs contrôles
best_controls_per_exposed <- list()

for (exposed_group in hs_exposed_char) {
  # 1) Extraire la trajectoire de ce groupe exposé (2013-2020)
  exposed_traj <- hs_index %>%
    filter(hs_2 == exposed_group, year >= 2013, year <= 2020) %>%
    select(year, index_value)
  
  # 2) Extraire les groupes non exposés
  non_exposed_groups <- hs_index %>%
    filter(!hs_2 %in% hs_exposed_char, year >= 2013, year <= 2020) %>%
    group_by(hs_2) %>%
    group_split()
  
  # 3) Calculer le MSE de chaque non exposé avec ce groupe exposé
  mse_list <- sapply(non_exposed_groups, function(df) {
    calculate_mse(exposed_traj, df %>% select(year, index_value))
  })
  
  # 4) Récupérer les noms des groupes non exposés
  non_exposed_names <- sapply(non_exposed_groups, function(df) unique(df$hs_2))
  
  # 5) Assembler dans un data.frame
  mse_df <- data.frame(
    hs_2 = non_exposed_names,
    mse = mse_list
  )
  
  # 6) Trier par mse et garder les 5 meilleurs
  best_5 <- mse_df %>%
    arrange(mse) %>%
    slice_head(n = 5)
  
  # 7) Stocker dans la liste
  best_controls_per_exposed[[exposed_group]] <- best_5
}

# Pour afficher le résultat pour un groupe exposé donné, exemple :
print(best_controls_per_exposed[[hs_exposed_char[1]]])

# concaténer tout dans un seul data.frame avec le nom du groupe exposé associé
best_controls_all <- bind_rows(
  lapply(names(best_controls_per_exposed), function(exposed) {
    df <- best_controls_per_exposed[[exposed]]
    df$exposed_hs_2 <- exposed
    return(df)
  })
)

print(best_controls_all)



# Transformer en liste de vecteurs de contrôles par exposé
selected_controls_list <- best_controls_all %>%
  group_by(exposed_hs_2) %>%
  summarise(controls = list(hs_2)) %>%
  # optionnel : transformer en liste nommée
  { setNames(.$controls, .$exposed_hs_2) }

# Afficher la liste des contrôles pour un exposé, par ex le premier
print(selected_controls_list[[1]])

# Afficher la liste complète (exposés en noms, vecteurs en valeurs)
print(selected_controls_list)


# Pour chaque groupe exposé, on crée un vecteur nommé selected_control_<exposed>
for(exposed in unique(best_controls_all$exposed_hs_2)) {
  # Extraire les 5 contrôles pour ce groupe exposé
  controls_vec <- best_controls_all %>%
    filter(exposed_hs_2 == exposed) %>%
    slice_head(n = 5) %>%
    pull(hs_2)
  
  # Créer un nom de variable comme "selected_control_25", etc.
  var_name <- paste0("selected_control_", exposed)
  
  # Assigner le vecteur dans l'environnement global
  assign(var_name, controls_vec, envir = .GlobalEnv)
}





## LOOP FOR THE DID FOR EACH GROUP


library(dplyr)
library(fixest)

# Liste des HS exposés à analyser
hs_codes_to_test <- unique(best_controls_all$exposed_hs_2)

for(hs_code in hs_codes_to_test) {
  
  cat("=== Résultats pour HS code:", hs_code, "===\n\n")
  
  # Récupérer dynamiquement le vecteur des contrôles associés
  controls_varname <- paste0("selected_control_", hs_code)
  selected_controls <- get(controls_varname, envir = .GlobalEnv)
  
  # Préparation des données : HS traité + contrôles
  hs_index_did_simple <- hs_index %>%
    filter(hs_2 %in% c(hs_code, selected_controls)) %>%
    mutate(
      treated = if_else(hs_2 == hs_code, 1, 0),
      post = if_else(year >= 2021, 1, 0),
      treated_post = treated * post,
      year_factor = as.factor(year)
    )
  
  # Estimation DiD simple avec effets fixes produit + année
  did_model_simple <- feols(
    index_value ~ treated_post | hs_2 + year_factor,
    data = hs_index_did_simple,
    cluster = ~hs_2
  )
  
  print(summary(did_model_simple))
  
  # Préparation pour modèle dynamique
  hs_index_did_dynamic <- hs_index %>%
    filter(hs_2 %in% c(hs_code, selected_controls)) %>%
    mutate(
      treated = if_else(hs_2 == hs_code, 1, 0),
      year_factor = as.factor(year)
    )
  
  # Estimation dynamique : interaction année x traitement, référence 2020
  did_model_dynamic <- feols(
    index_value ~ i(year_factor, treated, ref = "2020") | hs_2 + year_factor,
    data = hs_index_did_dynamic,
    cluster = ~hs_2
  )
  
  print(summary(did_model_dynamic))
  
  # Graphique dynamique (affiché pour chaque HS code)
  iplot(did_model_dynamic,
        main = paste("Effet dynamique du traitement HS", hs_code, "par année (référence 2020)"),
        xlab = "Année",
        ylab = "Effet estimé (DiD)",
        ci_level = 0.95)
  
  cat("\n\n")
}
