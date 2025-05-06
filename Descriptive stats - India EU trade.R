######### DESCRIPTIVE STATISTICS ON INDIAN TRADE WITH EU PARTNERS ############
 
# First assessment made on the year 2019 (before covid and before CBAM announcements)
# Using BACI dataset, 01-2025 version

#### PACKAGES

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

#### DATA WORK

# importing the data
baci <- read_csv("~/work/BACI_HS02_Y2019_V202501.csv")
country_codes <- read_csv("~/work/country_codes_V202501.csv")

baci <- baci %>%
  rename(
    year     = t,
    exporter = i,
    importer = j,
    product  = k,
    value    = v,
    quantity = q
  )

# Merging baci with the country_codes to get country names for exporter and importer
baci <- baci %>%
  left_join(country_codes, by = c("exporter" = "country_code")) %>%
  rename(exporter_name = country_name) %>%
  left_join(country_codes, by = c("importer" = "country_code")) %>%
  rename(importer_name = country_name)

# creating a list of EU members to add a row for the EU entity
# this will allow us to aggregate the flows from India to the whole group of partners
eu_members <- c(
  276, 40, 56, 100, 196, 191, 208, 724, 233, 246,
  251, 300, 348, 372, 380, 428, 440, 442, 470, 528,
  616, 620, 203, 642, 703, 705, 752
)


## IMPORTANCE OF EUROPE AS A TRADE PARTNER, ALL SECTORS INCLUDED

# indian exports only
baci_total <- baci %>%
  filter(exporter == 699) %>%
  mutate(
    destination = ifelse(importer %in% eu_members, "EU", as.character(importer_name))
  )

# aggregating all the flows per destinations (summing the products)
total_exports_grouped <- baci_total %>%
  group_by(destination) %>%
  summarise(total_exports = sum(value, na.rm = TRUE), .groups = "drop")

# top ten destinations
top10_total <- total_exports_grouped %>%
  slice_max(order_by = total_exports, n = 10) %>%
  pull(destination)
#  "Rest of the world"
total_exports_grouped <- total_exports_grouped %>%
  mutate(destination_grouped = ifelse(destination %in% top10_total, destination, "Rest of the world")) %>%
  group_by(destination_grouped) %>%
  summarise(total_exports = sum(total_exports), .groups = "drop")

# percents (for clarity)
total_exports_grouped <- total_exports_grouped %>%
  mutate(
    pct = total_exports / sum(total_exports),
    label = paste0(round(pct * 100, 1), "%")
  )

# Camembert
ggplot(total_exports_grouped, aes(x = "", y = total_exports, fill = destination_grouped)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  labs(title = "2019 Indian exports in their main destinations (all sectors)",
       fill = "Destination") +
  theme_void()



## STEEL SECTOR AND IMPORTANCE OF EUROPE

# keeping just india, its couples, and the lines of relevant products
# product codes corresponding to the steel sector
baci_steel <- baci %>%
  filter(
    exporter == 699,
    str_sub(as.character(product), 1, 3) %in% c("720", "721", "722", "730", "731", "732")
  ) %>%
  mutate(
    destination = ifelse(importer %in% eu_members, "EU", as.character(importer_name))
  )

# grouping by destination
steel_exports_grouped <- baci_steel %>%
  group_by(destination) %>%
  summarise(total_exports = sum(value, na.rm = TRUE), .groups = "drop")

# keeping only the main destinations
top10 <- steel_exports_grouped %>%
  slice_max(order_by = total_exports, n = 10) %>%
  pull(destination)

# rest of the world category
steel_exports_grouped <- steel_exports_grouped %>%
  mutate(destination_grouped = ifelse(destination %in% top10, destination, "Rest of the world")) %>%
  group_by(destination_grouped) %>%
  summarise(total_exports = sum(total_exports), .groups = "drop")

# Calcul des pourcentages pour les labels
steel_exports_grouped <- steel_exports_grouped %>%
  mutate(
    pct = total_exports / sum(total_exports),
    label = paste0(round(pct * 100, 1), "%")
  )

# camembert plot avec pourcentages uniquement
ggplot(steel_exports_grouped, aes(x = "", y = total_exports, fill = destination_grouped)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  labs(title = "2019 Indian steel exports in their main destinations",
       fill = "Destination") +
  theme_void()



#### SAME FOR CEMENT SECTOR

# FILTRAGE CIMENT : produits exacts + ceux commençant par 681
baci_cement <- baci %>%
  filter(
    exporter == 699,
    product %in% c(251710, 382440, 382450) |
      str_starts(as.character(product), "681")
  ) %>%
  mutate(
    destination = ifelse(importer %in% eu_members, "EU", as.character(importer))
  )

# AGRÉGATION PAR DESTINATION
cement_exports_grouped <- baci_cement %>%
  group_by(destination) %>%
  summarise(total_exports = sum(value, na.rm = TRUE), .groups = "drop")

# TOP 10 DESTINATIONS
top10 <- cement_exports_grouped %>%
  slice_max(order_by = total_exports, n = 10) %>%
  pull(destination)

# Regroupement
cement_exports_grouped <- cement_exports_grouped %>%
  mutate(destination_grouped = ifelse(destination %in% top10, destination, "Rest of the world")) %>%
  group_by(destination_grouped) %>%
  summarise(total_exports = sum(total_exports), .groups = "drop")

# Conversion des codes numériques
cement_exports_grouped <- cement_exports_grouped %>%
  mutate(code_numeric = suppressWarnings(as.numeric(destination_grouped)))

# Fusion avec country_codes
cement_with_names <- cement_exports_grouped %>%
  left_join(country_codes, by = c("code_numeric" = "country_code")) %>%
  mutate(
    label = case_when(
      destination_grouped == "EU" ~ "EU",
      destination_grouped == "Rest of the world" ~ "Rest of the world",
      TRUE ~ country_name
    )
  )

# Calcul des pourcentages
cement_with_names <- cement_with_names %>%
  mutate(percentage = total_exports / sum(total_exports) * 100)

# CAMEMBERT
ggplot(cement_with_names, aes(x = "", y = total_exports, fill = label)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(
    title = "Exportations de ciment de l'Inde en 2019 (top 10 partenaires)",
    fill = "Destination"
  ) + 
  theme_void()  



  



#### same thing for aluminium

#### hydrogen

#### fertilisers  




