### DESCRIPTIVE DATA BASED ON ASI ######################################

#### PACKAGES

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)

# data
asi_principal_char <- read_excel("~/work/principal_characterstics_industry_group.xls",  skip = 4)
# Supprimer la ligne 2 et 3 du DataFrame (correspondent aux lignes 6 et 7 du fichier brut)
asi_clean <- asi_principal_char[-c(2), ]

# Aperçu
head(asi_clean)




