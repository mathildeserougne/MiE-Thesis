## July version of the DID on BACI data ###

# No need to aggregate before doing the did regression
# fixed effect by year, at hs level

# Libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(broom)
library(tidyr)


# BACI data (flows from india) + exposed hs codes + eu countries
baci_indian_pov <- read_csv("~/MiE 24-25/THESIS/baci_indian_pov.csv")
hs_cbam <- c("25", "76", "31", "28", "29")
eu_members <- c(
  276, 40, 56, 100, 196, 191, 208, 724, 233, 246,
  251, 300, 348, 372, 380, 428, 440, 442, 470, 528,
  616, 620, 203, 642, 703, 705, 752
)


colnames(baci_indian_pov)

cbam_year <- 2021



# Regression without aggregating exposed products before the did



baci <- baci_indian_pov %>% 
  mutate(hs2     = str_pad(product, 6, side = "left", pad = "0") |> str_sub(1, 2),
         exposed = hs2 %in% hs_cbam,
         eu      = importer %in% eu_members,
         treated = exposed & eu,
         post    = year >= cbam_year,
         log_val = log(value + 1))







