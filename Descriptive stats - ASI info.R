### DESCRIPTIVE DATA BASED ON ASI ######################################

#### PACKAGES

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)

################################# data #############################################

## PRINCIPAL CHARACTERISTICS TABLES

# year 2021-2022
asi_principal_char21_22 <- read_excel("~/work/principal_characteristics_industry_group2122.xls",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean21_22 <- asi_principal_char21_22[-c(2), ]
asi_clean21_22 <- asi_clean21_22[-1, ]
# overview
head(asi_clean21_22)


## ANNUAL SERIES
asi_an_series_21_22 <- read_excel("~/work/annual_series_all_industries_21_22.xlsx", skip=1)
head(asi_an_series_21_22)
# transposing it for the analysis
transposed <- t(asi_an_series_21_22)
transposed_df <- as.data.frame(transposed) # data frame
colnames(transposed_df) <- asi_an_series_21_22$CHARACTERISTICS # names of columns
transposed_df$Year <- rownames(transposed_df) # name of years
transposed_df <- transposed_df[, c("Year", setdiff(names(transposed_df), "Year"))]
rownames(transposed_df) <- NULL
# deleting useless contents (doubles through transposition)
clean_an_series_21_22 <- transposed_df[-1, ]
clean_an_series_21_22 <- clean_an_series_21_22[, -2]






