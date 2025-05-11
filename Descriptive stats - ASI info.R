########################################################################
### DESCRIPTIVE DATA BASED ON ASI ######################################
########################################################################

#### PACKAGES

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
install.packages("writexl")
library(writexl)

####################################################################################
################################# data #############################################

############################################################################
## PRINCIPAL CHARACTERISTICS TABLES = INFORMATION, DECOMPOSED ACROSS SECTORS

# all years prior to 2010 are in PDF format. not adapted.

# year 2010-2011
asi_principal_char10_11 <- read_excel("~/work/pcp_c_ind_group_10_11.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean10_11 <- asi_principal_char10_11[-c(2), ]
asi_clean10_11 <- asi_clean10_11[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean10_11$Year <- 2011
# overview
head(asi_clean10_11)

# year 2011-2012
asi_principal_char11_12 <- read_excel("~/work/pcp_c_ind_group_1112.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean11_12 <- asi_principal_char11_12[-c(2), ]
asi_clean11_12 <- asi_clean11_12[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean11_12$Year <- 2012
# overview
head(asi_clean11_12)

# year 2012-2013
asi_principal_char12_13 <- read_excel("~/work/pcp_c_ind_group_1213.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean12_13 <- asi_principal_char12_13[-c(2), ]
asi_clean12_13 <- asi_clean12_13[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean12_13$Year <- 2013
# overview
head(asi_clean12_13)

# for 2013-2014, not formatted (only in PDF) so we ignore that year

# year 2014-2015
asi_principal_char14_15 <- read_excel("~/work/pcp_c_ind_group_1415.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean14_15 <- asi_principal_char14_15[-c(2), ]
asi_clean14_15 <- asi_clean14_15[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean14_15$Year <- 2015
# overview
head(asi_clean14_15)

# year 2015-2016
asi_principal_char15_16 <- read_excel("~/work/pcp_c_ind_group_1516.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean15_16 <- asi_principal_char15_16[-c(2), ]
asi_clean15_16 <- asi_clean15_16[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean15_16$Year <- 2016
# overview
head(asi_clean15_16)

# year 2016-2017
asi_principal_char16_17 <- read_excel("~/work/pcp_c_ind_group_1617.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean16_17 <- asi_principal_char16_17[-c(2), ]
asi_clean16_17 <- asi_clean16_17[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean16_17$Year <- 2017
# overview
head(asi_clean16_17)

# year 2017-2018
asi_principal_char17_18 <- read_excel("~/work/pcp_c_ind_group_1718.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean17_18 <- asi_principal_char17_18[-c(2), ]
asi_clean17_18 <- asi_clean17_18[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean17_18$Year <- 2018
# overview
head(asi_clean17_18)

# year 2018-2019
asi_principal_char18_19 <- read_excel("~/work/pcp_c_ind_group_1819.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean18_19 <- asi_principal_char18_19[-c(2), ]
asi_clean18_19 <- asi_clean18_19[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean18_19$Year <- 2019
# changing the name of some variables (pbm of * bc of updates that year)
asi_clean18_19 <- asi_clean18_19 %>%
  rename(`Total Input` = `Total Input*`,
         `Total Output` = `Total Output *`,
         `Net Value Added` = `Net Value Added*`,
         `Rent Paid` = `Rent Paid*`)
# overview
head(asi_clean18_19)

# year 2019-2020
asi_principal_char19_20 <- read_excel("~/work/pcpal_c_ind_group_1920.xlsx",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean19_20 <- asi_principal_char19_20[-c(2), ]
asi_clean19_20 <- asi_clean19_20[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean19_20$Year <- 2020
# overview
head(asi_clean19_20)

# year 2020-2021
asi_principal_char20_21 <- read_excel("~/work/principal_characteristics_industry_group2021.xls",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean20_21 <- asi_principal_char20_21[-c(2), ]
asi_clean20_21 <- asi_clean20_21[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean20_21$Year <- 2021
# overview
head(asi_clean20_21)

# year 2021-2022
asi_principal_char21_22 <- read_excel("~/work/principal_characteristics_industry_group2122.xls",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean21_22 <- asi_principal_char21_22[-c(2), ]
asi_clean21_22 <- asi_clean21_22[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean21_22$Year <- 2022
# overview
head(asi_clean21_22)

# year 2022-2023
asi_principal_char22_23 <- read_excel("~/work/principal_characteristics_industry_group2223.xls",  skip = 4)
# deleting some rows corresponding to the layout of the xls doc
asi_clean22_23 <- asi_principal_char22_23[-c(2), ]
asi_clean22_23 <- asi_clean22_23[-1, ]
# adding a "year" variable for the upcoming merge
asi_clean22_23$Year <- 2023
# overview
head(asi_clean22_23)

## dowloading the clean data
write_xlsx(asi_clean10_11, path = "~/work/ASI_clean_1011.xlsx")
write_xlsx(asi_clean11_12, path = "~/work/ASI_clean_1112.xlsx")
write_xlsx(asi_clean12_13, path = "~/work/ASI_clean_1213.xlsx")
#nothing for 1314
write_xlsx(asi_clean14_15, path = "~/work/ASI_clean_1415.xlsx")
write_xlsx(asi_clean15_16, path = "~/work/ASI_clean_1516.xlsx")
write_xlsx(asi_clean16_17, path = "~/work/ASI_clean_1617.xlsx")
write_xlsx(asi_clean17_18, path = "~/work/ASI_clean_1718.xlsx")
write_xlsx(asi_clean18_19, path = "~/work/ASI_clean_1819.xlsx")
write_xlsx(asi_clean19_20, path = "~/work/ASI_clean_1920.xlsx")
write_xlsx(asi_clean20_21, path = "~/work/ASI_clean_2021.xlsx")
write_xlsx(asi_clean21_22, path = "~/work/ASI_clean_2122.xlsx")
write_xlsx(asi_clean22_23, path = "~/work/ASI_clean_2223.xlsx")


#### MERGING the tables to get a set over 2014-2023

asi_all_years <- bind_rows(
  asi_clean14_15,
  asi_clean15_16,
  asi_clean16_17,
  asi_clean17_18,
  asi_clean18_19,
  asi_clean19_20,
  asi_clean20_21,
  asi_clean21_22,
  asi_clean22_23
)
asi_all_years$Description[is.na(asi_all_years$Description)] <- "All India"

# taking a look at the product
head(asi_all_years)
table(asi_all_years$Year)         
unique(asi_all_years$Description) 

# downloading it
write_xlsx(asi_all_years, path = "~/work/ASI_all_years14-23.xlsx")


###  doing the same thing but with the gap in 2013-2014
asi_all_years_ext <- bind_rows(
  asi_clean10_11,
  asi_clean11_12,
  asi_clean12_13,
  asi_clean14_15,
  asi_clean15_16,
  asi_clean16_17,
  asi_clean17_18,
  asi_clean18_19,
  asi_clean19_20,
  asi_clean20_21,
  asi_clean21_22,
  asi_clean22_23
)
asi_all_years_ext$Description[is.na(asi_all_years_ext$Description)] <- "All India"
# taking a look at the product
head(asi_all_years_ext)
table(asi_all_years_ext$Year)         
unique(asi_all_years_ext$Description) 
# downloading it
write_xlsx(asi_all_years_ext, path = "~/work/ASI_all_years10-23.xlsx")


###############################################################################
## ANNUAL SERIES ##############################################################

# very aggregated. not decomposed by sectors.

# older version (useless, in fact)
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


# annual series for all industries aggregated (1989-2023)
asi_an_series_22_23 <- read_excel("~/work/annual_series_all_industries_22_23.xlsx", skip=1)
head(asi_an_series_22_23)
# transposing it for the analysis
transposed2 <- t(asi_an_series_22_23)
transposed2_df <- as.data.frame(transposed2) # data frame
colnames(transposed2_df) <- asi_an_series_22_23$CHARACTERISTICS # names of columns
transposed2_df$Year <- rownames(transposed2_df) # name of years
transposed2_df <- transposed2_df[, c("Year", setdiff(names(transposed2_df), "Year"))]
rownames(transposed2_df) <- NULL
# deleting useless contents (doubles through transposition)
clean_an_series_22_23 <- transposed2_df[-1, ]
clean_an_series_22_23 <- clean_an_series_22_23[, -2]


# downloading it
write_xlsx(clean_an_series_22_23, path = "~/work/ASI_annual_series.xlsx")


###############################################################################
################### DESCRIPTIVE STATISTICS ####################################
###############################################################################

######### ALL INDUSTRIES ?

# faire un truc avec l'évolution du capital, de l'investissement
# un truc sur les employés 
# capital formation
# added value and profit


######### THEN, SOMETHING PER SECTOR 






