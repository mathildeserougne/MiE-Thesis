#### PACKAGES

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
install.packages("writexl")
library(writexl)
library(scales)
install.packages("zoo")  
library(zoo)

###############################################################################
################### DESCRIPTIVE STATISTICS ####################################
###############################################################################

######### ALL INDUSTRIES ?

#calling the series once that processed
#(no need to reprocess everything since the clean series are on the repo)

clean_an_series_22_23 <- read_excel("~/work/MiE-Thesis/ASI_clean_data/ASI_annual_series.xlsx")

colnames(clean_an_series_22_23)

##  MEASURES OF INVESTMENT AND CAPITAL FORMATION

# GFCF represents the annual level of investment.
# NFCF reflects the actual growth of productive capital (GFCF - K depreciation)

# re-formatting the dates (from 1989-90 to 1989)
clean_an_series_22_23 <- clean_an_series_22_23 %>%
  mutate(Year_clean = as.numeric(str_sub(Year, 1, 4)))  

# long format reshape
df_long <- clean_an_series_22_23 %>%
  select(Year_clean,
         Gross = `26.GROSS FIXED CAPITAL  FORMATION`,
         Net = `25.NET FIXED CAPITAL  FORMATION`) %>%
  pivot_longer(cols = c(Gross, Net),
               names_to = "Type",
               values_to = "Capital_Formation")

# deleting potential separators or random spaces before numeric values
df_long <- df_long %>%
  mutate(Capital_Formation = parse_number(Capital_Formation))

# ggplot v1 - NOT USED BECAUSE DEFAULTSSSSSS
ggplot(df_long, aes(x = Year_clean, y = Capital_Formation, color = Type, group = Type)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(
    title = "Gross and net capital formation in the Indian industry (all sectors, 1989–2022)",
    x = "Year",
    y = "Capital formation (Rs. Lakh)",
    color = "Type"
  ) +
  scale_x_continuous(breaks = seq(min(df_long$Year_clean), max(df_long$Year_clean), by = 2)) +
  theme_minimal()


# CLEAN ggplot with separators
ggplot(df_long, aes(x = Year_clean, y = Capital_Formation, color = Type, group = Type)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(
    title = "Gross and net capital formation in the Indian industry (all sectors, 1989–2022)",
    x = "Year",
    y = "Capital formation (Rs. Lakh)",
    color = "Type"
  ) +
  scale_y_continuous(labels = label_comma()) +  # séparateurs de milliers (ex: 1,000,000)
  scale_x_continuous(breaks = seq(min(df_long$Year_clean), max(df_long$Year_clean), by = 2)) +
  theme_minimal()


# Investissement net/brut: NET FIXED CAPITAL FORMATION/GFCF 



## GFCF/FIXED CAPITAL gives an idea of the accumulation / renewal of fixed capital

# ATTENTION first version - not used because unclear
# creating the variable = ratio GFCF / Fixed Capital
df_invest_ratio <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    GFCF = parse_number(`26.GROSS FIXED CAPITAL  FORMATION`),
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Investment_Effort = GFCF / Fixed_Capital
  ) %>%
  select(Year_clean, Investment_Effort)
# plotting it over time 
ggplot(df_invest_ratio, aes(x = Year_clean, y = Investment_Effort)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkgray") +
  labs(
    title = "Investment Effort in Indian Industry (GFCF / Fixed Capital, 1989–2022)",
    x = "Year",
    y = "Investment Effort Ratio",
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(df_invest_ratio$Year_clean), max(df_invest_ratio$Year_clean), by = 2)) +
  theme_minimal()

# CLEANER VERSION
# creating the ratio for a period since 2000 (more representative for our question)
df_invest_ratio <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    GFCF = parse_number(`26.GROSS FIXED CAPITAL  FORMATION`),
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Investment_Effort = GFCF / Fixed_Capital
  ) %>%
  filter(Year_clean >= 2000) %>%
  select(Year_clean, Investment_Effort)
# plot + trend using loess
ggplot(df_invest_ratio, aes(x = Year_clean, y = Investment_Effort)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2") +
  geom_smooth(method = "loess", span = 0.5, se = FALSE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Investment Effort in Indian Industry (GFCF / Fixed Capital, 2000–2022)",
    x = "Year",
    y = "Investment Effort Ratio",
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, max(df_invest_ratio$Year_clean), by = 2)) +
  theme_minimal()

# VERSION WITH MOVING AVERAGE !
df_invest_ratio <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    GFCF = parse_number(`26.GROSS FIXED CAPITAL  FORMATION`),
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Investment_Effort = GFCF / Fixed_Capital
  ) %>%
  filter(Year_clean >= 2000) %>%
  select(Year_clean, Investment_Effort) %>%
  arrange(Year_clean) %>%
  mutate(MA_3yr = zoo::rollmean(Investment_Effort, k = 3, fill = NA, align = "center"))
# graph with MA(3)
ggplot(df_invest_ratio, aes(x = Year_clean)) +
  geom_line(aes(y = Investment_Effort), color = "#0072B2", linewidth = 1.2) +
  geom_point(aes(y = Investment_Effort), color = "#0072B2") +
  geom_line(aes(y = MA_3yr), color = "darkred", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Investment Effort in Indian Industry (GFCF / Fixed Capital, 2000–2022)",
    subtitle = "With 3-year centered moving average (dashed line)",
    x = "Year",
    y = "Investment Effort Ratio"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, max(df_invest_ratio$Year_clean), by = 2)) +
  theme_minimal()


# interpretation: 
# the higher the ratio, the more dynamic the economy is.
# a weak value corresponds to a stagnant dynamic, where the capital stock can get used without being (sufficiantly?) replaced.


###  EFFICIENCY, PRODUCTIVITY ?
# trying to get more insights to get a right interpretation of the lower investment effort since the 2010s

# CAPITAL PRODUCTIVITY - ratio added value / fixed capital
df_capital_productivity <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    Net_Value_Added = parse_number(`21.NET VALUE ADDED`),
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Capital_Productivity = Net_Value_Added / Fixed_Capital
  ) %>%
  filter(Year_clean >= 2000) %>%
  select(Year_clean, Capital_Productivity) %>%
  arrange(Year_clean) %>%
  mutate(MA_3yr = zoo::rollmean(Capital_Productivity, k = 3, fill = NA, align = "center"))
# plot with MA(3)
ggplot(df_capital_productivity, aes(x = Year_clean)) +
  geom_line(aes(y = Capital_Productivity), color = "#009E73", linewidth = 1.2) +
  geom_point(aes(y = Capital_Productivity), color = "#009E73") +
  geom_line(aes(y = MA_3yr), color = "darkorange", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Capital Productivity in Indian Industry (Net Value Added / Fixed Capital, 2000–2022)",
    subtitle = "With 3-year centered moving average (dashed line)",
    x = "Year",
    y = "Capital Productivity Ratio"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, max(df_capital_productivity$Year_clean), by = 2)) +
  theme_minimal()

# interpretation?



# LABOR PRODUCTIVITY - net added value / mandays workers






## COMPARING ACCUMULATION OF CAPITAL AND PRODUCTIVITY



## MEASURES ON ADDED VALUE IN INDIAN INDUSTRY OVERALL?

# Taux de valeur ajoutée: NET VALUE ADDED/VALUE OF OUTPUT
# Taux de dépréciation: DEPRECIATION/FIXED CAPITAL (20/2)
# net value added? déflater par cpi (il faut ajouter la série sur une sous-base sur 2013-2023)







## intensité capitalistique ? comparaison investissement valeur ajoutée






## un truc sur les employés n
## added value and profit

######### THEN, SOMETHING PER SECTOR 