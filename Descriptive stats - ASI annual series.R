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

######### ALL INDUSTRIES ? #####################################################

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


## NET/GROSS INVESTMENT
#NET FIXED CAPITAL FORMATION/GFCF 

# ratio nfcf/gfcf + the moving average
df_ratio_net_gross <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    GFCF = parse_number(`26.GROSS FIXED CAPITAL  FORMATION`),
    NFCF = parse_number(`25.NET FIXED CAPITAL  FORMATION`),
    Ratio_Net_Gross = NFCF / GFCF
  ) %>%
  filter(!is.na(Ratio_Net_Gross) & Year_clean >= 2000) %>%
  arrange(Year_clean) %>%
  mutate(MA_3yr = zoo::rollmean(Ratio_Net_Gross, k = 3, fill = NA, align = "center"))
# plot of the ratio + moving average
ggplot(df_ratio_net_gross, aes(x = Year_clean)) +
  geom_line(aes(y = Ratio_Net_Gross), color = "#0072B2", linewidth = 1.2) +
  geom_point(aes(y = Ratio_Net_Gross), color = "#0072B2") +
  geom_line(aes(y = MA_3yr), color = "#D55E00", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Ratio of Net to Gross Capital Formation in Indian Industry (2000–2022)",
    subtitle = "Share of gross investment that results in net capital accumulation",
    x = "Year",
    y = "NFCF / GFCF (%)"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, max(df_ratio_net_gross$Year_clean), by = 2)) +
  theme_minimal()

# interpretation?
# close to 100% = few capital is replaced, most of I effectively increases productive K
# lower ratio = important share of I to compensate depreciation


## DEPRECIATION RATE: DEPRECIATION / FIXED CAPITAL

# creating the ratio
df_depreciation_rate <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    Depreciation = parse_number(`20.DEPRECIATION`),
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Depreciation_Rate = Depreciation / Fixed_Capital
  ) %>%
  filter(!is.na(Depreciation_Rate) & Year_clean >= 2000) %>%
  arrange(Year_clean) %>%
  mutate(MA_3yr = zoo::rollmean(Depreciation_Rate, k = 3, fill = NA, align = "center"))
# plot
ggplot(df_depreciation_rate, aes(x = Year_clean)) +
  geom_line(aes(y = Depreciation_Rate), color = "#009E73", linewidth = 1.2) +
  geom_point(aes(y = Depreciation_Rate), color = "#009E73") +
  geom_line(aes(y = MA_3yr), color = "darkred", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Depreciation Rate in Indian Industry (Depreciation / Fixed Capital, 2000–2022)",
    subtitle = "With 3-year centered moving average (dashed line)",
    x = "Year",
    y = "Depreciation Rate"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, max(df_depreciation_rate$Year_clean), by = 2)) +
  theme_minimal()


## COMPARISON OF DEPRECIATION AND INVESTMENT
# comparing depreciation and investment (both plots on the same graph)

# date - ratios - filter 2000 and after
df_combined_ratios <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    GFCF = parse_number(`26.GROSS FIXED CAPITAL  FORMATION`),
    NFCF = parse_number(`25.NET FIXED CAPITAL  FORMATION`),
    Depreciation = parse_number(`20.DEPRECIATION`),
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Ratio_NFCF_GFCF = NFCF / GFCF,
    Ratio_Dep_FC = Depreciation / Fixed_Capital
  ) %>%
  filter(!is.na(Ratio_NFCF_GFCF) & !is.na(Ratio_Dep_FC) & Year_clean >= 2000) %>%
  select(Year_clean, Ratio_NFCF_GFCF, Ratio_Dep_FC) %>%
  pivot_longer(cols = c(Ratio_NFCF_GFCF, Ratio_Dep_FC),
               names_to = "Ratio_Type",
               values_to = "Value") %>%
  mutate(
    Ratio_Type = recode(Ratio_Type,
                        "Ratio_NFCF_GFCF" = "Net / Gross Capital Formation",
                        "Ratio_Dep_FC" = "Depreciation / Fixed Capital")
  )
# plot
ggplot(df_combined_ratios, aes(x = Year_clean, y = Value, color = Ratio_Type, group = Ratio_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(
    title = "Capital Dynamics in Indian Industry (2000–2022)",
    subtitle = "Net/Gross Capital Formation vs Depreciation/Fixed Capital",
    x = "Year",
    y = "Ratio",
    color = "Ratio Type"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, max(df_combined_ratios$Year_clean), by = 2)) +
  theme_minimal()

# interpretation
# net/gross CF shows the proportion of CF investment actually contributing to expansion of productive capacities
# depreciation ratio : rate of obsolescence of existing capital
# very stable depreciation rate (6-8%), more volatility for net/gross CF
# early 2000s, restarting after the asian crisis
# 2004-2012, robust expansion, indian economic boom
# then, slow because post global crisis
# covid crisis followed by strong recovery (highest since the crisis mid 2010s)
# very resilient economy. when good cycle, fast expansion (rather than pure renewal). despite modernisation, stable depreciation. 
# when low net/gross ration, signals that indian industry mainly invested to protect its capacities rather than expand



## GFCF/FIXED CAPITAL
#gives an idea of the accumulation / renewal of fixed capital

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
## trying to get more insights to get a right interpretation of the lower investment effort since the 2010s

## CAPITAL PRODUCTIVITY

#ratio added value / fixed capital
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
  geom_line(aes(y = MA_3yr), color = "#66D3A4", linetype = "dashed", linewidth = 1.2) +
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
# use this graph to question India's ability to decarbonate its industry while the country struggles to stabilize its own capital productivity


# capital productivity in index 2000
df_capital_productivity_indexed <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    Net_Value_Added = parse_number(`21.NET VALUE ADDED`),
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Capital_Productivity = Net_Value_Added / Fixed_Capital
  ) %>%
  filter(Year_clean >= 2000) %>%
  arrange(Year_clean) %>%
  mutate(
    Capital_Productivity_Index = 100 * Capital_Productivity / Capital_Productivity[Year_clean == 2000],
    MA_3yr = zoo::rollmean(Capital_Productivity_Index, k = 3, fill = NA, align = "center")
  ) %>%
  select(Year_clean, Capital_Productivity_Index, MA_3yr)

# Plot
ggplot(df_capital_productivity_indexed, aes(x = Year_clean)) +
  geom_line(aes(y = Capital_Productivity_Index), color = "#009E73", linewidth = 1.2) +
  geom_point(aes(y = Capital_Productivity_Index), color = "#009E73") +
  geom_line(aes(y = MA_3yr), color = "#66D3A4", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Indexed Capital Productivity in Indian Industry (2000–2022)",
    subtitle = "Net Value Added / Fixed Capital (2000 = 100), with 3-year centered moving average (dashed line)",
    x = "Year",
    y = "Capital Productivity Index (2000 = 100)"
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()



## LABOR PRODUCTIVITY
# net added value / mandays workers

# ratio of labor productivity
df_labour_productivity <- clean_an_series_22_23 %>%
  mutate(Year_clean = as.numeric(str_sub(Year, 1, 4))) %>%
  filter(Year_clean >= 2000) %>%
  mutate(
    Net_Value_Added = parse_number(`21.NET VALUE ADDED`),
    Mandays_Workers = parse_number(`7. MANDAYS-WORKERS`),
    Labour_Productivity = Net_Value_Added / Mandays_Workers
  ) %>%
  select(Year_clean, Labour_Productivity) %>%
  arrange(Year_clean) %>%
  mutate(MA_3yr = zoo::rollmean(Labour_Productivity, k = 3, fill = NA, align = "center"))

# plot
ggplot(df_labour_productivity, aes(x = Year_clean)) +
  geom_line(aes(y = Labour_Productivity), color = "#D55E00", linewidth = 1.2) +
  geom_point(aes(y = Labour_Productivity), color = "#D55E00") +
  geom_line(aes(y = MA_3yr), color = "#F4A582", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Labour Productivity in Indian Industry (Net Value Added / Mandays, 2000–2022)",
    subtitle = "With 3-year centered moving average (dashed line)",
    x = "Year",
    y = "Labour Productivity (Rs. per Manday)"
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2), limits = c(2000, 2022)) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()


## labour productivity mais en index

df_labour_productivity_pct <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    Net_Value_Added = parse_number(`21.NET VALUE ADDED`),
    Mandays_Workers = parse_number(`7. MANDAYS-WORKERS`),
    Labour_Productivity = Net_Value_Added / Mandays_Workers
  ) %>%
  filter(Year_clean >= 2000) %>%
  arrange(Year_clean) %>%
  mutate(
    Labour_Productivity_Index = 100 * Labour_Productivity / Labour_Productivity[Year_clean == 2000],
    MA_3yr = zoo::rollmean(Labour_Productivity_Index, k = 3, fill = NA, align = "center")
  ) %>%
  select(Year_clean, Labour_Productivity_Index, MA_3yr)

# Tracé du graphique
ggplot(df_labour_productivity_pct, aes(x = Year_clean)) +
  geom_line(aes(y = Labour_Productivity_Index), color = "#D55E00", linewidth = 1.2) +
  geom_point(aes(y = Labour_Productivity_Index), color = "#D55E00") +
  geom_line(aes(y = MA_3yr), color = "#F4A582", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Indexed Labour Productivity in Indian Industry (2000–2022)",
    subtitle = "Net Value Added per Manday (2000 = 100), with 3-year centered moving average (dashed line)",
    x = "Year",
    y = "Labour Productivity Index (2000 = 100%)"
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2), limits = c(2000, 2022)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()



# interpretation?
# very strong increasing trend over the last 25 years, not disrupted by major crises
# decoupling of capital and human productivity
# leaves the door open to a solution to preserve Indian competitiveness... needs to be evaluated at the level of sectors!




## CAPITAL INTENSITY
# comparing investment and labor FIXED K / MANDAYS

# computing said ratio
df_capital_intensity <- clean_an_series_22_23 %>%
  mutate(Year_clean = as.numeric(str_sub(Year, 1, 4))) %>%
  filter(Year_clean >= 2000) %>%
  mutate(
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Mandays_Workers = parse_number(`7. MANDAYS-WORKERS`),
    Capital_Intensity = Fixed_Capital / Mandays_Workers
  ) %>%
  select(Year_clean, Capital_Intensity) %>%
  arrange(Year_clean) %>%
  mutate(MA_3yr = zoo::rollmean(Capital_Intensity, k = 3, fill = NA, align = "center"))

# plot
ggplot(df_capital_intensity, aes(x = Year_clean)) +
  geom_line(aes(y = Capital_Intensity), color = "#0072B2", linewidth = 1.2) +  # bleu foncé
  geom_point(aes(y = Capital_Intensity), color = "#0072B2") +
  geom_line(aes(y = MA_3yr), color = "#89CFF0", linetype = "dashed", linewidth = 1.2) +  # bleu clair
  labs(
    title = "Capital Intensity in Indian Industry (Fixed Capital / Mandays, 2000–2022)",
    subtitle = "With 3-year centered moving average (dashed line)",
    x = "Year",
    y = "Capital Intensity (Rs. per Manday)"
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2), limits = c(2000, 2022)) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()

# interpretation?
# strong and continuous growth of the capital intensity. recent stabilization.


# capital intensity but index
df_capital_intensity_pct <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    Fixed_Capital = parse_number(`2. FIXED  CAPITAL`),
    Mandays_Workers = parse_number(`7. MANDAYS-WORKERS`),
    Capital_Intensity = Fixed_Capital / Mandays_Workers
  ) %>%
  filter(Year_clean >= 2000) %>%
  arrange(Year_clean) %>%
  mutate(
    Capital_Intensity_Index = 100 * Capital_Intensity / Capital_Intensity[Year_clean == 2000],
    MA_3yr = zoo::rollmean(Capital_Intensity_Index, k = 3, fill = NA, align = "center")
  ) %>%
  select(Year_clean, Capital_Intensity_Index, MA_3yr)
# plottt
ggplot(df_capital_intensity_pct, aes(x = Year_clean)) +
  geom_line(aes(y = Capital_Intensity_Index), color = "#0072B2", linewidth = 1.2) +  # bleu foncé
  geom_point(aes(y = Capital_Intensity_Index), color = "#0072B2") +
  geom_line(aes(y = MA_3yr), color = "#89CFF0", linetype = "dashed", linewidth = 1.2) +  # bleu clair
  labs(
    title = "Indexed Capital Intensity in Indian Industry (2000–2022)",
    subtitle = "Fixed Capital per Manday (2000 = 100), with 3-year centered moving average (dashed line)",
    x = "Year",
    y = "Capital Intensity Index (2000 = 100%)"
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2), limits = c(2000, 2022)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()



###  COMPARISON PLOTS

## capital vs labor productivity

# common data frame bc not the same scales (% or Rs per day)
df_prod_indexed <- df_capital_productivity %>%
  select(Year_clean, Capital_Productivity) %>%
  inner_join(df_labour_productivity %>% select(Year_clean, Labour_Productivity), by = "Year_clean") %>%
  mutate(
    Capital_Productivity_Index = 100 * Capital_Productivity / Capital_Productivity[Year_clean == 2000],
    Labour_Productivity_Index = 100 * Labour_Productivity / Labour_Productivity[Year_clean == 2000]
  ) %>%
  select(Year_clean, Capital_Productivity_Index, Labour_Productivity_Index) %>%
  pivot_longer(cols = c(Capital_Productivity_Index, Labour_Productivity_Index),
               names_to = "Productivity_Type",
               values_to = "Value")

# plot
ggplot(df_prod_indexed, aes(x = Year_clean, y = Value, color = Productivity_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_color_manual(
    values = c("Capital_Productivity_Index" = "#009E73", "Labour_Productivity_Index" = "#D55E00"),
    labels = c("Capital Productivity (Index)", "Labour Productivity (Index)")
  ) +
  labs(
    title = "Indexed Capital & Labour Productivity in Indian Industry (2000–2022)",
    subtitle = "Base year = 2000, index (2000 = 100)",
    x = "Year",
    y = "Productivity Index",
    color = "Indicator"
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  theme_minimal()



## capital productivity, labour productivity, capital intensity

# Création du data frame avec les trois indicateurs
df_all <- clean_an_series_22_23 %>%
  mutate(
    Year_clean = as.numeric(str_sub(Year, 1, 4)),
    Net_Value_Added = readr::parse_number(`21.NET VALUE ADDED`),
    Fixed_Capital = readr::parse_number(`2. FIXED  CAPITAL`),
    Mandays_Workers = readr::parse_number(`7. MANDAYS-WORKERS`),
    Capital_Productivity = Net_Value_Added / Fixed_Capital,
    Labour_Productivity = Net_Value_Added / Mandays_Workers,
    Capital_Intensity = Fixed_Capital / Mandays_Workers
  ) %>%
  filter(Year_clean >= 2000) %>%
  select(Year_clean, Capital_Productivity, Labour_Productivity, Capital_Intensity)

# Indexation base 100 en 2000
df_all_indexed <- df_all %>%
  mutate(
    Capital_Productivity_Index = 100 * Capital_Productivity / Capital_Productivity[Year_clean == 2000],
    Labour_Productivity_Index = 100 * Labour_Productivity / Labour_Productivity[Year_clean == 2000],
    Capital_Intensity_Index = 100 * Capital_Intensity / Capital_Intensity[Year_clean == 2000]
  ) %>%
  select(Year_clean, Capital_Productivity_Index, Labour_Productivity_Index, Capital_Intensity_Index) %>%
  pivot_longer(
    cols = ends_with("_Index"),
    names_to = "Indicator",
    values_to = "Value"
  )

# Tracé du graphique
ggplot(df_all_indexed, aes(x = Year_clean, y = Value, color = Indicator)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_color_manual(
    values = c(
      "Capital_Productivity_Index" = "#009E73",  # vert
      "Labour_Productivity_Index"  = "#D55E00",  # orange
      "Capital_Intensity_Index"    = "#0072B2"   # bleu
    ),
    labels = c(
      "Capital Productivity (Index)",
      "Labour Productivity (Index)",
      "Capital Intensity (Index)"
    )
  ) +
  labs(
    title = "Indexed Capital & Labour Productivity and Capital Intensity in Indian Industry (2000–2022)",
    subtitle = "Base year = 2000, index (2000 = 100)",
    x = "Year",
    y = "Index (2000 = 100)",
    color = "Indicator"
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  theme_minimal()



## COMMON INTERPRETATION

# the group of graphs helps us highlighting a decoupling of capital / labor productivity
# the lower capital productivity corresponds to a very fast capital intensification
# indeed: diminshing returns, potential overinvestment?, depends on the sectors...
# continuous augmentation of labor prodty resulting from a substitution

