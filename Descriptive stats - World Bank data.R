#### PACKAGES #################################################################

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


############# DESCRIPTIVE STATISTICS FROM THE WORLD BANK ######################

# data obtained through: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG?end=2023&locations=IN&start=1995


###### Indian GDP growth over the 21st century

gdp_growth_world <- read_excel("~/work/world_bank_gdp_growth.xls", skip = 3)
head(gdp_growth_world)

print(colnames(gdp_growth_world))

# selecting india and keeping only the time series
india_row <- gdp_growth_world %>% filter(`Country Name` == "Inde")
gdp_india <- india_row %>%
  select(all_of(colnames(gdp_growth_world)[5:ncol(gdp_growth_world)])) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "GDP_Growth"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    GDP_Growth = as.numeric(GDP_Growth)
  ) %>%
  filter(Year >= 2000, !is.na(GDP_Growth))

# plotting the time series (with detailed x and y axis)
ggplot(gdp_india, aes(x = Year, y = GDP_Growth)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") + 
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2") +
  scale_y_continuous(
    breaks = seq(-10, 12, by = 2),  
    limits = c(-10, 12),            
    labels = function(x) paste0(x, " %")  
  ) +
  labs(
    title = "Annual growth of Indian GDP (2000–2023)",
    x = "Year",
    y = "GDP growth (%)"
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  theme_minimal()

