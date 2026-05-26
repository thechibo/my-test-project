# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCA for Market Expansion Strategy
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Dependencies              ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages(c("eurostat", "tidyverse", "janitor"))

library(eurostat)
library(tidyverse)
library(janitor)

# Choose a recent year
year <- 2018

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Eurostat Urban Indicators ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Available datasets
search_results <- search_eurostat("city")
head(search_results)

# Population
df_pop <- get_eurostat("urb_cpop1", time_format = "num")

# Unemployment
df_unemp <- get_eurostat("urb_clma", time_format = "num")

# Education
df_educ <- get_eurostat("urb_ceduc", time_format = "num")

# Tourism (proxy for activity / attractiveness)
df_tourism <- get_eurostat("urb_ctour", time_format = "num")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Data Wrangling
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_pop <- df_pop %>% 
  filter(TIME_PERIOD == year & indic_ur == "DE1001V") %>%
  select(indic_ur, cities, values)

df_unemp <- df_unemp %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

df_educ <- df_educ %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

df_tourism <- df_tourism %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

# Merge the dataset
df <- rbind(df_pop, df_unemp, df_educ, df_tourism)

# Update the labels
df <- label_eurostat(df)

# Remove indicators that are available for the male-female population
df <- df %>%
  filter(!str_detect(indic_ur, regex("male|female", ignore_case = TRUE)))

# Bring to tidy format
df <- pivot_wider(df,
                  names_from = indic_ur,
                  values_from = values)

# Keep only the variables with low missing values
df <- df[, colMeans(is.na(df)) < 0.30]

# Keep cities with no missing values
df <- df[rowMeans(is.na(df)) == 0, ]
