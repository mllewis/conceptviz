### use climate dists and trade data to look at pairwise  relations ####
library(tidyverse)
library(forcats)
library(knitr)
library(countrycode)
library(stringr)


precip <- read_csv( "../../data/supplementary_data/cultural_sim_measures/geo/country_precip.csv") %>%
  select(ISO_3DIGIT, Annual_precip) %>%
  rename(precip = Annual_precip)

temp <- read_csv( "../../data/supplementary_data/cultural_sim_measures/geo/country_temp.csv")  %>%
  select(ISO_3DIGIT, Annual_temp) %>%
  rename(temp = Annual_temp)

climate_data <- precip %>%
  left_join(temp) %>%
  mutate(country_code  = countrycode(ISO_3DIGIT, "iso3c", "iso2c")) %>%
  select(country_code, temp, precip ) %>%
  mutate(country_code = as.factor(country_code))

write_csv(climate_data, "../../data/supplementary_data/cultural_sim_measures/geo/all_climiate_data.csv")