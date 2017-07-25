###########Get dists between all captials##########

library(tidyverse)
library(forcats)

## get countries in google
countries <- read.csv("../../data/supplementary_data/iso_3166_2_countries.csv") %>%
  rename(country = Common.Name,
         country_code = ISO.3166.1.2.Letter.Code) %>%
  select(country, country_code) %>%
  mutate(country_code = as.character(country_code)) %>% # for joining, below
  group_by(country_code) %>%
  slice(1)


geo_codes <- read_csv("../../data/supplementary_data/geo_codes.csv")

d <- list.files("../../data/summary_data/") %>%
  purrr::map(function(x) paste0("../../data/summary_data/", x)) %>% 
  purrr::map(read_feather) %>% 
  bind_rows() 

d_codes <- left_join(d, geo_codes, by = c("country" = "countries")) %>%
  left_join(countries) %>%
  distinct(country, country_code)

#write_csv(d_codes, "../../data/supplementary_data/cultural_sim_measures/geo/all_google_countries.csv")
g_cities = read_csv("../../data/supplementary_data/cultural_sim_measures/geo/all_google_countries.csv")


## get capital cities
data(maps::world.cities) # load map data

all_capitals <-  world.cities %>%
  filter(capital == 1) %>%
  mutate(country.etc = fct_recode(country.etc,
                                  "United Kingdom" = "UK",
                                  "United States" = "USA",
                                  "Korea, South" = "Korea South",
                                  "China, Republic of (Taiwan)" = "Taiwan",
                                  "Serbia" = "Serbia and Montenegro"))
                                  

g_cities_data <- g_cities %>%
  left_join(all_capitals, by = c("country" = "country.etc")) %>%
  select(-capital, -pop) %>%
  rename(capital_name = name)

#write_csv(g_cities_data, "../../data/supplementary_data/cultural_sim_measures/geo/all_google_capitals.csv")
g_capitals <- read_csv("../../data/supplementary_data/cultural_sim_measures/geo/all_google_capitals.csv")

## get pairwise capital dist
g_dists <- geosphere::distm(cbind(g_capitals$long, g_capitals$lat)) # in meters
rownames(g_dists) <- g_capitals$capital_name
colnames(g_dists) <- g_capitals$capital_name

all_dists <- reshape2::melt(g_dists) %>%
  rename(city1 = Var1,
         city2 = Var2,
         dist_meters = value)

all_dists_with_countries = all_dists %>%
  left_join(g_capitals %>% select(country, capital_name), by = c("city1" = "capital_name")) %>%
  left_join(g_capitals %>% select(country, capital_name), by = c("city2" = "capital_name")) %>%
  rename(country1 = country.x,
         country2 = country.y) %>%
  select(country1, country2, city1, city2, dist_meters)

write_csv(all_dists_with_countries, "../../data/cultural_sim_measures/geo/supplementary_data/all_google_capital_dists.csv")
  
  
