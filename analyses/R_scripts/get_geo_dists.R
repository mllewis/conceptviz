###########Get dists between all captials and country centers##########

library(tidyverse)
library(forcats)
library(feather)

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
g_countries = read_csv("../../data/supplementary_data/cultural_sim_measures/geo/all_google_countries.csv")

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
                                  

g_cities_data <- g_countries %>%
  left_join(all_capitals, by = c("country" = "country.etc")) %>%
  select(-capital, -pop) %>%
  rename(capital_name = name)

#write_csv(g_cities_data, "../../data/supplementary_data/cultural_sim_measures/geo/all_google_capitals.csv")
g_capitals <- read_csv("../../data/supplementary_data/cultural_sim_measures/geo/all_google_capitals.csv")

#################Capital Distance#################
g_dists <- geosphere::distm(cbind(g_capitals$long, g_capitals$lat)) # in meters
rownames(g_dists) <- g_capitals$capital_name
colnames(g_dists) <- g_capitals$capital_name

all_dists <- reshape2::melt(g_dists) %>%
  rename(city1 = Var1,
         city2 = Var2,
         capital_dist_meters = value)

all_capital_dists_with_countries <- all_dists %>%
  left_join(g_capitals %>% select(country, capital_name), by = c("city1" = "capital_name")) %>%
  left_join(g_capitals %>% select(country, capital_name), by = c("city2" = "capital_name")) %>%
  rename(country1 = country.x,
         country2 = country.y) %>%
  select(country1, country2, city1, city2, capital_dist_meters)

#################Country Centroids#################
# data from: http://gothos.info/resource_files/country_centroids.zip
centroids <- read_tsv("../../data/supplementary_data/cultural_sim_measures/geo/country_centroids/country_centroids_primary.csv") %>%
  select(ISO3136, SHORT_NAME, LAT, LONG) %>%
  left_join(g_countries, by = c("ISO3136" = "country_code")) %>%
  filter(!is.na(country))

g_centroids <- geosphere::distm(cbind(centroids$LONG, centroids$LAT)) # in meters
rownames(g_centroids) <- centroids$country
colnames(g_centroids) <- centroids$country

all_centroid_dists <- reshape2::melt(g_centroids) %>%
  rename(country1 = Var1,
         country2 = Var2,
         centroid_dist_meters = value)

all_dists <- left_join(all_capital_dists_with_countries, all_centroid_dists) %>%
  left_join(g_countries %>% select(country, country_code), by = c("country1"= "country")) %>%
  rename(country_code1 = country_code) %>%
  left_join(g_countries %>% select(country, country_code), by = c("country2"= "country")) %>%
  rename(country_code2 = country_code) %>%
  select(country_code1, country_code2, city1, city2, capital_dist_meters, centroid_dist_meters)

ggplot(all_dists, aes(x = log(centroid_dist_meters),
                      y = log(capital_dist_meters))) +
  geom_point() +
  geom_smooth(method = "lm")

write_csv(all_dists, "../../data/supplementary_data/cultural_sim_measures/geo/all_google_geo_dists.csv")
