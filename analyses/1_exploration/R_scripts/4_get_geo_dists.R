###########Get dists between all captials and country centers##########

library(tidyverse)
library(forcats)
library(feather)
library(maps)
library(countrycode)

## get countries in google
d <- read_feather("../../data/raw_data/feathers/atleast_100/tree.txt")

all_g_countries <- d %>%
  group_by(key_id) %>%
  slice(1)%>%
  ungroup() %>%
  count(country_code) %>%
  select(-n)

#write_csv(all_g_countries, "../../data/supplementary_data/cultural_sim_measures/geo/all_google_countries.csv")
g_countries = read_csv("../../data/supplementary_data/cultural_sim_measures/geo/all_google_countries.csv")

## get capital cities
data(world.cities) # load map data

all_capitals <- world.cities %>%
  mutate(country_code = countrycode(country.etc, 'country.name', 'iso2c')) %>%
  mutate(country_code = replace(country_code, country.etc =="Serbia and Montenegro", "RS")) %>%
  filter(capital == 1) 

g_capitals <- g_countries %>%
  left_join(all_capitals)  %>%
  select(-capital, -pop, -country.etc) %>%
  rename(capital_name = name) %>%
  mutate(capital_name = replace(capital_name, country_code =="HK", NA),
         lat = replace(lat, country_code =="HK", 22.28552),
         long = replace(long, country_code =="HK", 114.15769)) #hong kong doesn't have capital

#################Capital Distance#################
g_dists <- geosphere::distm(cbind(g_capitals$long, g_capitals$lat)) # in meters
rownames(g_dists) <- g_capitals$capital_name
colnames(g_dists) <- g_capitals$capital_name

all_dists <- reshape2::melt(g_dists) %>%
  rename(city1 = Var1,
         city2 = Var2,
         capital_dist_meters = value)

all_capital_dists_with_countries <- all_dists %>%
  left_join(g_capitals %>% select(country_code, capital_name), by = c("city1" = "capital_name")) %>%
  left_join(g_capitals %>% select(country_code, capital_name), by = c("city2" = "capital_name")) %>%
  rename(country_code_1 = country_code.x,
         country_code_2 = country_code.y) %>%
  select(country_code_1, country_code_2, city1, city2, capital_dist_meters)

#################Country Centroids#################
# data from: http://gothos.info/resource_files/country_centroids.zip
centroids <- read_tsv("../../data/supplementary_data/cultural_sim_measures/geo/country_centroids/country_centroids_primary.csv") %>%
  select(ISO3136, LAT, LONG) %>%
  rename(country_code = ISO3136) %>%
  add_row(country_code = "HK", LAT = 22.28552, LONG =  114.15769) %>%
  right_join(g_countries)  

g_centroids <- geosphere::distm(cbind(centroids$LONG, centroids$LAT)) # in meters
rownames(g_centroids) <- centroids$country_code
colnames(g_centroids) <- centroids$country_code

all_centroid_dists <- reshape2::melt(g_centroids) %>%
  rename(country_code_1 = Var1,
         country_code_2 = Var2,
         centroid_dist_meters = value)

all_dists <- left_join(all_capital_dists_with_countries, all_centroid_dists) %>%
  select(country_code_1, country_code_2,
         capital_dist_meters, centroid_dist_meters)

ggplot(all_dists, aes(x = log(centroid_dist_meters),
                      y = log(capital_dist_meters))) +
  geom_point() +
  geom_smooth(method = "lm")

write_csv(all_centroid_dists, "../../data/supplementary_data/cultural_sim_measures/geo/all_google_geo_dists_clean.csv")
