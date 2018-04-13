### get country pairwise HD means and save to csv

library(tidyverse)
library(feather)
library(data.table)

files <- list.files("/Volumes/wilbur_the_great/CONCEPTVIZ/similarity_measures/HD/country_pairwise/",
                    full.names = T)[-326]
df <- map_df(files, read_feather)

overall_means <- df %>%
  group_by(country1, country2, category) %>%
  summarize(mean_hd_sim = mean(hd_sim)) %>%
  group_by(country1, country2) %>%
  summarize(mean_hd_sim = mean(mean_hd_sim)) %>%
  mutate(type = "all")

by_category_means <- df %>%
  group_by(country1, country2, category) %>%
  summarize(mean_hd_sim = mean(hd_sim)) %>%
  mutate(type = "by_category")

category_types <- read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/google_categories_coded.csv") %>%
  select(google_category_name, category) %>%
  rename(category_type = category)

by_category_type_means <- df %>%
  left_join(category_types, by = c("category" = "google_category_name")) %>%
  group_by(country1, country2, category_type) %>%
  summarize(mean_hd_sim = mean(hd_sim)) %>%
  mutate(type = "by_category_type")

all_means <- bind_rows(overall_means, by_category_means, by_category_type_means)
write_csv(all_means, "/Volumes/wilbur_the_great/CONCEPTVIZ/word_distances/all_drawing_sims.csv")