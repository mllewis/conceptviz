### (1) Get counts of items by country (all_country_counts)
### (2) Saves lists of countries that meet a criteria (good_country_list)

library(tidyverse)
library(feather)
library(data.table)

### PARAMS ###
INPUT_PATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/raw_data/feathers/all/"
OUTPUT_PATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/misc/country_counts.csv"
OUTPUT_PATH2 <- "/Volumes/wilbur_the_great/CONCEPTVIZ/misc/good_country_list.csv"
CATEGORY_NAMES <- read_csv("../../../data/misc/google_categories_coded.csv")  %>%
  filter(exclude != 1) %>% 
  select(google_category_name) %>%
  unlist(use.names = F)

### (1) COUNTS BY ITEM-COUNTRY ###

get_countries <- function(category, input_path){
  
  print(category)
  
  raw_drawing <- read_feather(paste0(input_path, category, "_tidy.txt")) %>%
    data.table(key = "key_id")
  
  countries_ids <- unique(raw_drawing[,.(countrycode, key_id)])
  country_counts <- countries_ids[, .(.N), by = .(countrycode)]
  country_counts[, "word" := category]

}

# do thte thing
all_country_counts <- map_df(CATEGORY_NAMES, get_countries, INPUT_PATH)

# write_csv(all_country_counts, OUTPUT_PATH)

### (2) DETERMINE LIST OF COUNTRIES ###
MIN_NUM_DRAWINGS_PER_ITEM <- 50
MIN_NUM_COUNTRIES_PER_ITEM <- 288

good_country_list <- all_country_counts %>%
  filter(N >= MIN_NUM_DRAWINGS_PER_ITEM)  %>%
  count(countrycode) %>%
  filter(n == MIN_NUM_COUNTRIES_PER_ITEM)

# write_csv(good_country_list, OUTPUT_PATH2)
  
