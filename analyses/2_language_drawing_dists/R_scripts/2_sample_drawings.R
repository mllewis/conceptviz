### Sample N drawings per item and country and write to feather

# load packages
library(tidyverse)
library(feather)
library(data.table)

### PARAMS ###
INPUT_PATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/raw_data/feathers/all/"
OUTPUT_PATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/raw_data/feathers/sampled/"
N_SAMPLES_PER_COUNTRY <- 50

CATEGORY_NAMES <- read_csv("../../data/misc/google_categories_coded.csv")  %>%
  filter(exclude != 1) %>% 
  select(google_category_name) %>%
  unlist(use.names = F)

### FUNCTIONS ###
write_sampled_drawings_for_one_country <- function(x, output_path) {
  df_x <- data.frame(x)
  path <- paste0(output_path, x$word[1], "_", x$countrycode[1], ".feather")
  write_feather(df_x, path)
}

write_drawings_for_item <- function(category, 
                                    nsamples, 
                                    input_path, 
                                    output_path){
  
  print(category)
  
  raw_drawing <- read_feather(paste0(input_path, category, "_tidy.txt")) %>%
    data.table(key = "key_id")
  
  sampled_drawings_ids <- raw_drawing[,.(key_id, countrycode)] %>%
    unique() %>%
    split(.$countrycode) %>%
    map(possibly(sample_n, data.frame(key_id = NA, countrycode = NA)), nsamples) %>%
    bind_rows() %>%
    filter(!is.na(countrycode)) %>%
    data.table(key = "key_id")
  
  sample_drawings <- raw_drawing[sampled_drawings_ids]
  
  sample_drawings %>%
    split(.$countrycode) %>% 
    walk(write_sampled_drawings_for_one_country, output_path)
}

### DO THE THING ###
walk(CATEGORY_NAMES,
     write_drawings_for_item, 
     N_SAMPLES_PER_COUNTRY, 
     INPUT_PATH, 
     OUTPUT_PATH)
