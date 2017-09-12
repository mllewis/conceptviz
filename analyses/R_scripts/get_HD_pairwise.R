### calculate HD pairwise using fast HD algorithm

rm(list=ls())

reticulate::use_python("/usr/local/bin/python") ## load this first otherwise get import errors

# load packages
library(tidyverse)
library(purrr)
library(feather)
library(data.table)
source("../R_scripts/helpers.R")

### PARAMS ###
RAW_PATH <- "../../data/raw_data/feathers/atleast_100/"
N_SAMPLES_PER_COUNTRY <- 50
N_PAIRS <- 1500
py2 <- reticulate::py_run_file("../R_scripts/hausdorff_fast_wrapper.py")


## get this from line 223 in concept_1_abstract_analyses
countries <- read_csv("~/Documents/research/Projects/conceptviz/data/supplementary_data/cultural_sim_measures/all_dyadic_vars2.csv") %>%
  separate(all_codes, c("country_code_1", "country_code_2"), 2) %>%
  na.omit() 
GOOD_COUNTRIES <- unique(c(countries$country_code_1, countries$country_code_2))

### ITEMS ###
items <- c("arm","leg", "toe", "foot", "face", "eye",
           "ear", "hand", "knee", "mouth", "line", "circle",
           "square", "hexagon", "triangle", "octagan")

### LOOP OVER ITEMS AND GET HD FOR PAIRS ###

for (i in length(items)){
  
  print(items[i])
  
  # point data
  raw_data <- read_feather(paste0(RAW_PATH, items[i], ".txt")) %>%
    data.table() %>%
    filter(country_code %in% GOOD_COUNTRIES) ### find a faster way to do this
  
  # get N_SAMPLES_PER_COUNTRY drawing key_ids from each country
  current_key_ids <- raw_data %>%
    group_by(country_code) %>%
    sample_n(N_SAMPLES_PER_COUNTRY) 
  
  # get indexes of random samples
  is_of_key_ids_1 <- sample(1:N_PAIRS)
  is_of_key_ids_2 <- sample(1:N_PAIRS)

  # get actual key_ids
  crit_pairs <- data.frame(key_id_1 = current_key_ids$key_ids[is_of_key_ids_1], 
                           key_id_2 = current_key_ids$key_ids[is_of_key_ids_2],
                           country_code_1 = current_key_ids$country_code[is_of_key_ids_1], 
                           country_code_2 = current_key_ids$country_code[is_of_key_ids_2])

  # get point data for the unique drawings in the pairs
  unique_ids_in_pairs <- unique(c(crit_pairs$key_id_1, crit_pairs$key_id_2))
  
  point_data <- raw_data %>%
    filter(key_id %in% unique_ids_in_pairs) %>%
    mutate(key_id_name = key_id) %>%
    rename(x_line = x, y_line = y) # this is dumb - it's just the name in the func

  # Get Hausdorff Distance (points only)
  hd_sims <- map2_df(crit_pairs$key_id_1, 
                     crit_pairs$key_id_2, 
                     get_hd_distance_fast, 
                     point_data,
                     py2)
  
  # merge in countries
  hd_sims_with_meta <- left_join(hd_sims, crit_pairs) %>%
    mutate(item = items[i])

  hd_this_path <- paste0("../../data/hausdorff_similarities/pair_lists/", items[i], "_sampled_pairs_with_sims_hd.csv")
  write_csv(hd_sims_with_meta, hd_this_path)
}
