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
N_SAMPLES_PER_COUNTRY <- 200
py2 <- reticulate::py_run_file("../R_scripts/hausdorff_fast_wrapper.py")

GOOD_COUNTRIES <-  c("US", "NZ", "NL", "BR" ,"IT", "KR", "AR", "BG",
                     "NO", "AU" ,"DE" ,"GB" ,"HU" ,"PL", "SE", "AE", "SA", "PH",
                     "RS", "ID", "DK", "VN", "SG", "CA",
                     "CZ", "MY", "JP", "FR", "EE", "RU",
                      "QA", "TR", "TH", "IE" ,"FI", "HR",
                      "ES" ,"UA", "IL", "SK", "CL", "TW",
                      "PT" ,"RO", "IQ", "DZ", "IN", "KH",
                      "AT", "HK", "EG", "BE", "SI", "LT",
                      "ZA", "GR", "BY", "BA" ,"MX" ,"CH",
                      "CO" ,"KW", "PK" ,"LV" ,"KZ" ,"JO")
items <- c("bread")


# point data
raw_data <- read_feather(paste0(RAW_PATH, items[1], ".txt")) %>%
  data.table() %>%
  filter(country_code %in% GOOD_COUNTRIES) ### find a faster way to do this


### LOOP OVER ITEMS AND GET HD FOR PAIRS ###

for (i in 1:length(GOOD_COUNTRIES)){
  
  print(GOOD_COUNTRIES[i])
  
  # get N_SAMPLES_PER_COUNTRY drawing key_ids from each country
  current_key_ids <- raw_data %>%
    filter(country_code == GOOD_COUNTRIES[i]) %>%
    group_by(country_code, key_id) %>%
    slice(1) %>%
    group_by(country_code) %>%
    sample_n(N_SAMPLES_PER_COUNTRY, replace = TRUE)  %>%
    mutate(ID = 1:n()) %>%
    select(country_code, key_id, ID)
    
  crit_pairs <- as.data.frame(t(combn(current_key_ids$key_id, 2))) %>%
      rename(key_id_1 = V1, 
             key_id_2 = V2)
  

  # get point data for the unique drawings in the pairs
  unique_ids_in_pairs <- unique(unlist(list(crit_pairs$key_id_1, 
                                            crit_pairs$key_id_2)))
  
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
    mutate(item = "bread",
           country_code = GOOD_COUNTRIES[i])

  hd_this_path <- paste0("../../data/hausdorff_similarities/within_country_distances/bread/bread_within_country_pairs_", GOOD_COUNTRIES[i], ".csv")
  write_csv(hd_sims_with_meta, hd_this_path)
}
