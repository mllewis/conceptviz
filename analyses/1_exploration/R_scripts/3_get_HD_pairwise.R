### calculate HD pairwise using fast HD algorithm - even drawing pairs across country pairs

reticulate::use_python("/usr/local/bin/python") ## load this first otherwise get import errors

# load packages
library(tidyverse)
library(purrr)
library(feather)
library(data.table)
#source("../R_scripts/helpers.R")

#source("helpers.R")

get_unique_pairs <- function(dat) data.frame(unique(t(apply(dat, 1, sort))))


### PARAMS ###
RAW_PATH <- "../../data/raw_data/feathers/atleast_100/"
N_SAMPLES_PER_COUNTRY <- 20
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
items1 <- c("")

get_key_id_pairs <- function(cc_1, cc_2, d){
   
   first_key_ids <- d %>%
     filter(country_code == cc_1) %>%
     select(country_code, key_id) %>%
     rename(country_code = country_code,
            key_id_1 = key_id) %>%
     sample_frac(1)
   
   
   second_key_ids <- d %>%
     filter(country_code == cc_2) %>%
     select(country_code, key_id) %>%
     rename(country_code = country_code,
            key_id_2 = key_id)
   
   data.frame(cbind(first_key_ids, second_key_ids))
   
 } 


unique_country_pairs <- as.data.frame(t(combn(GOOD_COUNTRIES, 2))) %>%
  rename(country_code_1 = V1, 
         country_code_2 = V2)

### LOOP OVER ITEMS AND GET HD FOR PAIRS ###

for (i in 1:length(items)){
  
  print(items[i])
  
  # point data
  raw_data <- read_feather(paste0(RAW_PATH, items[i], ".txt")) %>%
    data.table() %>%
    filter(country_code %in% GOOD_COUNTRIES) ### find a faster way to do this
  
  # get N_SAMPLES_PER_COUNTRY drawing key_ids from each country
  current_key_ids <- raw_data %>%
    group_by(country_code, key_id) %>%
    slice(1) %>%
    group_by(country_code) %>%
    sample_n(N_SAMPLES_PER_COUNTRY)  %>%
    mutate(ID = 1:n()) %>%
    select(country_code, key_id, ID)
  
  crit_pairs <- map2_df(unique_country_pairs$country_code_1,
                         unique_country_pairs$country_code_2,
                         get_key_id_pairs,
                         current_key_ids)

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
    mutate(item = items[i])

  hd_this_path <- paste0("../../data/hausdorff_similarities/pair_lists/", items[i], "_sampled_pairs_with_sims_hd_even_countries.csv")
  write_csv(hd_sims_with_meta, hd_this_path)
}
