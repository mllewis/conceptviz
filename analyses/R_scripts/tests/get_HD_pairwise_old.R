#### Sample pairs and calculate Haussdorf distance###

### LOAD PACKAGES ###
library(feather)
library(data.table)
library(lsa)
library(tidyverse)

#### PARAMS #######
RAW_PATH <- "../../data/raw_data/feathers/atleast_100/"
N_SAMPLES_PER_COUNTRY <- 50

#########  STUFF FOR LOOPING ####################################################################

# pairwise country key_ids function for looping over
get_country_pair_key_ids <- function(country_code_1, country_code_2, this_item, nested_raw_data, py){ 
  print(paste0(country_code_1, "_", country_code_2))
  
  # get key_ids for each country in pair
  country_1_key_ids <-  unlist(nested_raw_data[country_code == country_code_1, key_id], use.names = FALSE)
  country_2_key_ids <-  unlist(nested_raw_data[country_code == country_code_2, key_id], use.names = FALSE)
  
  # get all combos of key_id pairs
  all_key_id_combos <- expand.grid(country_1_key_ids, country_2_key_ids, stringsAsFactors = FALSE) %>%
    rename(key_id_1 = Var1, 
           key_id_2 = Var2)
  
  # key sims for each combo of key_id pairs
  sim_measures <- pmap(all_key_id_combos, get_pair_sim, country_code_1, country_code_2, nested_raw_data, py) %>%
    bind_rows() %>%
    mutate(word = this_item) %>%
    select(word, country_code_1, country_code_2, key_id_1, 
           key_id_2,  hd_sim)
  
  return(sim_measures)
}

# actual similarity function
get_pair_sim <- function(key_id_1, key_id_2, country_code_1, country_code_2, nested_raw_data, py){
  
  # points for two drawings
    pts1 = nested_raw_data[key_id == key_id_1, data][[1]] %>%
               distinct() %>%
               as.matrix()

    pts2 = nested_raw_data[key_id == key_id_2, data][[1]] %>%
      distinct() %>%
      as.matrix()
    
  # get hausdorff distance
  hd_sim <- py$hausdorff_wrapper(pts1, pts2)

  this_pair_sim <- data.frame(hd_sim = hd_sim, 
                              key_id_1 = key_id_1,
                              key_id_2 = key_id_2, 
                              country_code_1 = country_code_1,
                              country_code_2 = country_code_2)
  return(this_pair_sim)
}

py <- reticulate::py_run_file("../R_scripts/hausdorff_fast_wrapper.py")

######## DO THE THING #################################################################################
# loop over each unique pair of countries and get sims for all pairs
items <- c("arm","leg", "toe", "foot", "face", "eye",
           "ear", "hand", "knee", "mouth", "line", "circle",
           "square", "hexagon", "triangle", "octogan")
  
for (i in length(items)){
  print(items[i])
  # point data
  raw_data <- read_feather(paste0(RAW_PATH, items[i], ".txt")) %>%
    data.table()
  
  # nested point data
  nested_raw_data <- raw_data %>%
    select(word, country_code, key_id, x, y) %>%
    group_by(word, country_code, key_id) %>%
    nest() %>%
    data.table()
  
  setkey(nested_raw_data, key_id) # this allows us to index into featurs faster below
  
  # get all unique combos of countries into argument list
  all_countries <- unique(nested_raw_data$country_code)
  arg_list <- get_country_combos(all_countries)
  
  system.time(
  all_sims <- pmap(arg_list, 
                   get_country_pair_key_ids, 
                   items[i], 
                   nested_raw_data,
                   py) %>%
              bind_rows()
  )
  
  write_path <- paste0("../data/hausdorff_similarities/pair_sim_drawings/pair_lists/", items[i] ,"_sampled_pairs_with_sims_hd.csv")
  write_feather(all_sims, write_path)
}
