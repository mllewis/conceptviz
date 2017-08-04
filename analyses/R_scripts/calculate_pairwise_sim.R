#### Sample pairwise sim for single item based on pre-cacluated model features ######

### LOAD PACKAGES ###
library(feather)
library(data.table)
library(lsa)
library(tidyverse)

#### PARAMS #######
ITEM <- "tree"
READ_PATH <- paste0("../../data/keras_similarities/features/", ITEM, "_country_features.csv")
WRITE_PATH <- ""

d <- read_csv(READ_PATH, col_names = FALSE)

d_clean <-  d %>%
  mutate(item = ITEM) %>%
  select(-X2) %>%
  rename(country_code = X3,
         key_id = X4) %>%
  mutate(key_id = as.character(key_id)) %>%
  distinct() %>% # get rid of accidental duplicates
  group_by(country_code, key_id, item) %>%
  nest(.key = features)

#pairs <- combn(unique(d_clean$country_code), 2)

arg_list = list(d_clean$country_code, # 50 x each country code
                d_clean$key_id, # each of 50 key ids for each country
                d_clean$item, # all trees
                d_clean$features) # features for each key id

get_sim <- function(features_2, key_id_2, country_code_2, features_1){
  
  f1 = as.vector(unlist(features_1))
  f2 = as.vector(unlist(features_2))

  cosine <- lsa::cosine(f1, f2)[1]

  this_pair_sim <- data.frame(cosine = cosine, 
                              key_id_2 = key_id_2,
                              country_code_2 = country_code_2)
  
  return(this_pair_sim)
}

get_pairwise_sims <- function(this_country_code, this_key_id, this_item, 
                              these_features, all_key_ids, all_features,
                              all_country_codes){
  print(this_key_id)
  cosines <- pmap(list(all_features, all_key_ids, all_country_codes),
                  get_sim, these_features) %>%
    bind_rows %>%
    mutate(country_code_1 = this_country_code, 
           key_id_1 = this_key_id,
           item = this_item) %>%
    select(item, key_id_1, country_code_1, 
           key_id_2, country_code_2, cosine)

  return(cosines)
}

all_sims = pmap(arg_list, 
                get_pairwise_sims, 
                d_clean$key_id,
                d_clean$features, 
                d_clean$country_code) %>%
        bind_rows()

write_feather(all_sims, "all_sims.txt")
# get all unique combinations using combination function

