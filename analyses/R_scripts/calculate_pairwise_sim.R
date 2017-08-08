#### Sample pairwise sim for single item based on pre-cacluated model features ######
# ~ 3.5 hours per item

### LOAD PACKAGES ###
library(feather)
library(data.table)
library(lsa)
library(tidyverse)

#### PARAMS #######
ITEM <- "bread"
READ_PATH <- paste0("../../data/keras_similarities/features/", ITEM, "_country_features.csv")
WRITE_PATH <- paste0("../../data/keras_similarities/pairwise_country/", ITEM, "_all_sims.txt")

# read in feature data
d <- read_csv(READ_PATH, col_names = FALSE)

#d_write <- d %>%
#  slice(1:1000) %>%
#  rename(country = X3, subj = X4)

#write_tsv(select(d_write, country, subj), "labels.tsv")
#write_tsv(select(d_write, X5:X4100), "values.tsv", col_names = F)

# tidy feature data
d_clean <-  d %>%
  mutate(X1 = ITEM) %>%
  rename(item = X1,
        country_code = X3,
        key_id = X4) %>%
  select(-X2) %>%
  mutate(key_id = as.character(key_id)) %>%
  distinct() %>% # get rid of accidental duplicates
  group_by(item, country_code, key_id) %>%
  nest(.key = features) %>% 
  data.table()

setkey(d_clean, key_id) # this allows us to index into featurs faster below

# get all unique combos of countries 
unique_country_combos <- combinat::combn(unique(d_clean$country_code), 2) %>%
  t() %>%
  as.data.frame() %>%
  rename(c_1 = V1,
         c_2 = V2) %>%
  mutate_all(as.character) %>%
  bind_rows(data.frame(c_1 = unique(d_clean$country_code),# (include within country, e.g. US_US)
            c_2 = unique(d_clean$country_code)))

# make lists for looping over
arg_list = list(unique_country_combos$c_1,
                unique_country_combos$c_2)
# arg_list = list("US", "CA") # for debugging

# define similarity functions
get_pairwise_sims <- function(country_code_1, country_code_2, this_item, all_data){
  print(paste0(country_code_1, "_", country_code_2))
  
  # get key_ids for each country in pair
  country_1_key_ids <-  unlist(all_data[country_code == country_code_1, key_id], use.names = FALSE)
  country_2_key_ids <-  unlist(all_data[country_code == country_code_2, key_id], use.names = FALSE)
  
  # get all combos of key_id pairs
  all_key_id_combos <- expand.grid(country_1_key_ids, country_2_key_ids, stringsAsFactors = FALSE) %>%
    rename(key_id_1 = Var1, 
           key_id_2 = Var2)
  
  # key sims for each combo of key_id pairs
  cosines <- pmap(all_key_id_combos, get_sim, country_code_1, country_code_2, all_data) %>%
    bind_rows() %>%
    mutate(item = this_item) %>%
    select(item, country_code_1,country_code_2, key_id_1, 
           key_id_2,  cosine)
  
  return(cosines)
}

get_sim <- function(key_id_1, key_id_2, country_code_1, country_code_2, all_data){
  
  # get features for two key_ids
  f1 = unlist(all_data[key_id_1, features], use.names = FALSE) # false speeds up
  f2 = unlist(all_data[key_id_2, features], use.names = FALSE)
  
  # get sim
  cosine <- lsa::cosine(f1, f2)[1]

  this_pair_sim <- data.frame(cosine = cosine, 
                              key_id_1 = key_id_1,
                              key_id_2 = key_id_2, 
                              country_code_1 = country_code_1,
                              country_code_2 = country_code_2)
  
  return(this_pair_sim)
}

# DO THE THING: loop over each unique pair of countries and get sims for all pairs
all_sims = pmap(arg_list, get_pairwise_sims, ITEM, d_clean) %>%
              bind_rows()

write_feather(all_sims, WRITE_PATH)

