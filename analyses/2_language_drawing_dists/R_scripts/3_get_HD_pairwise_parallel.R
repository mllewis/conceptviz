### Calculate HD country-pairiwise pairwise by item using fast HD algorithm

# loops over unique country pairs, and for all sampled drawings (e.g. 50) for each category 
# get pairwise HD distance. Writes a single feather for each country pair (with all categories).

reticulate::use_python("/usr/local/bin/python") ## load this first otherwise get import errors

# load packages
library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(multidplyr)
library(parallel)

### PARAMS ###
INPUT_PATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/raw_data/feathers/sampled/"
OUTPUT_PATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/similarity_measures/HD/country_pairwise/"

GOOD_COUNTRIES <- read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/langs_sampled.csv") %>%
  select(countrycode) %>%
  unlist(use.names = F)

CATEGORY_NAMES <- read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/google_categories_coded.csv")  %>%
  filter(critical == 1,
         category %in% c("F", "K", "A", "M")) %>% 
  select(google_category_name) %>%
  unlist(use.names = F)

py2 <- reticulate::py_run_file("3a_hausdorff_fast_wrapper.py")

### FUNCTIONS ###
# Utility function for getting unique pairs
get_unique_pairs <- function(dat) data.frame(unique(t(apply(dat, 1, sort))))

# Hausdorff Distance (points only) function for two drawings
get_hd_distance_fast <- function(raw_d1, raw_d2, py){
  d1 <- raw_d1 %>%
    select(x, y) %>%
    distinct() %>%
    as.matrix()
  
  d2 <- raw_d2 %>%
    select(x, y) %>%
    distinct() %>%
    as.matrix()
  
  hd_sim <- py$hausdorff_wrapper(d1, d2)
  
  data.frame(key_id_1 = raw_d1$key_id[1],
             key_id_2 = raw_d2$key_id[1], 
             hd_sim = hd_sim)
}

# Get HD for all drawings for country-country-category
get_country_country_category_distances <- function(category,
                                                   country1, 
                                                   country2, 
                                                   inputpath,
                                                   py){
  print(category)
  
  country1path <- paste0(inputpath, category, "_", country1, ".feather")
  country1_drawings <- read_feather(country1path) %>%
    split(.$key_id)
  
  country2path <- paste0(inputpath, category, "_", country2, ".feather")
  country2_drawings <- read_feather(country2path) %>%
    split(.$key_id)
  
  country_country_category_pairwise_distances <- 
    cross2(country1_drawings, country2_drawings) %>%
    purrr::transpose() %>%
    pmap_df(get_hd_distance_fast, py) %>%
    mutate(category = category, 
           country1 = country1,
           country2 = country2)
  
  country_country_category_pairwise_distances

}

get_HDS_for_country_pairs_parallel <- function(country1, 
                                               country2,
                                               categories, 
                                               inputpath, 
                                               outputpath, 
                                               py) {
  reticulate::use_python("/usr/local/bin/python") ## load this first otherwise get import errors
  
  all_country_country_distances <- map_df(categories, 
                                          get_country_country_category_distances, 
                                          country1, 
                                          country2, 
                                          inputpath, 
                                          py)
  
  #return(all_country_country_distances)
  print("country1")
  
  full_outpath <- paste0(outputpath, country2, "_", country1, "_sampled_distances_all_categories.feather")
  write_feather(all_country_country_distances, full_outpath)
  
  
}


## GET THE COUNTRY PAIRS
BEGIN_INDEX <- 5
END_INDEX <- 730

country_country_pairs <- list(x = rev(GOOD_COUNTRIES), y = rev(GOOD_COUNTRIES)) %>%
  cross_df() %>%
  get_unique_pairs() %>%
  as.data.frame() %>%
  rename(countrycode1 = X1, countrycode2 = X2)  %>%
  mutate_all(as.character) %>%
  filter(countrycode1 != countrycode2) %>%
  slice(BEGIN_INDEX:END_INDEX) %>%
  as.list() %>%
  transpose()

### DO IN PARALLEL
parallel_wrapper <- function(x){
  langs <- unlist(x)
  walk2(langs[1], 
        langs[2], 
        get_HDS_for_country_pairs_parallel, 
        CATEGORY_NAMES, 
        INPUT_PATH, 
        OUTPUT_PATH,
        py2)
}

# INITIATE CLUSTER
cluster <- makeCluster(7, type = "FORK")

# DO THE THING (IN PARALLEL)
parLapply(cluster, 
          country_country_pairs,
          parallel_wrapper)


