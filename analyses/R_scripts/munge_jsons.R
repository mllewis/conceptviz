# read in raw simplified json from google, munge into long form tidy data frame and write to feathers
# critically, long form data include info about stroke number
# takes ~15 min per item

# load packages
library(tidyverse) 
library(rlist)
library(data.table)

# specify params
MIN_NUM_PARTICIPANTS_PER_COUNTRY <- 100

# get names for country codes
countries <- read.csv("../../data/supplementary_data/iso_3166_2_countries.csv") %>%
  rename(country = Common.Name,
         country_code = ISO.3166.1.2.Letter.Code) %>%
  select(country, country_code) %>%
  mutate(country_code = as.character(country_code)) %>% # for joining, below
  group_by(country_code) %>%
  slice(1) # get one name per code

# read in all file names
# file_list <- list.files("../../data/raw_data/simplified")
new_items <- c("eye", "face", "foot", "leg", "toe", "arm")
file_list <- lapply(new_items, function(x){paste0("full%2Fsimplified%2F", x, ".ndjson")}) %>%
  unlist()
# read data, munge and write function
write_drawings_to_feather <- function(name){
  
  # print to item to console
  print(name)
  
  # read in json
  d <- jsonlite::stream_in(file(paste0("../../data/raw_data/simplified/", name)), simplifyMatrix = FALSE)
  
  # get list of countries with many participants for this item
  keep_countries <- d %>%
    rename(country_code = countrycode) %>%
    count(country_code) %>%
    filter(n > MIN_NUM_PARTICIPANTS_PER_COUNTRY)
  
  # get subset of data with countries of minsize
  dc <- d %>%
    rename(country_code = countrycode) %>%
    filter(country_code %in% keep_countries$country_code) %>%
    select(word, country_code, drawing, key_id, recognized) %>%
    inner_join(countries) %>%
    filter(!is.na(country)) 
  
  # get meta-data in wide form
  dc_wide <- dc %>%
            mutate(drawing = lapply(drawing, lapply, function(x) { # bind together x and y values
                                  do.call(rbind, x)}), 
                  n_strokes = unlist(lapply(drawing, length))) 
  
  # get stroke number
  dc_strokes <- dc_wide %>%
          rowwise() %>%
          do(stroke_lengths = unlist(lapply(.$drawing, function(x) dim(x)[2]))) %>%
          ungroup() %>%
          mutate(mean_length = unlist(lapply(stroke_lengths, function(x) {mean(unlist(x))}))) %>%
          cbind(dc_wide) %>%
          arrange(key_id) # these is necessary for binding below

  # get all data in long form
  dc_coords <- dc_strokes %>%
    data.table() %>%
    group_by(key_id) %>%
    do(data.frame(t(as.matrix(data.frame(.$drawing))), # optimize here
                        country_code = as.character(.$country_code[1]),
                        country = as.character(.$country[1]), 
                        key_id = as.character(.$key_id[1]),
                        word = as.character(.$word[1]),
                        recognized = as.character(.$recognized[1]),
                        n_strokes = .$n_strokes[1],
                        mean_length = .$mean_length[1])) %>%
    rename(x = X1,
           y = X2) %>%
    arrange(key_id) # these is necessary for binding below

  # bind in stroke numbers to long form
  dc_coords_with_strokes <- dc_coords %>%
    bind_cols(data.frame(stroke_num = unlist(lapply(dc_strokes$stroke_lengths, 
                                                    function(m){unlist(lapply(seq_along(m), 
                                                                             function(x){rep(x, m[x])}))}))))
  # write to feather
  feather::write_feather(dc_coords_with_strokes, paste0("../../data/raw_data/feathers/atleast_100/", 
                                                        gsub(" ", "", dc_coords_with_strokes$word[1], fixed = TRUE)
, ".txt"))
}

# DO THE THING: loop over all files
purrr::walk(file_list, write_drawings_to_feather)
