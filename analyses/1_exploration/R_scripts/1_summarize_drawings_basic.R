# For each word x country, summarizes: n participants, mean/sd length, mean/sd strokes, prop recognized 
# writes feather for item

# load libraries and functions
library(tidyverse)
library(stringr)

file_list = list.files("../../data/raw_data/feathers")[72]

write_summaries_to_feather <- function(file_name){
    
    ####################Read in data####################
    d <- feather::read_feather(paste0("../../data/raw_data/feathers/", file_name))
    
    # print to item to console
    WORD <- unlist(str_split(d$word[1], ".txt"))[[1]]
    print(WORD)

    ############### Get ns by country #####################
    ns <- d %>%
      select(key_id, country) %>%
      distinct() %>%
      count(country)
    
    ### Get mean drawing length, n strokes, prop recognized ######
    strokes <- d %>%
      group_by(country, key_id) %>%
      slice(1) %>%
      group_by(country) %>%
      summarize(mean_lengths = mean(mean_length),
                sd_lengths  = sd(mean_length),
                mean_n_strokes = mean(n_strokes),
                sd_n_strokes = sd(n_strokes),
                prop_recognized = sum(as.logical(recognized))/n())

    ########## Merge and write to feather ################
    all <- ns %>%
      left_join(strokes) %>%
      mutate(word = WORD) %>%
      select(word, everything())
    
    feather::write_feather(all, paste0("../../data/summary_data/", WORD, "_summary"))
}

purrr::walk(file_list, write_summaries_to_feather)
