# munge simplified jsons to longform csv's

# load packages
library(dtplyr)
library(tidyverse) 
library(rlist)

countries <- read.csv("data/iso_3166_2_countries.csv") %>%
  select(Common.Name, ISO.3166.1.2.Letter.Code)

file.list = list.files("data/simplified")

for (i in 2:17){
    
    print(file.list[i])
    
    d <- jsonlite::stream_in(file(paste0("data/simplified/", file.list[i])), flatten = TRUE)
    
    d.flat = d %>%
      rowwise() %>%
      do(draw = list.flatten(list(drop(.$drawing)))) %>%
      cbind(d)
    
    
    keep.countries = d.flat %>%
      count(countrycode) %>%
      filter(n > 1500)
      
    ds = d.flat %>%
      filter(countrycode %in% keep.countries$countrycode) %>%
      select(word, countrycode, draw, key_id, recognized) %>%
      inner_join(countries, c("countrycode" = "ISO.3166.1.2.Letter.Code")) %>%
      rename(country = Common.Name) %>%
      filter(!is.na(country)) %>%
      mutate(n_strokes = unlist(lapply(draw, length)),
             mean_length = unlist(lapply(draw, 
                                         function(y) mean(unlist(sapply(y, length))))))

    ds.coords = ds  %>%
      rowwise() %>%
      do(data.frame(cbind(t(as.data.frame(list.flatten(.$draw))),
                          country = as.character(.$country[1]), 
                          key_id = as.character(.$key_id[1]),
                          word = as.character(.$word[1]),
                          recognized = as.character(.$recognized[1]),
                          n_strokes = .$n_strokes[1],
                          mean_length = .$mean_length[1]))) %>%
      bind_rows() %>%
      rename(x = V1,
             y = V2)
    
    write_csv(ds.coords, paste0("data/csvs/", ds.coords$word[1], ".csv"))
}