# Calculates mean length, number of strokes and entropy for each word x country

# load libraries and functions
library(tidyverse)
library(entropy)
library(purrr)
library(data.table)

file.list =  list.files("data/rdata/")

for (i in 3:length(file.list)){
  
    print(file.list[i])
  
    ####################Read in data####################
    load(paste0("data/rdata/", file.list[i]))
    d = select(d, 1:8)
    
    WORD = d$word[1]

    ###############Get ns by country#####################
    by.key.id = d %>%
      group_by(key_id) %>%
      slice(1) %>%
      ungroup()
    
    ns = by.key.id %>%
            count(country)
    
    ###Get mean drawing length and number of strokes######
    strokes = by.key.id %>%
      group_by(country) %>%
      summarize(mean_lengths = mean(mean_length),
                n_strokes = mean(n_strokes))

    ########Get entropy over heatmap by country###########
    counts = d %>%
      count(x, y, country) %>%
      mutate(n = log(as.numeric(n))) # take log
    
    counts.trimmed = counts %>%
      filter(x > 10 & y > 10 & x < 245 & y < 245) %>% #removed edges
      group_by(country) %>%
      summarize(med = median(n),
                sd = sd(n)) %>%
      left_join(counts) %>%
      group_by(country) %>%
      filter(n<(med + 1.5*sd) & n>(med-1.5*sd)) # remove outliers
    
    counts.scaled = counts.trimmed %>%
      group_by(country) %>%
      summarize(min = min(n),
                max = max(n)) %>%
      left_join(counts.trimmed) %>%
      group_by(country) %>%
      mutate(n.scaled = (n-min)/(max-min)) %>%
      data.table()
    
    get_entropies = function(cntry, counts.scaled){
      
      d <- counts.scaled[counts.scaled$country == cntry,]
      
      data.for.matrix = d %>%
        select(x,y,n) 
      
      n_H = spread(data.for.matrix, x, n) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .))) %>%
        do(H = mi.empirical(as.matrix(.))) %>% 
        unlist()
      
      data.for.matrix2 = d %>%
        select(x,y,n.scaled) 
      
      n.scaled_H = spread(data.for.matrix2, x, n.scaled) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .))) %>%
        do(H = mi.empirical(as.matrix(.))) %>% 
        unlist()
      
      data.frame(country = cntry, H = n_H , H.scaled = n.scaled_H, row.names = NULL)
    }
    
    entropies = unique(counts.scaled$country) %>%
          map(get_entropies, counts.scaled) %>%
          bind_rows()
    
    ##########Merge and write to data file################
    
    all = ns %>%
      left_join(strokes) %>%
      left_join(entropies) %>%
      mutate(word = WORD) %>%
      select(word, country:H.scaled)
    
    write_csv(all, paste0("data/summaries/", WORD, "_summary.csv"))
}

