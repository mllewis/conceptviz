# load libraries and functions
library(tidyverse)
library(entropy)
library(purrr)
library(data.table)


theme_blank <- function(...) {
  ret <- theme_bw(...)
  ret$line <- element_blank()
  ret$axis.text <- element_blank()
  ret$axis.title <- element_blank()
  ret
}

file.list = list.files("data/csvs/")

for (i in 3:length(file.list)){
  
    print(file.list[i])
  
    ####################Read in data####################
    d = read_csv(paste0("data/csvs/", file.list[i]))
    d = select(d, 1:8)
    
    WORD = d$word[1]
    
    # Save as Rdata to make reading in easier in future
    save(d, file=paste0("data/rdata/", WORD, ".RData"))
    
    #load(paste0("data/rdata/", "stereo", ".RData"))
    
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
      
    #############Save US vs.non-US heat map###############
    english_countries = c("Australia", "Canada", "United Kingdom", "United States")
    
    counts = d %>%
      #data.table() %>%
      mutate(lang.status = ifelse(country %in% english_countries, "english", "non_english")) %>%
      count(x, y, lang.status) %>%
      mutate(n = log(as.numeric(n)))  # take log
    
    counts.trimmed = counts %>%
      filter(x > 10 & y > 10 & x < 245 & y < 245) %>% #remove edges
      group_by(lang.status) %>%
      summarize(med = median(n),
                sd = sd(n)) %>%
      left_join(counts) %>%
      group_by(lang.status) %>%
      filter(n<(med + 1.5*sd) & n>(med-1.5*sd)) # remove outliers
    
    counts.scaled = counts.trimmed %>%
      group_by(lang.status) %>%
      summarize(min = min(n),
                max = max(n)) %>%
      left_join(counts.trimmed) %>%
      group_by(lang.status) %>%
      mutate(n.scaled =(n-min)/(max-min)) %>%
      data.table()
    
    ns_lang = by.key.id %>%
      ungroup() %>%
      mutate(lang.status = ifelse(country %in% english_countries, "english", "non_english")) %>%
      count(lang.status)
    
    heat.maps = ggplot(counts.scaled, aes(x,y)) + 
      facet_wrap(~lang.status) +
      geom_raster(aes(fill = n.scaled), interpolate = TRUE) + 
      scale_fill_gradient2(midpoint = .5, low = "green", mid = "white", high = "red") +
      scale_y_continuous(limits = c(10,245)) + 
      scale_x_continuous(limits = c(10,245))+
      ggtitle(WORD) +
      geom_text(data=data.frame(x=36, y=240, label=c(ns_lang[ns_lang$lang.status == "english", "n"][[1]],
                                                     ns_lang[ns_lang$lang.status == "non_english", "n"][[1]]), 
                lang.status = c("english", "non_english")), 
                aes(x,y,label=label), inherit.aes = FALSE) +
      theme_blank() +
      theme(legend.position = "none",
            strip.text= element_text(size = 10))
            
    pdf(paste0("heatmaps/", WORD, "_lang_heatmap.pdf"), width = 8, height = 4.5)
      print(heat.maps)
    dev.off()
    
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

