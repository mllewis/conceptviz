# save raw data as rdata rather than csv

# load libraries and functions
library(tidyverse)
library(entropy)
library(purrr)
library(data.table)


file.list = list.files("data/csvs/")

for (i in 1:length(file.list)){
  
    print(file.list[i])
  
    ####################Read in data####################
    d = read_csv(paste0("data/csvs/", file.list[i]))
    d = select(d, 1:8)
    
    WORD = d$word[1]
    
    # Save as Rdata to make reading in easier in future
    save(d, file=paste0("data/rdata/", WORD, ".RData"))
    
    #load(paste0("data/rdata/", "stereo", ".RData"))
    
}
