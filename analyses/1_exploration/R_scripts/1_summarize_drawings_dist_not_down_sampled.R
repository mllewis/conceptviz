# load libraries and functions
library(tidyverse)
library(emdist)
library(purrr)
library(data.table)
library(magrittr)
library(gtools)

theme_blank <- function(...) {
  ret <- theme_bw(...)
  ret$line <- element_blank()
  ret$axis.text <- element_blank()
  ret$axis.title <- element_blank()
  ret
}

rescale <- function(x, newrange=range(x)){
  xrange <- range(x)
  mfac <- (newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  newrange[1]+(x-xrange[1])*mfac
}

ResizeMat <- function(mat, ndim=dim(mat)){
  if(!require(fields)) stop("`fields` required.")
  
  # input object
  odim <- dim(mat)
  obj <- list(x= 1:odim[1], y=1:odim[2], z= mat)
  
  # output object
  ans <- matrix(NA, nrow=ndim[1], ncol=ndim[2])
  ndim <- dim(ans)
  
  # rescaling
  ncord <- as.matrix(expand.grid(seq_len(ndim[1]), seq_len(ndim[2])))
  loc <- ncord
  loc[,1] = rescale(ncord[,1], c(1,odim[1]))
  loc[,2] = rescale(ncord[,2], c(1,odim[2]))
  
  # interpolation
  ans[ncord] <- interp.surface(obj, loc)
  
  ans
}

# dataframes for subsets countries needed below
big.countries <- c( "Australia", "Canada",
                    "Germany","Russia", "United Kingdom","United States","Finland" ,
                    "Sweden","Brazil", "Czech Republic" , "Italy","Poland","Philippines",
                    "France","Thailand" ,"Korea, South","Saudi Arabia","Hungary") 

country.combo <- combinations(n = length(big.countries), 
                              r = 2, 
                              repeats.allowed = F, 
                              v = big.countries) %>%
  as.data.frame() %>%
  rename(c1 = V1, c2 = V2)

# loop over files
file.list =  list.files("data/rdata/")

for (i in 1:length(file.list)){
  print(file.list[i])
  
  load(paste0("data/rdata/", file.list[i]))
  
  WORD = d$word[1]
  # N_TOTAL = 1500
  # 
  # down.sampled.d = d %>%
  #   data.table() %>%
  #   .[country %in% big.countries] %>%
  #   group_by(country, key_id) %>%
  #   nest() %>%
  #   group_by(country) %>%
  #   sample_n(N_TOTAL) %>%
  #   unnest()
  
  # num counts by x, y, country
  counts = d %>%
    count(x, y, country) %>%
    mutate(n = log(as.numeric(n)))  # take log
  
  ########Get mean distance between countries ###########
  
  get_entropies = function(cntry1, cntry2, counts){
    
    d1 <- counts[counts$country == cntry1,]
    d2 <- counts[counts$country == cntry2,]
    
    DIM = 20 # downsampling size (for emd)
    
    matrix1 = d1 %>%
      ungroup() %>%
      select(x, y, n) %>%
      spread(x, n) %>%
      mutate_each(funs(ifelse(is.na(.), 0, .))) %>%
      select(-y) %>%
      as.matrix(., rownames.force = NA)  %>%
      ResizeMat(., c(DIM, DIM)) # downsample matrix size
    
    matrix1 = matrix1/sum(matrix1) # normalize
    
    matrix2 = d2 %>%
      ungroup() %>%
      select(x, y, n) %>%
      spread(x, n) %>%
      mutate_each(funs(ifelse(is.na(.), 0, .))) %>%
      select(-y) %>%
      as.matrix(., rownames.force = NA)  %>%
      ResizeMat(., c(DIM, DIM))
    
    matrix2 = matrix2/sum(matrix2)
    
    distance = emd2d(matrix1, matrix2) # earth mover distance
    
    data.frame(country1 = cntry1, 
               country2 = cntry2, 
               dist = distance, row.names = NULL)
  }
  
  # get rid of country combos not present for this item
  this.country.combo <- country.combo %>%
    filter(c1 %in% unique(counts$country) & c2 %in% unique(counts$country))
  
  distances = purrr::map2(this.country.combo$c1, this.country.combo$c2, 
                          get_entropies, counts) %>%
    bind_rows()
  
  ##########Merge and write to data file################
  
  distances %<>% mutate(word = WORD) 
  
  write_csv(distances, paste0("data/emd/", WORD, "_emd.csv"))
}