# gets down sampled earth movers distance
# load libraries and functions
library(tidyverse)
library(emdist)
library(purrr)
library(data.table)
library(magrittr)
library(gtools)

######## define functions  ######## 
rescale <- function(x, newrange=range(x)){
  xrange <- range(x)
  mfac <- (newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  newrange[1]+(x-xrange[1])*mfac
}

resize_mat <- function(mat, ndim=dim(mat)){
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

get_movers <- function(cntry1, cntry2, counts, dim){
  
  d1 <- counts[counts$country == cntry1,]
  d2 <- counts[counts$country == cntry2,]
  
  matrix1 = d1 %>%
    ungroup() %>%
    select(x, y, n) %>%
    spread(x, n) %>%
    mutate_each(funs(ifelse(is.na(.), 0, .))) %>%
    select(-y) %>%
    as.matrix(., rownames.force = NA)  %>%
    resize_mat(., c(dim, dim)) # downsample matrix size
  
  matrix1 = matrix1/sum(matrix1) # normalize
  
  matrix2 = d2 %>%
    ungroup() %>%
    select(x, y, n) %>%
    spread(x, n) %>%
    mutate_each(funs(ifelse(is.na(.), 0, .))) %>%
    select(-y) %>%
    as.matrix(., rownames.force = NA)  %>%
    resize_mat(., c(dim, dim))
  
  matrix2 = matrix2/sum(matrix2)
  
  distance = emd2d(matrix1, matrix2) # earth mover distance
  
  data.frame(country1 = cntry1, 
             country2 = cntry2, 
             dist = distance, row.names = NULL)
}

######## define params ######## 
DIM = 20 # number of square the earth mover evaluates (DIM x DIM)
N_TOTAL = 1500 # minimum numer of drawings

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

######## define drawing files (by item) ######## 
file.list =  list.files("data/rdata/")

for (i in 1:length(file.list)){
    print(file.list[i])
    
    load(paste0("data/rdata/", file.list[i]))
    
    WORD = d$word[1]
    
    down.sampled.d = d %>%
      data.table() %>%
      .[country %in% big.countries] %>%
      group_by(country, key_id) %>%
      nest() %>%
      group_by(country) %>%
      sample_n(N_TOTAL) %>%
      unnest()
      
    # num counts in each square by x, y, country
    counts = down.sampled.d %>%
    count(x, y, country) %>%
    mutate(n = log(as.numeric(n))) # take log
    
    ######## Get movers distances between countries ###########
    
    # get rid of country combos not present for this item
    this.country.combo <- country.combo %>%
              filter(c1 %in% unique(counts$country) & c2 %in% unique(counts$country))
    
    distances = purrr::map2(this.country.combo$c1, this.country.combo$c2, 
                    get_movers, counts, DIM) %>%
                bind_rows()
    
    ##########Merge and write to data file################
    
    distances %<>% mutate(word = WORD) 
    
    write_csv(distances, paste0("data/emd_ds/", WORD, "_ds_emd.csv"))
  }
