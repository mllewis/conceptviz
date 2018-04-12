## Get languages corresponding to  each country
# because mutliple countries speak each languagem
# i'm downsampling to only represent each language by a single counytr


library(tidyverse)
library(forcats)
library(feather)


OUTPATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/misc/langs_sampled.csv"
GOOD_COUNTRIES  <- read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/good_country_list.csv") %>%
  select(countrycode)

CL_MAPPINGS  <- read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/geo_cepii.csv") 

langs <- left_join(GOOD_COUNTRIES, CL_MAPPINGS, by = c("countrycode" = "iso2")) %>%
  group_by(countrycode) %>%
  select(countrycode, country, langoff_1, iso3) %>%
  distinct()
  
langs_sampled <- langs %>%
  filter(!is.na(langoff_1)) %>%
  group_by(langoff_1) %>%
  sample_n(1)  

write_csv(langs_sampled, OUTPATH)

