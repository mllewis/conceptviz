###########Get linguistic distances ##########
#### wals_eucldean and asjp  
# these come from https://github.com/ddediu/lgfam-newick

library(tidyverse)
library(forcats)
library(feather)
library(lingtypology)

# get countries in our dataset
g_countries  <- read_csv("../../data/supplementary_data/cultural_sim_measures/geo/all_google_countries.csv")

# get language of eachcountry
country_lang <- read_csv("../../data/supplementary_data/cultural_sim_measures/lang/geo_cepii.csv") %>%
  rename(country_code = iso2,
         language = langoff_1) %>%
  select(country_code, language) %>%
  mutate(country_code = fct_recode(country_code,
                           "RS" = "YU")) %>%
  group_by(country_code) %>% 
  slice(1)

g_countries_lang <- g_countries %>%
  left_join(country_lang)  %>%
  mutate(language = replace(language, country_code == "IN", "Hindi")) 

### NOTE: need to get percentage of language in each country - here using official language which is weird for India. Change this manually
g_countries_lang_full <- g_countries_lang %>%
  mutate(language = fct_recode(language,
                           "Standard Arabic" = "Arabic",
                           "Croatian" = "Serbo-Croatian",
                           "Standard Malay" = "Malay",
                           "Mandarin Chinese" = "Chinese", 
                           "Belarusian" = "Belarussian",
                           "Modern Greek" = "Greek",
                           "Modern Hebrew" = "Hebrew",
                           "Central Khmer" = "Khmer")) %>%
  mutate(iso_lang = iso.lang(language))

load("../../data/supplementary_data/cultural_sim_measures/lang/wals-euclidean-mode-dm.RData")

wals_euclidean <- as.matrix(wals.euclidean.mode.dm)

g_indices <- which(dimnames(wals_euclidean)[[1]] %in% g_countries_lang_full$iso_lang)
g_wals_euclidean <- wals_euclidean[g_indices, g_indices] %>%
                      reshape2::melt() %>%
                      rename(lang1 = Var1,
                             lang2 = Var2,
                             wals_euclidean_dist = value) 

#### MG2015####
# load("../../data/supplementary_data/cultural_sim_measures/lang/MG2015-wals-alpha=0.69.RData")
# 
# mg_wals <- MG2015.WALS
# all_names <- dimnames(mg_wals)[[1]] %>%
#   lapply(function(x){str_extract(string = x, pattern = regex("(?<=\\[i-).*(?=\\]\\[w)"))}) %>%
#   unlist()
# 
# dimnames(mg_wals) = list(all_names, all_names)
# 
# g_indices_mg <- which(dimnames(mg_wals)[[1]] %in% g_countries_lang_full$iso)
# 
# mg_wals_dist <- mg_wals[g_indices_mg, g_indices_mg] %>%
#   reshape2::melt() %>%
#   rename(lang1 = Var1,
#          lang2 = Var2,
#          mg_dist = value) %>%
#   distinct() %>%
#   group_by(lang1, lang2) %>%
#   summarize(mg_dist = mean(mg_dist, na.rm = F)) %>%
#   filter(!is.na(mg_dist))

# there's very little variabiltiy - this doesn't seem that useful

#### asjp ####
load("../../data/supplementary_data/cultural_sim_measures/lang/asjp16-dists.RData")

asjp <- as.matrix(asjp16.dm)
g_indices_asjp <- which(dimnames(asjp)[[1]] %in% g_countries_lang_full$iso_lang)

g_asjp <- asjp[g_indices_asjp, g_indices_asjp] %>%
  reshape2::melt() %>%
  rename(lang1 = Var1,
         lang2 = Var2,
         asjp_dist = value) 

####autotype####
#load("../../data/supplementary_data/cultural_sim_measures/lang/autotyp-dist.RData")
# indicies don't have names

### write to file with country names ####
all_lang_dists <-  full_join(g_wals_euclidean, g_asjp) %>%
  left_join(g_countries_lang_full, by = c("lang1" = "iso_lang")) %>% 
  rename(country_code_1 = country_code) %>%
  left_join(g_countries_lang_full, by = c("lang2" = "iso_lang")) %>%
  rename(country_code_2 = country_code) %>%
  mutate(asjp_dist = ifelse(lang1 == lang2, 0, asjp_dist),# we want zero here for pairs dataset doesn't happen to have
         wals_euclidean_dist = ifelse(lang1 == lang2, 0, wals_euclidean_dist)) %>%
  select(country_code_1, country_code_2, 
         wals_euclidean_dist, asjp_dist) 

write_csv(all_lang_dists, "../../data/supplementary_data/cultural_sim_measures/lang/all_google_lang_dists.csv")
