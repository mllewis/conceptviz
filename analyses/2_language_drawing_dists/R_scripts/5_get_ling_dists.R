###########Get linguistic distances ##########
#### wals_eucldean and asjp  
# these come from https://github.com/ddediu/lgfam-newick

library(tidyverse)
library(forcats)
library(feather)
library(lingtypology)

target_countries <- read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/langs_sampled.csv") %>%
  mutate_all(as.character)

### WALS ###
load("../../../data/supplementary_data/cultural_sim_measures/lang/wals-euclidean-mode-dm.RData")

wals_euclidean <- as.matrix(wals.euclidean.mode.dm)
g_indices <- which(dimnames(wals_euclidean)[[1]] %in% tolower(target_countries$wals_code))
g_wals_euclidean <- wals_euclidean[g_indices, g_indices] %>%
                      reshape2::melt() %>%
                      rename(lang_wals_1 = Var1,
                             lang_wals_2 = Var2,
                             wals_euclidean_dist = value)  %>%
                      left_join(target_countries %>% select(wals_code, iso_lang), 
                                by = c("lang_wals_1" = "wals_code")) %>%
                      rename(iso_lang1 = iso_lang) %>%
                      left_join(target_countries %>% select(wals_code, iso_lang), 
                                by = c("lang_wals_2" = "wals_code")) %>%
                      rename(iso_lang2 = iso_lang) %>%
                      mutate_if(is.character, as.factor)

  
#### asjp ####
load("../../../data/supplementary_data/cultural_sim_measures/lang/asjp16-dists.RData")

asjp <- as.matrix(asjp16.dm)
g_indices_asjp <- which(dimnames(asjp)[[1]] %in% target_countries$iso_lang)

g_asjp <- asjp[g_indices_asjp, g_indices_asjp] %>%
  reshape2::melt() %>%
  rename(iso_lang1 = Var1,
         iso_lang2 = Var2,
         asjp_dist = value)   %>%
  mutate_if(is.character, as.factor)

both_ling_dists <- full_join(g_asjp, g_wals_euclidean)

write_csv(both_ling_dists, "/Volumes/wilbur_the_great/CONCEPTVIZ/misc/wals_asjp_dists.csv")
