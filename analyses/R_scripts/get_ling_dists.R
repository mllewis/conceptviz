###########Get linguistic distances ##########
#### wals_eucldean and asjp  
# these come from https://github.com/ddediu/lgfam-newick

library(tidyverse)
library(forcats)
library(feather)
library(lingtypology)

# get countries in our dataset
#g_countries  <- read_csv(paste0("../../data/hausdorff_similarities/pair_lists/line_sampled_pairs_with_sims_hd.csv"))  %>%
#  select(country_code_1) %>%
#distinct()

g_countries <- data.frame(country_code = c("US", "NZ", "NL", "BR" ,"IT", "KR", "AR", "BG",
                 "NO", "AU" ,"DE" ,"GB" ,"HU" ,"PL", "SE", "AE", "SA", "PH",
                 "RS", "ID", "DK", "VN", "SG", "CA",
                 "CZ", "MY", "JP", "FR", "EE", "RU",
                 "QA", "TR", "TH", "IE" ,"FI", "HR",
                 "ES" ,"UA", "IL", "SK", "CL", "TW",
                 "PT" ,"RO", "IQ", "DZ", "IN", "KH",
                 "AT", "HK", "EG", "BE", "SI", "LT",
                 "ZA", "GR", "BY", "MX" ,"CH",
                 "CO" ,"KW", "PK" ,"LV" ,"KZ" ,"JO")) 

# get language of eachcountry
iso_codes <- read_csv("../../data/supplementary_data/cultural_sim_measures/lang/iso3_and_1.csv") %>%
  select(-English) %>%
  rename(iso3 = `alpha3-b`)


## coud do second and third languages too
g_countries_lang <- read_tsv("../../data/supplementary_data/cultural_sim_measures/lang/geo_codes.csv", skip = 50) %>%
  rename(country_code = `#ISO`,
         language = Languages,
         area = `Area(in sq km)`) %>%
  select(country_code, language, area) %>%
  filter(country_code %in% g_countries$country_code)  %>%
  separate(language, into = c("top_lang", "other_langs"), 
                             sep = ",") %>%
  separate(top_lang, into = c("top_lang", "other_langs2"), 
             sep = "-") %>%
  select(country_code, top_lang, area)%>%
  left_join(iso_codes, by = c("top_lang" = "alpha2")) %>%
  mutate(iso3 = replace(iso3, top_lang == "cmn", "cmn")) %>%
  select(-top_lang)

load("../../data/supplementary_data/cultural_sim_measures/lang/wals-euclidean-mode-dm.RData")

wals_euclidean <- as.matrix(wals.euclidean.mode.dm)

g_countries_lang$iso_lang_eu <- g_countries_lang$iso3 # need asjp-specifc codes
g_countries_lang[g_countries_lang$iso_lang_eu == "hrv","iso_lang_eu"] = "srb"
g_countries_lang[g_countries_lang$iso_lang_eu == "srp","iso_lang_eu"] = "srb"

g_indices <- which(dimnames(wals_euclidean)[[1]] %in% g_countries_lang$iso_lang_eu)
setdiff(g_countries_lang$iso_lang_eu,dimnames(wals_euclidean)[[1]])

g_wals_euclidean <- wals_euclidean[g_indices, g_indices] %>%
                      reshape2::melt() %>%
                      rename(lang1 = Var1,
                             lang2 = Var2,
                             wals_euclidean_dist = value)  %>%
  left_join(g_countries_lang %>% select(country_code, iso_lang_eu),
            by = c("lang1" = "iso_lang_eu")) %>% 
  rename(country_code_1 = country_code) %>%
  left_join(g_countries_lang %>% select(country_code, iso_lang_eu), 
            by = c("lang2" = "iso_lang_eu")) %>%
  rename(country_code_2 = country_code) %>%
  mutate_if(is.character, as.factor) %>%
  select(-lang1, -lang2)

write_csv(g_wals_euclidean, "wals_for_ETS.csv")

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

g_countries_lang$iso_lang_asjp <- g_countries_lang$iso3# need asjp-specifc codes
#g_countries_lang[g_countries_lang$iso_lang_asjp == "arb","iso_lang_asjp"] = "acm" # this is some dialect of arabic; doesn't seem to have macro
g_countries_lang[g_countries_lang$iso_lang_asjp == "ara","iso_lang_asjp"] = "acm" # this is some dialect of arabic; doesn't seem to have macro
g_countries_lang[g_countries_lang$iso_lang_asjp == "heb","iso_lang_asjp"] = "hbo" 
g_countries_lang[g_countries_lang$iso_lang_asjp == "lav","iso_lang_asjp"] = "lvs" # i think these are bcecause asjp uses 639-3, and the codes i have are 639-2 
g_countries_lang[g_countries_lang$iso_lang_asjp == "zsm","iso_lang_asjp"] = "ind"
g_countries_lang[g_countries_lang$iso_lang_asjp == "ger","iso_lang_asjp"] = "deu"
g_countries_lang[g_countries_lang$iso_lang_asjp == "dut","iso_lang_asjp"] = "nld"
g_countries_lang[g_countries_lang$iso_lang_asjp == "cze","iso_lang_asjp"] = "ces"
g_countries_lang[g_countries_lang$iso_lang_asjp == "est","iso_lang_asjp"] = "ekk"
g_countries_lang[g_countries_lang$iso_lang_asjp == "fre","iso_lang_asjp"] = "fra"
g_countries_lang[g_countries_lang$iso_lang_asjp == "gre","iso_lang_asjp"] = "ell"
g_countries_lang[g_countries_lang$iso_lang_asjp == "chi","iso_lang_asjp"] = "cmn"
g_countries_lang[g_countries_lang$iso_lang_asjp == "may","iso_lang_asjp"] = "ind"
g_countries_lang[g_countries_lang$iso_lang_asjp == "rum","iso_lang_asjp"] = "ron"
g_countries_lang[g_countries_lang$iso_lang_asjp == "slo","iso_lang_asjp"] = "slk"

g_indices_asjp <- which(dimnames(asjp)[[1]] %in% g_countries_lang$iso_lang_asjp)

g_asjp <- asjp[g_indices_asjp, g_indices_asjp] %>%
  reshape2::melt() %>%
  rename(lang1 = Var1,
         lang2 = Var2,
         asjp_dist = value)  %>%
  full_join(g_countries_lang, by = c("lang1" = "iso_lang_asjp")) %>%
  rename(country_code_1 = country_code) %>%
  full_join(g_countries_lang, by = c("lang2" = "iso_lang_asjp")) %>%
  rename(country_code_2 = country_code) %>%
  select(country_code_1, country_code_2, asjp_dist)


####autotype####
#load("../../data/supplementary_data/cultural_sim_measures/lang/autotyp-dist.RData")
# indicies don't have names

### write to file with country names ####
all_lang_dists <-  full_join(g_wals_euclidean, g_asjp, by = c('country_code_1', "country_code_2") )

write_csv(all_lang_dists, "../../data/supplementary_data/cultural_sim_measures/lang/all_google_lang_dists_clean.csv")
