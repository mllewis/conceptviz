# This takes in a bag of words across all essays (essay_word_counts.feather)
# and identifies the TOP_N most frequent words. It then uses the google tranlsate
# API to translate those words into all languages (GOOGLE_LANGS). Google charges 
# $20 more 1M characters.

library(feather)
library(tidyverse)
library(data.table)
library(googleLanguageR)
source("4b_essay_translation_helper.R")

####################### PARAMS ######################
gl_auth("L2ETS\ prompt\ translations-a8edd99b5aa9.json") # authenticate google api
OUTPUT_PATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/misc/target_word_translations.csv"

####################### SET LANGS ######################
GOOGLE_LANGS <-  read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/langs_sampled.csv") %>%
  select(wiki_language_code) %>%
  unlist(use.names = F)

################## GET TARGET WORDS ######################
CATEGORY_NAMES <- read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/google_categories_coded.csv")  %>%
  filter(critical == 1,
         category %in% c("F", "K", "A", "M")) %>% 
  select(google_category_name) %>%
  unlist(use.names = F)

################## GET ALL TRANSLATIONS ######################
lw_combos <- expand.grid(GOOGLE_LANGS, CATEGORY_NAMES) %>%
  rename(langs = Var1, words = Var2)

walk2(as.character(lw_combos$langs),  
                             as.character(lw_combos$words), 
                             get_essay_words_translation,
                             OUTPUT_PATH) 

# save to feather
temp <- read_csv(OUTPUT_PATH)
write_feather(temp, paste0(OUTPUT_PATH, ".feather"))

