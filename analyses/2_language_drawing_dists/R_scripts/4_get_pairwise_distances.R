# Get pairwise distances between target wrods

library(tidyverse)

MODEL_PATH <- "wiki.en.vec"
INPUT_PATH <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
OUTPUT_PATH <- "/Volumes/wilbur_the_great/CONCEPTVIZ/word_distances/word_dists_by_language.csv"
GOOGLE_LANGS <-  read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/langs_sampled.csv") %>%
  select(wiki_language_code) %>%
  unlist(use.names = F)

CATEGORY_NAMES <- read_csv("/Volumes/wilbur_the_great/CONCEPTVIZ/misc/google_categories_coded.csv")  %>%
  filter(critical == 1,
         category %in% c("F", "K", "A", "M")) %>% 
  select(google_category_name) %>%
  unlist(use.names = F)


## functions for getting clustering/overlap coefficent 
get_pairwise_dist_beween_words <- function(d){
  model_matrix <- d %>%
    select(-1) %>%
    as.matrix()
  
  words <- d$target_word
  word_word_dists <- philentropy::distance(model_matrix, 
                                           method = "cosine") %>%
    as.data.frame()  %>%
    mutate(w1 = words)
  
  colnames(word_word_dists) = c(words, "w1") 
  
  long_word_word_dists <- gather(word_word_dists, "w2", "cos_dist", -w1)
  
  # this is a data.table way of getting unique pairs that seems fast
  long_word_word_dists_f <-  unique(as.data.table(long_word_word_dists)[, c("w1", "w2") := list(pmin(w1, w2),pmax(w1, w2))], by = c("w1", "w2")) %>%
    filter(w1 != w2)
  
  long_word_word_dists_f
}


get_distances_for_crit_words <- function(language, model_path, out_path, critical_words){
  print(language)
  full_model_path <- paste0(model_path, language, ".vec")
  
  model <- fread( 
      full_model_path,
      header = FALSE,
      skip = 1,
      quote = "",
      encoding = "UTF-8",
      data.table = TRUE,
      col.names = c("target_word", 
                    unlist(lapply(2:301, function(x) paste0("V", x)))))

  model_filtered <- model[target_word %in% critical_words]

  dists <- get_pairwise_dist_beween_words(model_filtered) %>%
    mutate(language = language)
  
  write_csv(dists, out_path, append = T)
}

# do belarusian (#2) be lt sk uk
#GOOGLE_LANGS
walk(GOOGLE_LANGS, 
     get_distances_for_crit_words, 
     INPUT_PATH, 
     OUTPUT_PATH,
     CATEGORY_NAMES)

