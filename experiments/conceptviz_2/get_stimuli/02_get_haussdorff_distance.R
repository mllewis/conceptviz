library(tidyverse)
library(feather)


get_hd_distance_fast <- function(id_1, id_2, long_data, py){

  d1 <- long_data %>%
    filter(key_id == id_1) %>%
    select(x_line, y_line) %>%
    distinct() %>%
    as.matrix()

  d2 <- long_data %>%
    filter(key_id == id_2) %>%
    select(x_line, y_line) %>%
    distinct() %>%
    as.matrix()

  hd_sim <- py$hausdorff_wrapper(d1, d2)

  data.frame(key_id_1 = id_1,
             key_id_2 = id_2,
             hd_sim = hd_sim)
}


ITEM <- "tree"
raw_data <- read_feather(paste0("../../../data/raw_data/feathers/atleast_100/",
                                ITEM, ".txt"))

# sample 10 drawings from each country (n = 71)
sample_ids <- raw_data %>%
  distinct(country, key_id) %>%
  group_by(country) %>%
  sample_n(10)

all_pair_combos <- combn(sample_ids$key_id, 2) %>%
  t() %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric) %>%
  filter(V1 < V2) # get all unique pairs %>%


sampled_pairs <- all_pair_combos %>%
  sample_n(10) %>%
  mutate_all(as.character)

unique_ids_in_pairs <- unique(sampled_pairs$V1, sampled_pairs$V2)

point_data <- raw_data %>%
  filter(key_id %in% unique_ids_in_pairs) %>%
  mutate(key_id_name = key_id) %>%
  rename(x_line = x, y_line = y) # this is dumb - it's just the name in the func

reticulate::use_python("/Users/mollylewis/anaconda2/bin/python2")
py2 <- reticulate::py_run_file("hausdorff_fast_wrapper.py")

hd_sims<- map2_df(sampled_pairs$V1,
                  sampled_pairs$V2,
                  get_hd_distance_fast,
                  point_data,
                  py2)

hd_this_path <- paste0("../../data/hausdorff_similarities/pair_sim_drawings/pair_lists/", ITEM, "_sampled_pairs_with_sims_hd.csv")
write_csv(hd_sims, hd_this_path)
```

