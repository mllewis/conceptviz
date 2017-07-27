#### Sample and compute similarity for single item ######

### LOAD PACKAGES ###
library(tidyverse)
library(feather)
library(keras) 
library(data.table)

#### PARAMS #######
ITEM <- "tree"
RAW_PATH <- "../../data/raw_data/feathers/atleast_100/"
WRITE_PATH <- paste0("../../data/keras_similarities/pairwise_country/", ITEM, "_country_pairwise_sim.csv")
GDRAW_SIZE <- 255
IMAGENET_SIZE <- 224
N_INTERPOLATE <- 1000
N_SAMPLES_PER_COUNTRY_PAIRS <- 20
LAYER <- 'fc2'
CURRENT_ROW  <- 1

##### FUNCTIONS ######
get_pairwise_similarities <- function(country1, country2, loop_num, total_loops, d, model,
                                      gdraw_size, imagenet_size, n_interpolate, write_path){
  
  if(loop_num %% 50 == 0){  # print every n loops
    print(paste0(loop_num, "/", total_loops, " (", 
               round(loop_num/total_loops, 2) * 100, "%) complete"))
  }
  
  # DRAWING 1
  all_key_ids_1 <- d[country_code == country1, "key_id"]
  key_id_1 <- all_key_ids_1[sample(nrow(all_key_ids_1), 1)]
  country_1_drawing <- d[key_id == key_id_1]
  mat_1 <- get_imagenet_matrix_from_google_draw(country_1_drawing, gdraw_size, imagenet_size, n_interpolate)
  
  # DRAWING 2
  all_key_ids_2 <- d[country_code == country2, "key_id"]
  key_id_2 <- all_key_ids_2[sample(nrow(all_key_ids_2), 1)]
  country_2_drawing <- d[key_id == key_id_2]
  mat_2 <- get_imagenet_matrix_from_google_draw(country_2_drawing, gdraw_size, imagenet_size, n_interpolate)
  
  # GET COSINE SIMILARITY
  similarity <- get_sim(mat_1, mat_2, imagenet_size, model)

  # APPEND TO CSV
  write_csv(data.frame(loop_num, country1, country2, key_id_1, key_id_2, similarity), append = TRUE, write_path)
  
  }

get_imagenet_matrix_from_google_draw <- function(data, size1, size2, n_interpolate){
  
  # get long form coordinates with lines interpolated
  image_draw <- data %>%
    rename(x_end = x, y_end = y) %>%
    select(x_end, y_end, stroke_num) %>%
    mutate(x_start = lag(x_end),
           y_start = lag(y_end)) %>%
    mutate(transition = ifelse(stroke_num != lag(stroke_num), 
                               1, 0)) %>%
    filter(transition != 1) %>% # remove connections between strokes
    drop_na() %>%   # deals with first row
    mutate(x_end = ifelse(x_end == x_start, x_end + .00000001, x_end), # spline can't deal with x's being equal
           y_end = abs(y_end - size1), # we're flipping the image on the x axis
           y_start = abs(y_start - size1)) %>% 
    rowwise() %>%
    mutate(line = list(spline(c(x_start, x_end), # could also use approx function
                              c(y_start, y_end), n = n_interpolate)), 
           x_line = list(line$x),
           y_line = list(line$y)) %>%
    ungroup() %>%
    select(x_line, y_line) %>%
    unnest() %>%
    mutate_all(round) # necessary for indexing matrix
  
  
  # make the drawing into a binary matrix
  mat <- array(0, c(size1, size1))
  mat[cbind(image_draw$x_line, image_draw$y_line)] <- 1 
  
  data.frame(list(mat))
}
get_sim <- function(mat1, mat2, imagenet_size, model){
  
  ### MAT 1 ###
  # get matrix out of dataframe
  mat1 <- as.matrix(mat1) 
  
  # mutate matrix into imagenet size and preprocess
  dim(mat1) <- c(dim(mat1), 1, 1)
  mat1 <- imager::resize(mat1, imagenet_size, imagenet_size) %>%
    as.matrix()
  img_mat1 <- array(0, c(1, imagenet_size, imagenet_size, 3)) # [1, 1:224, 1:224, 1:3]
  img_mat1[ , 1:imagenet_size, 1:imagenet_size,] <- mat1
  
  # preprocess
  img_mat1 <- imagenet_preprocess_input(img_mat1)
  
  # get layer
  features_1 <- model %>% 
    predict(img_mat1) %>%
    as.vector()  # flatten to 1 x 25088 vector
  
  ### MAT 2 ###
  # get matrix out of dataframe
  mat2 <- as.matrix(mat2) 
  
  # mutate matrix into imagenet size and preprocess
  dim(mat2) <- c(dim(mat2), 1, 1)
  mat2 <- imager::resize(mat2, imagenet_size, imagenet_size) %>%
    as.matrix()
  img_mat2 <- array(0, c(1, imagenet_size, imagenet_size, 3)) # [1, 1:224, 1:224, 1:3]
  img_mat2[ , 1:imagenet_size, 1:imagenet_size,] <- mat2
  
  # preprocess
  img_mat2 <- imagenet_preprocess_input(img_mat2)
  
  # get layer
  features_2 <- model %>% 
    predict(img_mat2) %>%
    as.vector()  # flatten to 1 x 25088 vector
  
  # get distance measure
  cosine <- lsa::cosine(features_1, features_2)[1]
  
  cosine
}


#### GET THE MODEL ####
# get model with appropriate outputs
base_model <- application_vgg16(include_top = TRUE, 
                                weights = "imagenet")

layer_dict <- base_model$layers
names(layer_dict) <- purrr::map_chr(layer_dict ,~.x$name)

model <- keras_model(inputs = base_model$input, 
                     outputs = get_layer(base_model, LAYER)$output) # outputs


#### CONTROL FLOW ####
# get raw data for this item
all_data <- feather::read_feather(paste0(RAW_PATH, ITEM, ".txt")) %>%
  as.data.table()

# get pairs of countries for this item
all_countries <- unique(all_data$country_code)
country_pair_list <- gtools::combinations(n = length(all_countries), 
                                          r = 2,
                                          all_countries)
country_pair_expanded <- country_pair_list[rep(seq_len(nrow(country_pair_list)), 
                                               each = N_SAMPLES_PER_COUNTRY_PAIRS),] # the pairs are repeated the number of samples we want for that pair

# for keeping track of where we are
total_loops <- dim(country_pair_expanded)[1]

# full set of arguments to pwalk
country_pair_list_expanded <- list(country_pair_expanded[,1][CURRENT_ROW:total_loops], 
                                   country_pair_expanded[,2][CURRENT_ROW:total_loops], 
                                   (1:total_loops)[CURRENT_ROW:total_loops])

# do the thing (appends data to csv on each loop)
purrr::pwalk(country_pair_list_expanded, get_pairwise_similarities, total_loops, all_data, model, 
             GDRAW_SIZE, IMAGENET_SIZE, N_INTERPOLATE, WRITE_PATH) 

