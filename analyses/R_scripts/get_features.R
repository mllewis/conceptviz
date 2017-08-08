#### Sample and compute imagnet model features for single item ######

### LOAD PACKAGES ###
library(feather)
library(keras) 
library(data.table)
library(imager)
library(lsa)
library(tidyverse)

#### PARAMS #######
ITEM <- "bread"
RAW_PATH <- "../../data/raw_data/feathers/atleast_100/"
WRITE_PATH <- paste0("../../data/keras_similarities/features/", ITEM, "_country_features.csv")
GDRAW_SIZE <- 255
IMAGENET_SIZE <- 224
N_INTERPOLATE <- 1000
N_SAMPLES_PER_COUNTRY <- 50
LAYER <- 'fc2'
CURRENT_ROW <- nrow(read_csv(WRITE_PATH, col_names = FALSE)) + 1 

##### FUNCTIONS ######
get_features <- function(country1, loop_num, total_loops, d, model, item,
                                      gdraw_size, imagenet_size, n_interpolate, write_path){
  
  # country1 = "KR"
  # loop_num = 1
  # total_loops = 5
  # d = all_data
  # gdraw_size = GDRAW_SIZE
  # imagenet_size = IMAGENET_SIZE
  # n_interpolate = N_INTERPOLATE
  # write_path = WRITE_PATH
  
  if(loop_num %% 50 == 0){  # print every n loops
    print(paste0(loop_num, "/", total_loops, " (", 
               round(loop_num/total_loops, 2) * 100, "%) complete"))
  }
  
  # DRAWING 1
  all_key_ids_1 <- d[country_code == country1, "key_id"]
  key_id_1 <- all_key_ids_1[sample(nrow(all_key_ids_1), 1)]
  country_1_drawing <- d[key_id == key_id_1]
  mat_1 <- get_imagenet_matrix_from_google_draw(country_1_drawing, 
                                                gdraw_size, imagenet_size, n_interpolate)
  
  ### MAT 1 ###
  # get matrix out of dataframe
  mat_1 <- as.matrix(mat_1) 
  
  # mutate matrix into imagenet size and preprocess
  dim(mat_1) <- c(dim(mat_1), 1, 1)
  mat_1 <- imager::resize(mat_1, imagenet_size, imagenet_size) %>%
    as.matrix()
  img_mat_1 <- array(0, c(1, imagenet_size, imagenet_size, 3)) # [1, 1:224, 1:224, 1:3]
  img_mat_1[ , 1:imagenet_size, 1:imagenet_size,] <- mat_1
  
  # preprocess
  img_mat_1 <- imagenet_preprocess_input(img_mat_1)
  
  # get layer
  features_1 <- model %>% 
    predict(img_mat_1) %>%
    as.vector()  

  df <- cbind(item, loop_num, country1,  key_id_1, t(features_1))

  # APPEND TO CSV
  write_csv(df, append = TRUE, write_path)
  
  }

get_imagenet_matrix_from_google_draw <- function(data, size1, size2, n_interpolate){
  
  # for debugging
  # size1 = GDRAW_SIZE
  # size2= IMAGENET_SIZE
  # data = country_1_drawing

  # get long form coordinates with lines interpolated
  image_draw <- data %>%
    dplyr::rename(x_end = x, y_end = y) %>%
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
all_data <- read_feather(paste0(RAW_PATH, ITEM, ".txt")) %>%
  as.data.table()

# get pairs of countries for this item
all_countries <- unique(all_data$country_code)
all_countries_expanded <- rep(all_countries, each = N_SAMPLES_PER_COUNTRY) # the countries are repeated the number of samples we want for that pair

# for keeping track of where we are
total_loops <- length(all_countries_expanded)

# full set of arguments to pwalk
all_country_list_expanded <- list(all_countries_expanded[CURRENT_ROW:total_loops], 
                                   (1:total_loops)[CURRENT_ROW:total_loops])

# do the thing (appends data to csv on each loop)
purrr::pwalk(all_country_list_expanded, get_features, total_loops, all_data, model, 
             ITEM, GDRAW_SIZE, IMAGENET_SIZE, N_INTERPOLATE, WRITE_PATH) 

