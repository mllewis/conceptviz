## Misc helper functions

get_image_from_google_draw <- function(data, 
                                        size1,
                                        size2,
                                        n_interpolate,
                                        return = "matrix",
                                        jpeg_size = NULL,
                                        jpeg_path = NULL){
  
  # for debugging
  # size1 = GDRAW_SIZE
  # size2= IMAGENET_SIZE
  # data = country_1_drawing
  
  key_id <- data$key_id[1]
  
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
  

    ## FIGURE OUT WHAT TO RETURN
    if (return == "matrix") {
      # make the drawing into a binary matrix
      mat <- array(0, c(size1, size1))
      mat[cbind(image_draw$x_line, image_draw$y_line)] <- 1 
      
      data.frame(list(mat))
    
    } else if (return == "long") {
      # return long form
      data.table(image_draw) %>%
          mutate(key_id = key_id) %>%
          select(key_id, everything())
    
    } else if (return == "jpeg") {
      # write to pdf
      
      file_name <- paste0(jpeg_path, key_id , ".jpeg")
      
      jpeg(file_name, width  = jpeg_size, height = jpeg_size, quality = 100)
        print(
          ggplot(image_draw, aes(x = x_line, y = y_line)) +
            geom_point(size = .5) +
            theme(line = element_blank(),
                  text = element_blank(),
                  title = element_blank())
        )
      dev.off()
    }
}
  
  
get_unique_relation_id <- function (x, y){
    pairs = c(x, y)
    ordered = order(pairs)
    paste0(pairs[ordered[1]], pairs[ordered[2]])
}

get_hd_distance <- function(id_1, id_2, long_data){
  
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
  
  hd_sim <- pracma::hausdorff_dist(d1, d2)
  
  data.frame(key_id_1 = id_1,
             key_id_2 = id_2, 
             hd_sim = hd_sim)
}

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

get_mhd_distance <- function(id_1, id_2, long_data, py, write, path){
  
  
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
  
   mhd_sim <- py$ModHausdorffDist(d1, d2)[1] %>%
              unlist()
   
   df <- data.frame(key_id_1 = id_1,
                    key_id_2 = id_2, 
                    mhd_sim = mhd_sim)
  
  if (write){
    write_csv(df, path, append = TRUE)
  } else {
    return(df)
  }
}

get_country_combos <- function(country_names){
  
  unique_country_combos <- combinat::combn(country_names, 2) %>%
    t() %>%
    as.data.frame() %>%
    rename(c_1 = V1,
           c_2 = V2) %>%
    mutate_all(as.character) %>%
    bind_rows(data.frame(c_1 = country_names,# (include within country, e.g. US_US)
                         c_2 = country_names))
  
  # make lists for looping over
  arg_list = list(unique_country_combos$c_1,
                  unique_country_combos$c_2)
  
  return(arg_list)
  
}





  