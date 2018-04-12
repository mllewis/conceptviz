

### GROUND TRUTH
ground_truth <- jsonlite::stream_in(file(paste0("../../data/raw_data/simplified/", name)), simplifyMatrix = FALSE)


#### PROCESSED DATA
processed_data <- read_feather(paste0("../../data/raw_data/feathers/atleast_100/", "tree", ".txt")) %>%
  as.data.table()

KEY_ID ="5431688580562944"

# get meta-data in wide form
dc_wide <- ground_truth %>%
  filter(key_id %in% c(KEY_ID, "6037332891271168", "5431688580562944", "6077887180439552", "4607852117229568")) %>%
  mutate(drawing = lapply(drawing, lapply, function(x) { # bind together x and y values
    do.call(rbind, x)}), 
    n_strokes = unlist(lapply(drawing, length))) 

# get stroke number
dc_strokes <- dc_wide %>%
  rowwise() %>%
  do(stroke_lengths = unlist(lapply(.$drawing, function(x) dim(x)[2]))) %>%
  ungroup() %>%
  mutate(mean_length = unlist(lapply(stroke_lengths, function(x) {mean(unlist(x))}))) %>%
  cbind(dc_wide)

# get all data in long form
dc_coords <- dc_strokes %>%
  data.table() %>%
  group_by(key_id) %>%
  do(data.frame(t(as.matrix(data.frame(.$drawing))), # optimize here
                key_id = as.character(.$key_id[1]),
                word = as.character(.$word[1]),
                recognized = as.character(.$recognized[1]),
                n_strokes = .$n_strokes[1],
                mean_length = .$mean_length[1])) %>%
  bind_rows() %>%
  rename(x = X1,
         y = X2)

# bind in stroke numbers to long form
processed <- dc_coords %>%
  bind_cols(data.frame(stroke_num = unlist(lapply(dc_strokes$stroke_lengths, 
                                                  function(m){unlist(lapply(seq_along(m), 
                                                                            function(x){rep(x, m[x])}))}))))
## verify stork num
processed %>%
  filter(key_id == KEY_ID) %>%
  as.data.frame()

# ground_truth
m = ground_truth %>%
  filter(key_id == KEY_ID) %>%
  select(drawing) 
m$drawing


####CORRECTED####

KEY_ID = "6037332891271168"

# get meta-data in wide form
dc_wide <- ground_truth %>%
  filter(key_id %in% c(KEY_ID, "6037332891271168", "5431688580562944", "6077887180439552", "4607852117229568")) %>%
  mutate(drawing = lapply(drawing, lapply, function(x) { # bind together x and y values
    do.call(rbind, x)}), 
    n_strokes = unlist(lapply(drawing, length))) 

# get stroke number
dc_strokes <- dc_wide %>%
  rowwise() %>%
  do(stroke_lengths = unlist(lapply(.$drawing, function(x) dim(x)[2]))) %>%
  ungroup() %>%
  mutate(mean_length = unlist(lapply(stroke_lengths, function(x) {mean(unlist(x))}))) %>%
  cbind(dc_wide) %>%
  arrange(key_id)

# get all data in long form
dc_coords <- dc_strokes %>%
  data.table() %>%
  group_by(key_id) %>%
  do(data.frame(t(as.matrix(data.frame(.$drawing))), # optimize here
                key_id = as.character(.$key_id[1]),
                word = as.character(.$word[1]),
                recognized = as.character(.$recognized[1]),
                n_strokes = .$n_strokes[1],
                mean_length = .$mean_length[1])) %>%
  bind_rows() %>%
  rename(x = X1,
         y = X2) %>%
  arrange(key_id)

# bind in stroke numbers to long form
processed <- dc_coords %>%
  bind_cols(data.frame(stroke_num = unlist(lapply(dc_strokes$stroke_lengths, 
                                                  function(m){unlist(lapply(seq_along(m), 
                                                                            function(x){rep(x, m[x])}))}))))
## verify stork num
processed %>%
  filter(key_id == KEY_ID) %>%
  as.data.frame()

# ground_truth
m = ground_truth %>%
  filter(key_id == KEY_ID) %>%
  select(drawing) 
m$drawing

k = filter(processed_data, key_id ==  "6755389750116352")

m = processed_data %>%
  filter(key_id == "5431688580562944") %>%
  group_by(key_id) %>%
  slice(n()) %>%
  mutate(correct = n_strokes == stroke_num )

ggplot(k, aes(x = x, y = -y, color = as.factor(stroke_num))) +
  geom_point() +
  theme_minimal()+
  theme(legend.position = "none")
