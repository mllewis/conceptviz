# keras misc

with kerasR- this seems inferior to keras
```{r}
reticulate::use_python("/usr/local/bin/python/") # this has to go before loading kerasR
library(kerasR)

keras_init()
inception <- InceptionV3(weights='imagenet')
img = load_img("/Users/mollylewis/Desktop/bernesepic.jpeg",
               target_size = c(299, 299))
x <- img_to_array(img)

x <- x / 255
pred <- keras_predict(inception, abind(x,x, rev.along = 4))
unlist(decode_predictions(pred, model = "InceptionV3", top = 5))

```

with keras - resnet
```{r}
library(keras)

mod <- application_resnet50(weights = 'imagenet')

img_path <- "/Users/mollylewis/Desktop/bernesepic.jpeg"
img <- image_load(img_path, target_size = c(224, 224))
x <- image_to_array(img)

# ensure we have a 4d tensor with single element in the batch dimension,
# the preprocess the input for prediction using resnet50
dim(x) <- c(1, dim(x))
x <- imagenet_preprocess_input(x)

preds <- mod %>% 
  predict(x)

imagenet_decode_predictions(preds, top = 3)[[1]]
```

with keras - v3
```{r}

modv3 <- application_inception_v3(weights = 'imagenet',
                                  include_top = TRUE)

img_path <- "/Users/mollylewis/Desktop/bernesepic.jpeg"
imgv3 <- image_load(img_path, target_size = c(299, 299))
x <- image_to_array(imgv3)

dim(x) <- c(1, dim(x))
x <- inception_v3_preprocess_input(x)

preds3 <- modv3 %>% 
  predict(x)

imagenet_decode_predictions(preds3, top = 5)
```
