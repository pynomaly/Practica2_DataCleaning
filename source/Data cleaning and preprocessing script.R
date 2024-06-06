

library(tidyverse) # Conjunto de paquetes para manejo de datos
library(magrittr) # Pipe
library(tidymodels) # Machine Learning en R
library(skimr) # Descriptivas univariadas masivas
library(rpart) # Arboles
library(rpart.plot)  # Graficar arboles
library(stringr)


data <- read.csv("../data/images_db.csv")

useful_columns <- c("search_topic", "page_num", "image_id","image_page", 
       "image_author", "image_title", "image_favs",
       "image_com", "image_views","private_collections", 
       "tags", "image_px", "image_size", "published_date")

data <- data %>% 
  dplyr::mutate(image_id = str_extract(image_page, "\\d+$") ) %>%
  dplyr::select(all_of(useful_columns)) 
