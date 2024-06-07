

# Importamos librerias de analisis de datos
library(tidyverse) # Conjunto de paquetes para manejo de datos
library(magrittr) # Pipe
library(dplyr)
library(recipes)
library(tidymodels) # Machine Learning en R
library(skimr) # Descriptivas univariadas masivas
library(ggplot2)
library(rpart) # Arboles
library(rpart.plot) # Graficar arboles

# Install rstudioapi if not already installed
if (!require("rstudioapi")) install.packages("rstudioapi")

# Set the working directory to the location of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("../data/images_db.csv")

# Search Topic

frecuencias <- table(data$search_topic)
df_frecuencias <- as.data.frame(frecuencias)
df_frecuencias_ordenado <- df_frecuencias[order(-df_frecuencias$Freq), ]

ggplot(head(df_frecuencias_ordenado, 15), aes(x = reorder(Var1, -Freq), y = Freq, fill = Freq)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Topics por Frecuencia",
       x = "Search Topic",
       y = "Frequencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Page num

ggplot(data, aes(x = page_num)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Page Numbers",
       x = "Page Number",
       y = "Frequency") +
  theme_minimal()


# Autores 
frecuencias <- table(data$image_author)
df_frecuencias <- as.data.frame(frecuencias)
df_frecuencias_ordenado <- df_frecuencias[order(-df_frecuencias$Freq), ]

ggplot(head(df_frecuencias_ordenado, 15), aes(x = reorder(Var1, -Freq), y = Freq, fill = Freq)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top Autores por Frequencia",
       x = "Author",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Image favs

ggplot(data, aes(x = image_favs)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de número de favoritos",
       x = "Número de favoritos",
       y = "Frequency") +
  theme_minimal()


##### Image coms #########

ggplot(data, aes(x = image_com)) +
  geom_histogram(fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribución de comentarios",
       x = "Número de Comentarios",
       y = "Frecuencia") +
  theme_minimal()


ggplot(data, aes(x = image_com)) +
  geom_boxplot(fill = "darkblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de comentarios",
       x = "Número de Comentarios",
       y = "Frecuencia") +
  theme_minimal()

# image views

ggplot(data, aes(x = image_views)) +
  geom_histogram(fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribución de número de vistas",
       x = "Número de Vistas",
       y = "Frecuencia") +
  theme_minimal()

ggplot(data, aes(x = image_views)) +
  geom_boxplot(fill = "darkblue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot de número de vistas",
       x = "Número de Vistas",
       y = "Frecuencia") +
  theme_minimal()

# private collections

ggplot(data, aes(x = private_collections)) +
  geom_histogram(fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribución de colecciones privadas",
       x = "Número de colecciones privadas",
       y = "Frecuencia") +
  theme_minimal()

ggplot(data, aes(x = private_collections)) +
  geom_boxplot(fill = "darkblue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot de colecciones privadas",
       x = "Número de colecciones privadas",
       y = "Frecuencia") +
  theme_minimal()

# image size
ggplot(data, aes(x = image_size)) +
  geom_histogram(fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribución de tamaño de imagen",
       x = "Tamaño de la imagen según image_size",
       y = "Frecuencia") +
  theme_minimal()

# image px

frecuencias <- table(data$image_px)
df_frecuencias <- as.data.frame(frecuencias)
df_frecuencias_ordenado <- df_frecuencias[order(-df_frecuencias$Freq), ]

ggplot(head(df_frecuencias_ordenado, 20), aes(x = reorder(Var1, -Freq), y = Freq, fill = Freq)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Píxeles de imágenes",
       x = "Píxeles",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Exploración de la frecuencia de los tags más comunes

# Función para limpiar y convertir las cadenas de tags en listas de tags
clean_and_split_tags <- function(tags_string, position) {
  # Elimina corchetes y comillas
  tags_string <- gsub("\\[|\\]|'", "", tags_string)  
  if (tags_string == "") {
    # Devuelve un vector vacío si la cadena está vacía
    return(NA)  
  }
  strsplit(tags_string, ", ")[[1]][position]  # Dividir la cadena en una lista de tags
}

# Aplicar la función a la columna de tags
#data$tags_list <- lapply(data$tags, clean_and_split_tags, 1)


for (i in 1:5) {
  # Crear lista total de tags
  all_tags <- unlist(lapply(data$tags, clean_and_split_tags, i))
  
  # Crear una tabla de frecuencias
  tag_frequencies <- table(all_tags)
  df_frecuencias <- as.data.frame(tag_frequencies)
  df_frecuencias_ordenado <- df_frecuencias[order(-df_frecuencias$Freq), ]
  
  p<-ggplot(head(df_frecuencias_ordenado, 50), aes(x = reorder(all_tags, -Freq), y = Freq, fill = Freq)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = paste("Tags en posición", i),
         x = "Tags",
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
}

# Selección de variables importantes 

data <- data.frame(lapply(data, function(x) {
  x[x == ""] <- NA
  return(x)
}), stringsAsFactors = FALSE)

colSums(is.na(data))

# Preprocesado de datos para dejarlos en un formato más útil que al inicio.
data_parsed <- data %>%
  mutate(image_id = str_extract(image_page, "\\d+$"),
        search_topic = as.factor(search_topic),
         published_date = as_datetime(published_date),
         year = year(published_date),
         month= month(published_date),
         day_of_week =as.factor(weekdays(published_date)),
         hour = hour(published_date),
         moment_of_day = as.factor(case_when(
           hour >= 5 & hour < 12 ~ "Morning",
           hour >= 12 & hour < 17 ~ "Afternoon",
           hour >= 17 & hour < 21 ~ "Evening",
           hour >= 21 | hour < 5 ~ "Night"
         )),
         image_license = as.factor(ifelse(grepl("©", data$image_license), 
                                          "Copyright", image_license)),
         image_px = ifelse(grepl("x", image_px), image_px, NA),
         superficie_px2 =sapply(strsplit(as.character(image_px), "x"), 
                                function(dim) as.numeric(dim[1]) * as.numeric(dim[2])),
         image_size_kb = ifelse(
           nchar(gsub("\\..*", "", image_size)) < 3, 
           image_size * 1024,  
           image_size ),
         tags_list_1 = lapply(tags, clean_and_split_tags, 1),
         image_favs_cat = cut(image_favs, 
                              breaks = quantile(image_favs, 
                                                probs = seq(0, 1, by = 0.25)), 
                              labels = c("Very Low", "Low", "Medium", "High"), 
                              include.lowest = TRUE)
  ) %>% select(
    all_of(c("search_topic", 
             "year", 
             "month", 
             "hour",
             "day_of_week",
             "moment_of_day",
             "image_license", 
             "superficie_px2", 
             "image_size_kb", 
             "image_com", 
             "image_views",
             "page_num",
             "private_collections", 
             "image_favs_cat", 
             "image_favs")
    )
  ) %>% na.omit()

# Receta del preprocesamiento de las variables resultantes del data frame.
recipe <- data_parsed %>%
  recipe(image_favs_cat ~ . ) %>% ## Crea la receta
  step_naomit(all_predictors(), -all_outcomes()) %>%  # Elimina filas con NA
  step_log(c("superficie_px2", "image_size_kb", 
             "image_com", "image_views", 
             "image_favs"), offset = 1) %>%  
  step_normalize(c("superficie_px2", "image_size_kb", 
                   "image_com", "image_views", "image_favs"))  

train <- prep(recipe, data_parsed)

data_preprocessed <- bake(train, data_parsed)

# Save the dataframe to a CSV file
write.csv(data_preprocessed, file = "../data/images_db_preprocessed.csv", row.names = FALSE)

#  Partición de los datos

library(caret)
library(randomForest)

set.seed(123)

trainIndex <- createDataPartition(data_parsed$image_favs_cat, p = .8, list = FALSE, times = 1)

data_train <- bake(train, new_data = data_parsed[trainIndex,])
data_test <- bake(train, new_data = data_parsed[-trainIndex,])

# Eliminar las variables de texto y la numérica de image_favs
selected_vars = c('image_favs_cat', 'search_topic', 'page_num', 'image_com', 'image_views', 'image_size_kb', 'published_date', 'image_license',"year", "month", "day_of_week", "moment_of_day", "private_collections", "superficie_px2")

data_train <- data_train[ , (names(data_train) %in% selected_vars)]
data_test <- data_test[ , (names(data_test) %in% selected_vars)]

# Eliminar filas con valores perdidos en data_train y data_test
data_train <- data_train[complete.cases(data_train), ]
data_test <- data_test[complete.cases(data_test), ]


# Ver los rangos de cada categoría

data_train %>%
  group_by( image_favs_cat) %>%
  count( name = 'frec') %>%
  ungroup() %>%
  mutate( Porc= frec/sum(frec)) %>%
  ggplot( aes(x= image_favs_cat, y= Porc)) +
  geom_segment( aes(xend= image_favs_cat, y=0, yend=Porc),
                color= "steelblue", linewidth= 1) +
  geom_point( size=5, color= "steelblue") +
  coord_flip() +
  scale_y_continuous( labels = percent_format()) +
  labs(title= 'Porcentaje de Imágenes por nivel de favoritos',
       y= "Porcentaje", x= "nivel favoritos") +
  theme_bw()

data_train %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot(
    method = "number", 
    type = "lower", 
    tl.col = "black", 
    tl.srt = 45, 
    number.cex = 0.75, 
    cl.pos = "b", 
    addCoef.col = "black",
    title = "Correlation Matrix",
    mar = c(0,0,2,0)
  )

# Crear los scatter plots
plot1 <- ggplot(data_train, aes(x = image_size_kb, y = superficie_px2, color = image_favs_cat)) +
  geom_point() +
  labs(title = "Scatter Plot 1: image_size_kb vs superficie_px2",
       x = "image_size_kb",
       y = "superficie_px2",
       color = "image_favs_cat")

plot2 <-
  ggplot( data_train %>% filter(image_views < max(image_views)), aes(x = image_com, y = image_views, color = image_favs_cat)) +
  geom_point() +
  labs(title = "Scatter Plot 2: image_com vs image_views",
       x = "image_com",
       y = "image_views",
       color = "image_favs_cat")

# Crear un grid de los scatter plots
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 1, nrow=2)

# KDE plot 1: image_size_kb vs superficie_px2
kde_plot1 <- ggplot(data_train, aes(x = image_size_kb, fill = image_favs_cat, color = image_favs_cat)) +
  geom_density(alpha = 0.5) +
  labs(title = "KDE Plot 1: image_size_kb",
       x = "image_size_kb",
       y = "Density",
       fill = "image_favs_cat",
       color = "image_favs_cat")

kde_plot2 <- ggplot(data_train, aes(x = superficie_px2, fill = image_favs_cat, color = image_favs_cat)) +
  geom_density(alpha = 0.5) +
  labs(title = "KDE Plot 2: superficie_px2",
       x = "superficie_px2",
       y = "Density",
       fill = "image_favs_cat",
       color = "image_favs_cat")

# KDE plot 3: image_com vs image_views
kde_plot3 <- ggplot(data_train, aes(x = image_com, fill = image_favs_cat, color = image_favs_cat)) +
  geom_density(alpha = 0.5) +
  labs(title = "KDE Plot 3: image_com",
       x = "image_com",
       y = "Density",
       fill = "image_favs_cat",
       color = "image_favs_cat")

kde_plot4 <- ggplot(data_train, aes(x = image_views, fill = image_favs_cat, color = image_favs_cat)) +
  geom_density(alpha = 0.5) +
  labs(title = "KDE Plot 4: image_views",
       x = "image_views",
       y = "Densidad",
       fill = "image_favs_cat",
       color = "image_favs_cat")

# Crear un grid de los KDE plots
grid.arrange(kde_plot1, kde_plot2, kde_plot3, kde_plot4, nrow = 4)


# Bar plot for day_of_week
bar_plot1 <- ggplot(data_train, aes(x = day_of_week, fill = image_favs_cat)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot: Day of Week by image_favs_cat",
       x = "Day of Week",
       y = "Count",
       fill = "image_favs_cat")

# Bar plot for moment_of_day
bar_plot2 <- ggplot(data_train, aes(x = moment_of_day, fill = image_favs_cat)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot: Moment of Day by image_favs_cat",
       x = "Moment of Day",
       y = "Count",
       fill = "image_favs_cat")

# Bar plot for search_topic
bar_plot3 <- ggplot(data_train, aes(x = search_topic, fill = image_favs_cat)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot: Search Topic by image_favs_cat",
       x = "Search Topic",
       y = "Count",
       fill = "image_favs_cat") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for search_topic
bar_plot4 <- ggplot(data_train, aes(x = page_num, fill = image_favs_cat)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot: Page num por image_favs_cat",
       x = "Page num",
       y = "Count",
       fill = "image_favs_cat") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for search_topic
bar_plot5 <- ggplot(data_train, aes(x = month, fill = image_favs_cat)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot: Mes por image_favs_cat",
       x = "mes",
       y = "Count",
       fill = "image_favs_cat") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for search_topic
bar_plot6 <- ggplot(data_train, aes(x = private_collections, fill = image_favs_cat)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot: Private collections por image_favs_cat",
       x = "private_collections",
       y = "Count",
       fill = "image_favs_cat") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Crear un grid de los bar plots
grid.arrange(bar_plot1, bar_plot2, nrow = 2)
print(bar_plot3)
print(bar_plot4)
print(bar_plot5)
print(bar_plot6)

# Modelo supervisado: Random Forest
set.seed(42)
rf_model <- randomForest(image_favs_cat ~ ., data = data_train, importance = TRUE)

# Predicciones
predictions <- predict(rf_model, data_test)

# Evaluar el modelo
conf_matrix <- confusionMatrix(predictions, data_test$image_favs_cat)

# Mostrar resultados
print(conf_matrix)

# Importancia de las variables
importance(rf_model)

# modelo no supervisado usando clustering

data_kmeans <- bake(train, new_data = data_parsed)

# Seleccionamos variables numericas para encontrar grupos de imagenes basadas en sus el numero de favoritos, de comentarios, de vistas, el peso y la resolucion.


selected_vars = c(
  'image_com', 'image_views', 
  'image_size_kb',"superficie_px2", "image_favs")

data_kmeans <- data_kmeans %>% select(all_of(selected_vars))


# Metodo del Codo
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(data_kmeans, i)
  resultados[i] <- fit$tot.withinss
}
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Número de clusters",ylab="tot.tot.withinss")

# Metodo de la silueta
if (!require('fpc')) install.packages('fpc')
library(fpc)
fit_asw <- kmeansruns(data_kmeans, krange = 1:10, criterion = "asw") 
plot(1:10,fit_asw$crit,type="o",col="blue",pch=0,xlab="Número de clústers",
     ylab="Criterio silueta media")

# Aplicar el algoritmo k-means
set.seed(123)  # Para reproducibilidad
k <- 4 # Número de clústeres
kmeans_res <- kmeans(data_kmeans, centers = k)


# bill_lLength y bill_depth
plot(data_kmeans[c(4,5)], col=kmeans_res$cluster, main="Clasificación k-means")

# Calcular las medias de las variables seleccionadas por clúster
means_by_cluster <- aggregate(data_kmeans, by = list(kmeans_res$cluster), FUN = mean)

# Convertir el dataframe de medias por cluster en formato largo
means_by_cluster_long <- tidyr::pivot_longer(means_by_cluster, cols = -Group.1, names_to = "Variable", values_to = "Media")

# Crear un gráfico de barras con las medias por clúster
ggplot(means_by_cluster_long, aes(x = Variable, y = Media, fill = factor(Group.1))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Medias de variables seleccionadas por clúster",
       x = "Variables", y = "Media",
       fill = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(k)) +
  coord_flip()


# Crear una tabla de contingencia
contingency_table <- table(data_parsed$search_topic, data_parsed$image_favs_cat)

# Realizar el test de Chi-cuadrado
chi_square_test <- chisq.test(contingency_table)

# Resumen del test de Chi-cuadrado
print(chi_square_test)
contingency_table
