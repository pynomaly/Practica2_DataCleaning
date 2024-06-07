# Practica2_DataCleaning
Práctica 2 de la asignatura "Tipología y ciclo de vida de los datos" del Máster de Ciencia de Datos (UOC). 

**Autores**: Esteban Braganza Cajas y Ana Álvarez Sánchez

## Objetivo del ejercicio
Nuestro objetivo es identificar y predecir los factores que determinan el número de favoritos que recibe cada imagen en DeviantArt. Buscamos entender las variables que explican esta interacción y cómo se relacionan con la popularidad de las imágenes.

Específicamente, queremos responder las siguientes preguntas:
1. ¿Qué características de las imágenes (como tamaño, resolución y número de comentarios) están más correlacionadas con el número de favoritos?
2. ¿Cómo influyen las variables temporales (como el día de la semana y el momento del día en que se publica una imagen) en la cantidad de favoritos?
3. ¿Qué temas de búsqueda son más propensos a recibir un alto número de favoritos?
4. ¿Existen patrones específicos en la comunidad de DeviantArt que puedan predecir la popularidad de una imagen?

Al entender estos factores, no solo podremos predecir mejor el número de favoritos que puede recibir una imagen, sino también ofrecer recomendaciones a los artistas sobre cómo optimizar sus publicaciones para aumentar su visibilidad y popularidad en la plataforma.

## Conjunto de datos utilizado
El [conjunto de datos utilizado](https://github.com/pynomaly/Practica2_DataCleaning/blob/main/data/images_db.csv) es el resultado del ejercicio de scraping de la web DeviantArt, que se puede encontrar en este [repositorio](https://github.com/EstebanBraganza77/Web-Scrapping-Practica1/tree/main). 

El fichero de datos contiene 7067 registros y 18 variables:
* `search_topic`: la imagen es resultado de la búsqueda por este tema
* `page_num`: la imagen aparece en este número de página de la búsqueda
* `image_page`: enlace a la página con la información de la imagen
* `image_url`: enlace a la imagen
* `image_title`: título de la imagen
* `image_author`: autor/a de la imagen
* `image_favs`: número de veces que le han dado a “me gusta” en la imagen
* `image_com`: número de comentarios que tiene la imagen
* `image_views`: número de vistas a la imagen
* `private_collections`: número de veces que ha sido incluida en una colección privada
* `tags`: etiquetas que se le han asignado a la imagen para facilitar su descubrimiento
* `location`: país o localización geográfica, si el autor la quiere identificar * * `description`: campo de texto abierto creado por el autor, que acompaña a la imagen. Puede incluir detalles técnicos o enlaces a las redes sociales del autor/a.
* `image_px`: dimensiones de la imagen, en pixeles
* `image_size`: peso de la imagen en MB.
* `published_date`: fecha de publicación de la imagen.
* `last_comment`: último comentario añadido a la imagen.
* `license`: licencia de la imagen

El ejercicio incluye la integración y selección de los datos, la limpieza de los datos, el análisis de los datos, incluyendo un modelo supervisado, un modelo no supervisado y un contraste de hipótesis, la representación de los resultados y la resolución del problema.

El código se puede encontrar en [RMarkdown](https://github.com/pynomaly/Practica2_DataCleaning/blob/main/source/PRACTICA_2.Rmd) y un [script de R](https://github.com/pynomaly/Practica2_DataCleaning/blob/main/source/PRACTICA_2.R).

# Licencia
Este proyecto se encuentra bajo licencia [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/legalcode), que permite la redistribución y reutilización de una obra con la condición de que el creador reciba el crédito adecuado.
