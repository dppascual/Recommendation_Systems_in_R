library(reshape2)
library(data.table)


#############
#
# Preparación de datos
#
#############
#Cargamos los rating que han hecho los usuarios de las peliculas
ratings <- fread("data/ml-100k/u.data", sep = "\t")
setnames(ratings, c("user", "item", "rating","date"))

#Cargamos el listado de peliculas
genre <- read.csv("data/ml-100k/u.genre", sep = "|", header = FALSE)
films <- fread("data/ml-100k/u.item", sep='|', header = FALSE)
setnames(films, c("item","movie_title","release_date", "video_release_date", "IMDb_URL",as.character(genre$V1)))


###########################
# 
# Recomendador
#
###########################

# Se buscan las almas gemelas con la siguiente función:
soulmatesUser <- function(receivedUser)
{  
  tmp.0 <- ratings[user == receivedUser,] 
  tmp.1 <- ratings[user != receivedUser,]  
  tmp <- merge(tmp.0, tmp.1, by = "item")
  res <- tmp[, .(cosine.dist = cor(rating.x, rating.y), n = length(item)), by = c("user.y")]
  # Para buscar almas gemelas tenemos en cuenta el signo para ordenar
  setorder(res, cols = -cosine.dist, na.last = TRUE)
  return (res[n > 5 & cosine.dist > 0.7, .(user = user.y)])
}

# Función a la que le pasamos el usuario y la película que queremos quitar, y devuelve su estimación
EstimatedRating <- function(receivedUser, receivedItem){  
  dt.soulmatesUser <- soulmatesUser(receivedUser)
  masterRating <- ratings[!((item  == receivedItem) & (user == receivedUser)),]  
  masterRating <- masterRating[item  == receivedItem,] 
  # Al rating de la pelicula le doy la media de los ratings que le han otorgado los usuarios gemelos del nuestro.
  return(merge(dt.soulmatesUser, masterRating, by="user")[, .(rating = mean(rating)), by = c("item")])
}

#####################
#
# PRUEBAS REALIZADAS
#
####################
#Vamos a borrar el rating que ha dado el usuario 166 a la pelicula 346 (Jackie Brown (1997)).
# El rating Real es un 1, obtenemos un rating de 2 (aceptable)
EstimatedRating(166,346)

#Vamos a borrar el rating que ha dado el usuario 157 a la pelicula 274 (Sabrina (1995)).
# El rating Real es un 4, obtenemos un rating de 3,6 (aceptable)
EstimatedRating(157,274)

#Vamos a borrar el rating que ha dado el usuario 160 a la pelicula 234 (Jaws (1975)).
# El rating Real es un 5, obtenemos un rating de 5 (ajustado)
EstimatedRating(160,234)


#####################
#
# CONCLUSIONES
#
####################
# Asigna bastante bien el rating. Se detectan un problema principalmente:
#  1  Cuando las almas gemelas del usuario no han visto las películas, no se puede hacer estimación.



####################
#
# Se utiliza el paquete e1071 para rellenar los NA's
#
####################
library(e1071)

EstimatedRating1071 <- function(receivedUser, receivedItem){ 
  sampleRating <- copy(ratings)
  sampleRating[((item  == receivedItem) & (user == receivedUser)),]$rating <- NA
  imputeSampleRating <- as.data.table(impute(sampleRating))
  return(imputeSampleRating[item  == receivedItem & user == receivedUser, .(item, rating)])
}

#####################
#
# PRUEBAS REALIZADAS
#
####################
#Vamos a borrar el rating que ha dado el usuario 166 a la pelicula 346 (Jackie Brown (1997)).
# El rating Real es un 1, obtenemos un rating de 4 (desviado)
EstimatedRating1071(166,346)

#Vamos a borrar el rating que ha dado el usuario 157 a la pelicula 274 (Sabrina (1995)).
# El rating Real es un 4, obtenemos un rating de 4 (ajustado)
EstimatedRating1071(157,274)

#Vamos a borrar el rating que ha dado el usuario 160 a la pelicula 234 (Jaws (1975)).
# El rating Real es un 5, obtenemos un rating de 4 (aceptable)
EstimatedRating1071(160,234)


#####################
#
# CONCLUSIONES
#
####################
# Reemplazo de valores pobre al asignar siempre el mismo rating ya que calcula la media de las columnas respectivas.


#####################
#
# CONCLUSIONES FINALES
#
####################

# Existe otro paquete (missForest) de imputación de valores perdidos que predice con mayor exactitud que el utilizado con "impute", sin embargo, el tiempo de
# ejecución necesario para su predicción es elevado.
# Por ello, creo que el mejor método a utilizar, basándonos en resultado-tiempo, es el método basado en filtrado colaborativo.