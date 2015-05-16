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

tmp <- merge(ratings, films, by="item")
tmp[, ':=' (movie_title = NULL, release_date = NULL, video_release_date = NULL, IMDb_URL = NULL, date = NULL)]
# Se aplanan los datos en función del usuario, item y rating
tmp <- melt(tmp, id.vars = c("user", "item", "rating"))
# Eliminamos las filas con valor igual a cero (peliculas que no pertenecen a ese genero)
tmp <- tmp[tmp$value == 1,]
tmp[, value := NULL]
setnames(tmp, c("user", "item", "rating","genre"))


#####################
#
# Recomendador
#
#####################
convertNameToItem <- function(title)
{  
  if (is.character(title) & films[films$movie_title == title, sum(.N)] == 1) {
    return (films[films$movie_title==title, item])
  }
  else {
    return (title)
  }
}

recomienda.peliculas <- function(title, k)
{  
  film <- convertNameToItem(title)
  if (is.integer(film)) {
    tmp.0 <- tmp[tmp$item == film,]
    tmp.1 <- tmp[tmp$item != film,]
  
    tmp <- merge(tmp.0, tmp.1, by = "genre", allow.cartesian=TRUE)    
  
    correlations <- tmp[, .(cosine.dist = cor(rating.x, rating.y)), by = c("item.y","genre")]
    correlations <- correlations[order(-abs(correlations$cosine.dist)),]  
    #Devolvemos las k peliculas pedidas 
    print(head(correlations, k))
    afines <- sapply(head(unique(correlations$item.y), k), function(x) films[films$item==x, movie_title]) 
    return (afines)
  }
  else {
    return (c("Error: It isn't found the film that you indicated"))
  }
}

recomienda.peliculas("Cinema Paradiso (1988)", 10)

films[films$item==100,]
