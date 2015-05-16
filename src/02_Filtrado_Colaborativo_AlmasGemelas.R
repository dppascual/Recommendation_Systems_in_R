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

#Cargamos el fichero con mi rating de películas
ratings.own <- fread("data/films.eag", sep ="|", header = FALSE)
setnames(ratings.own, c("user", "item", "rating"))


###########################
# 
# Recomendador
#
###########################
# Para que una persona sea considerada mi alma gemela en cuestiones cinematograficas tiene que haber visto como minimo
# diez de mis peliculas y la distancia tiene que ser por lo menos de 0.7

soulmates <- function()
{  
  tmp <- merge(ratings.own, ratings, by = "item") 
  res <- tmp[, .(cosine.dist = cor(rating.x, rating.y), n = length(item)), by = c("user.y")]
  # Para buscar almas gemelas tenemos en cuenta el signo para ordenar
  setorder(res, cols = -cosine.dist, na.last = TRUE)
  return (res[n > 10 & cosine.dist > 0.7, .(user.y)])
}

# Se obtienen las almas gemelas
soulmates <- soulmates()      
setnames(soulmates, c("user"))

# Se obtienen las peliculas de mis almas gemelas
twinFilms <- merge(soulmates, ratings, by ="user")

# Se obtienen las peliculas recomendadas
recommendedFilms <- twinFilms[, .(n = .N, rating = (sum(rating)/.N)) , by = c("item")]
setorder(recommendedFilms, cols = -rating, na.last = TRUE)
recommendedFilms

# Para que una pelicula sea considerada una recomendación tiene que tener al menos 5 personas que la valoren
recommendedFilms[n > 5, head(as.character(sapply(item, function(x) films[films$item==x, movie_title])), 10)]
# El porcentaje de acierto es alto