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

#Ahora cargamos el fichero de los usuarios
users <- as.data.table(read.csv("data/ml-100k/u.user", sep ="|",header = FALSE))
setnames(users, c("user_id","age","gender","occupation","zip_code"))


#####################
#
# Recomendador
#
#####################

# Se filtran usuarios parecidos a mi
oldUsers <- users[gender == "M" & age > 28 & age < 32 & occupation == "engineer", .(user = user_id)]
oldFilms <- merge(oldUsers, ratings, by="user")

oldFilmsRating <- oldFilms[, .(n = .N, rating = sum(rating)/.N), by = c("item")]
table(oldFilmsRating$n)
setorder(oldFilmsRating, -rating)

# Para que una pelicula sea considerada una recomendación se establece que tenga como mínimo 5 personas que la valoren.
# Se muestran las 10 primeras recomendaciones
oldFilmsRating[n > 4, sapply(head(item, 10), function(x) films[films$item==x, movie_title])]

#####################
#
# CONCLUSIONES
#
####################

# RECOMENDACIONES AFINES
#[1] "Braveheart (1995)"                "Shawshank Redemption, The (1994)" "Raiders of the Lost Ark (1981)"  
#[4] "Silence of the Lambs, The (1991)" "Princess Bride, The (1987)"       "Die Hard (1988)"                 
#[7] "Usual Suspects, The (1995)"       "Star Wars (1977)"                 "Schindler's List (1993)"         
#[10] "Contact (1997)" 


# RECOMENDACIONES VIEJUNAS
#[1] "Fargo (1996)"                     "Raiders of the Lost Ark (1981)"   "Vertigo (1958)"                  
#[4] "Godfather, The (1972)"            "Star Wars (1977)"                 "Empire Strikes Back, The (1980)" 
#[7] "Pulp Fiction (1994)"              "Twelve Monkeys (1995)"            "Alien (1979)"                    
#[10] "Silence of the Lambs, The (1991)"

# Las recomendaciones son muy similares en ambos métodos. Tienen un porcentaje de acierto del 80-90% en las peliculas
# recomendadas.
