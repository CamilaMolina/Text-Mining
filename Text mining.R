# Instalar packages para text mining ---------------------------------------------------------

#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("cluster")

# Cargar packages ---------------------------------------------------------

library(NLP) #lo pide el package "tm"
library(tm) #específico para text mining
library(SnowballC) 
library(RColorBrewer) #lo requiere "wordcloud"
library(wordcloud) #para graficar nubes de palabras
library(ggplot2) #una gramática de gráficas que expande las funciones base de R
library(dplyr) #con funciones auxiliares para manipular y transformar datos
#En particular, el operador %>% permite escribir funciones más legibles para seres humanos.
library(readr) #facilitará leer y escribir documentos
library(cluster) #con funciones para realizar análisis de grupos


# Cargar texto "Niebla" ---------------------------------------------------

#Empezaremos a leer el documento desde la línea 420, qué es donde termina el prólogo.
#Para ellos nos saltaremos 419 líneas previas, de manera complementaria, detendremos 
#la lectura en la línea 8313. Por tanto leemos un máximo de 8313.419 líneas.
#Asignamos el resultado al objeto nov_raw
nov_raw <- read_lines("49836-0.txt", skip = 419, n_max = 8313-419)


# Preparación del texto ---------------------------------------------------

#El objeto nov_raw que obtuvimos es uno de tipo character, con 7894 elementos
str(nov_raw)


# Creación de párrafos ----------------------------------------------------

#creamos un vector llamado diez con 10 repeticiones (rep) de los números desde 1 hasta el
#número de renglones en el documento, divido entre 10 (lenght(nov_raw)/10)
diez <- rep(1:ceiling(length(nov_raw)/10), each = 10)
#nos quedamos con un número de elementos igual al número de renglones del objeto
#now_raw (length(nov_raw)), para facilitar combinarlos.
diez <- diez[1:length(nov_raw)]
#combinamos diez con now_raw y los asignamos al objeto nov_text. Así tenemos una columna con los
#renglones de texto y otra con un número que identifica a qué grupo de diez renglones pertenece.
#además, convertimos a data.frame para que las columnas estén identificadas con un
#nombre, lo cual será útil en los siguientes pasos.
nov_text <- cbind(diez, nov_raw) %>% data.frame()
#usamos aggregate para concatenar los renglones (FUN=paste, con collapse="") para preservar el 
#espacio entre palabras), agrupados por diez (formula=nov_raw~diez)
nov_text <- aggregate(formula = nov_raw ~ diez,
                      data = nov_text,
                      FUN = paste,
                      collapse = " ")
#como necesitamos la columna con los ahora párrafos de texto, aprovechamos para transformar nov_text 
#en una matrix, pues esto nos facilitará los pasos siguientes.
nov_text <- nov_text %>% select(nov_raw) %>% as.matrix
dim(nov_text) #dimensión de la matriz


# También -----------------------------------------------------------------
#o también se puede realizar todo lo anterior en un único paso
nov_text <-
  cbind(
    rep(1:ceiling(length(nov_raw)/10), each = 10) %>%
      .[1:length(nov_raw)],
    nov_raw
  ) %>%
  data.frame %>%
  aggregate(
    nov_raw ~ V1,
    data = .,
    FUN = paste,
    collapse=" ") %>%
  select(nov_raw) %>%
  as.matrix

dim(nov_text) #dimensión de la matriz

# Limpieza de texto -------------------------------------------------------
#necesitamos limpiar el texto de caracteres que son de poca utilidad en nuestro text mining

#empezamos por asegurarnos que no hayan caracteres especiales de la codificación, como saltos de línea 
#y tabulaciones
nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
#convertimos todo a minúsculas
nov_text <- tolower(nov_text)
#usamos removeWords para eliminar palabras con poco valor de análisis, tales como algunas muletillas en español
nov_text <- removeWords(nov_text, words = stopwords("spanish"))
#nos desahecemos de la puntuación, ya que son identificadas como palabras diferentes
nov_text <- removePunctuation(nov_text)
#desahecmos los números, ya que no necesitamos fechas ni otras cantidades
nov_text <- removeNumbers(nov_text)
#eliminamos los espacios vacios excesivos, muchos de ellos introducidos por las transformaciones anteriores
nov_text <- stripWhitespace(nov_text)


# Analizar el documento ---------------------------------------------------
#Nuestro Corpus se compone de todos los parrafos del libro Niebla 
#nov_corpus <- Corpus(VectorSource(nov_text))
nov_corpus <- VCorpus(VectorSource(nov_text))
nov_corpus #nuestro Corpus está compuesto por 790 documentos.


# Nube de palabras --------------------------------------------------------
#mapareamos nuestro corpus como un documento plano usando las funciones tm_map y PlainTextDocument
nov_ptd <- tm_map(nov_corpus, PlainTextDocument)

#con nuestro corpus mapeado, podemos crear fácilmente una nube de palabras
wordcloud(nov_ptd, max.words = 80, 
          random.order = FALSE, 
          colors = brewer.pal(name = "Dark2", n = 8))
#There were 50 or more warnings (use warnings() to see the first 50)

# Nueva nube de palabras --------------------------------------------------
#como podemos observar aún se encuentran palabras con mucha frecuencia que en realidad no 
#son de mucha utilidad para el análisis de nuestro texto, por lo que haremos una segunda limpieza
nov_text <- removeWords(nov_text, words = c("usted", "pues", "tal", "tan", "así",
                                            "dijo", "cómo", "sino", "entonces", 
                                            "aunque", "don", "doña"))
nov_corpus <- nov_text %>% VectorSource() %>% VCorpus()
nov_ptd <- nov_corpus %>% tm_map(PlainTextDocument)

#las nubes de palabras deben tener distintos nombres

#generamos una nueva nube de palabras, en la cual es posible ver una diferencia significativa en su composición

wordcloud(nov_ptd, max.words = 80, 
  random.order = F,
  colors = brewer.pal(name = "Dark2", n = 8)
)


# Term Document Matrix ----------------------------------------------------

#mapearemos nuestro corpus indicando que es una matriz de términos, de esta manera podremos
#realizar operaciones como identificar asociaciones entre palabras y asignaremos el resultado al objeto nov_tdm
nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm #podemos considerar que tenemos 7236 palabras diferenetes en nuestro corpus


# Frecuencia de palabras --------------------------------------------------
#aunque una nube de palabras nos muestra de manera visual la frecuencia de palabras
#no nos devuelve cantidades

nov_mat <- as.matrix(nov_tdm) #objeto de clase matrix, que de nuevo tendrá un número de renglones igual al número de palabras distintas de nuestro corpus
dim(nov_mat)

#obtenemos la suma de renglones(rowSums) ordenadas de mayor a menor (sort con decreasing = TRUE) para conocer la frecuencia de cada palabra y después transformamos los resultados
#a objeto data.frame de dos columnas, palabra y frec, que nos permite graficar fácilmente su contenido
nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)

#con este objeto podemos crear una nube de palabras, aunque con argumentos un poco diferentes
wordcloud(
  words = nov_mat$palabra, 
  freq = nov_mat$frec, 
  max.words = 80, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

#podemos obtener las veinte palabras más frecuentes
nov_mat[1:20, ]


# Gráficas de frecuencia --------------------------------------------------

#creamos una gráfica donde podemos observar la frecuencia del uso de palabras
nov_mat[1:10, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes en Niebla",  x = "Palabras", y = "Número de usos")

#para obtener la misma información expresada anteriormente, pero expresada como proporción
#utilizamos la función mutate de dplyr para obtener el % de uso de c/palabra antes de graficar
nov_mat %>%
  mutate(perc = (frec/sum(frec))*100) %>%
  .[1:10, ] %>%
  ggplot(aes(palabra, perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "Diez palabras más frecuentes en Niebla", x = "Palabras", y = "Porcentaje de uso")
