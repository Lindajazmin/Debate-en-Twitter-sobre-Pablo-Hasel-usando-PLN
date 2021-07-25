

### Carga de Librerías ####
library(rtweet)
library(tidyverse)
library(lubridate)
library(wordcloud2)
library(RColorBrewer)
library(ggwordcloud)
library(gridExtra)
library(scales)
library(tidytext)
library(igraph)
library(ggraph)
library(quanteda)
library(e1071)
library(ggjoy)
library(dplyr)
library(cluster)
library(tm)
library(knitr)
library(tidytext)
library(stringi)
library(readxl)
library(SnowballC)
library(udpipe)
library(rvest)

library(RColorBrewer)
display.brewer.all()

## store api keys (these are fake example values; replace with your own keys)
consumer_key = '9kQFOxcbK13lTvZ3okc'
consumer_secret = 'idDJwClQXBtYEOasHh9JiSO1ucWmdnCOdtMGXTPtK0bqf'
access_token = '1351322832354107395-8SLxHYI4zMsVCxNKL0w9TQe'
access_secret = 'JPtPP6q3NJO5Mv394bR8jPyj8oEva8N2L5'

## authenticate via web browser
token <- create_token(
    app = "TallerSee",
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret)

phasel2_df <- read_rds('phasel2_df.rds')   ## se carga el archivo guardado

colnames(phasel2_df)


## Días de posteo ###

phasel2_df %>% mutate(created_at = floor_date(as.Date(created_at)),"hour")%>% 
    group_by(created_at) %>%
    summarise(total = n()) %>% 
    ggplot(aes(created_at,total,label=total,fill = total))+
    geom_bar(stat="identity")+ 
    geom_text(aes(label=total), vjust= 1.5, colour="white", size=2.5)+
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 13)) +
    theme(axis.text = element_text(size=10))+
    labs(
        x = NULL, y = NULL,
        title = "Figura 1: Tweets posteados por días desde el 19 al 28 de febrero del 2021",
        subtitle = paste0(nrow(phasel2_df)," mensajes analizados"),
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )

## Horas de posteo ###

phasel2_df %>% mutate(created_at = hour(created_at)) %>%  
    group_by(created_at) %>% summarise(total=n()) %>% 
    ggplot(aes(created_at,total,label=total,fill=total))+
    geom_bar(stat="identity")+
    theme_bw() + geom_label( colour = "white")+
    theme(plot.title = element_text(face = "bold", size = 13)) +
    theme(axis.text = element_text(size=10))+
    scale_x_discrete(name ="Horas", 
                     limits=seq(0,23))+
    labs(
        x = NULL, y = NULL,
        title = "Figura 2: Tweets por Hora de posteo (Madrid/España)",
        subtitle = paste0(nrow(phasel2_df)," mensajes analizados"),
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )


## cuentas que mas postean sobre Hasél

phasel2_df %>% group_by(screen_name) %>% summarise(total=n()) %>% arrange(desc(total)) %>% top_n(10) %>%
    ggplot(aes(reorder(screen_name,total),total,label=total,fill=total))+
    geom_bar(stat="identity")+ coord_flip() +
    scale_fill_brewer(palette="Spectral") +
    theme_minimal() + geom_label( colour = "white")+
    theme(plot.title = element_text(face = "bold", size = 13)) +
    theme(axis.text = element_text(size=10))+
    labs(
        x = NULL, y = NULL,
        title = "Figura 3: TOP 10 - Cuentas que más postean sobre Pablo Hasél",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )


#Grafo de Relaciones
from_to_ph <- phasel2_df %>%
    mutate(menciones=purrr::map(.x=text,
                                pattern='@\\w+',
                                .f=str_extract_all)) %>% 
    dplyr::select(screen_name,menciones,created_at) %>% 
    mutate(nombre_usuario=str_to_lower(paste0('@',screen_name))) %>% 
    unnest(menciones) %>% 
    unnest(menciones) %>% 
    mutate(menciones=str_to_lower(menciones))


### Cuenta más mencionada  MADRID #####
menciones <- from_to_ph %>% group_by(menciones) %>% 
    summarise(total = n()) %>% arrange(desc(total))


menciones %>% top_n(5) %>% arrange(desc(total)) %>% 
    ggplot(aes(reorder(menciones,total),total,label=total,fill = total))+
    geom_bar(stat="identity")+ 
    geom_text(aes(label=total), hjust=1.5, colour="white", size=3)+
    theme_minimal() +coord_flip()+
    theme(plot.title = element_text(face = "bold", size = 13)) +
    theme(axis.text = element_text(size=8))+
    labs(
        x = NULL, y = NULL,
        title = "Figura 4: TOP 5 - Cuentas más mencionadas",
        subtitle = "En los tweets hablando de Pablo Hasel",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )



### Agrupo y cuento cuantas interaciones tienen cada par de usuarios ####
grafo <- from_to_ph %>% dplyr::select(nombre_usuario,menciones) %>% 
    group_by(nombre_usuario,menciones) %>% 
    summarise(n=n()) %>% arrange(desc(nombre_usuario,n)) 

### Aqui selecciono las veces que ha interactuado las cuentas
grafo <- grafo %>% filter(n>50)

#### Creo el grafo ######
BP_graph <- graph_from_data_frame(d = grafo,directed = TRUE)

## Gráfico Final
BP_graph %>%
    ggraph() +
    geom_edge_link(arrow = arrow(type = "closed", length = unit(1.5, "mm")),
                   aes(end_cap = label_rect(node2.name))) +
    geom_node_label(aes(label = name)) +
    theme(plot.title = element_text(face = "bold", size = 13)) +
    theme(axis.text = element_text(size=8))+
    theme_graph()+ 
    labs(
        x = NULL, y = NULL,
        title = "Figura 5: Red de interacciones entre usuarios",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )



### TOKENIZACIÓN

tidy_phasel <- phasel2_df %>% 
    unnest_tokens(output = Token, input = text)

tidy_ph <- ph_lemma %>% 
  unnest_tokens(output = Token, input = text)

ejemplo_phasel = phasel2_df[115, ]

ejemplo_phasel$text


#### Eliminación de símbolos y palabras no utilizadas ####

colnames(tidy_phasel)

tidy_ph2 <- tidy_ph %>% 
    mutate(Token = removePunctuation(Token)) %>% 
    View()

tidy_ph2 <- tidy_ph2 %>% 
    mutate(Token = removeNumbers(Token)) %>% 
    View()

tidy_ph2 <- tidy_ph2 %>% 
    mutate(Token = str_squish(Token)) %>% 
    View()

tidy_ph2 <- tidy_ph2 %>% 
    mutate(Token = str_to_lower(Token, locale = "es")) %>% 
    View()

tidy_ph2 <- tidy_ph2 %>% 
    mutate(Token = stri_trans_general(Token, "Latin-ASCII")) %>% 
    View()


### ejemplo para diapositiva ###

ejemplo_tidy <- ejemplo_phasel %>% 
    unnest_tokens(output = Token, input = text)

ejemplo_tidy$Token


ejemplo_tidy2 <- ejemplo_tidy %>% 
    mutate(Token = removePunctuation(Token))

ejemplo_tidy2 <- ejemplo_tidy2 %>% 
    mutate(Token = removeNumbers(Token)) 

ejemplo_tidy2 <- ejemplo_tidy2 %>% 
    mutate(Token = str_squish(Token))

ejemplo_tidy2 <- ejemplo_tidy2 %>% 
    mutate(Token = str_to_lower(Token, locale = "es"))

ejemplo_tidy2 <- ejemplo_tidy2 %>% 
    mutate(Token = stri_trans_general(Token, "Latin-ASCII")) 


ejemplo_tidy2$Token



#### Stopwords ####

stopwords_es_1 = read_excel("CustomStopWords.xlsx")
names(stopwords_es_1) = c("Token","Fuente")
stopwords_es_2 = tibble(Token=tm::stopwords(kind="es"), Fuente="tm")
stopwords_es = rbind(stopwords_es_1, stopwords_es_2)
stopwords_es = stopwords_es[!duplicated(stopwords_es$Token),]
remove(stopwords_es_1, stopwords_es_2)


tidy_ph3 <- tidy_ph2 %>% 
    anti_join(stopwords_es)

ejemplo_tidy3 = ejemplo_tidy2 %>% 
    anti_join(stopwords_es)

ejemplo_tidy3$Token

tidy_ph3 <- tidy_ph3 %>% 
    mutate(Token=removeWords(Token, stopwords_es$Token))  ### con esto nos queda en la mitad, la base de datos

ejemplo_tidy3 <- ejemplo_tidy3 %>% 
  mutate(Token=removeWords(Token, stopwords_es$Token))  ### con esto nos queda en la mitad, la base de datos


#### Stemming ####

tidy_phasel4 = tidy_phasel3 %>% 
  mutate(stem = wordStem(Token, "spanish"))



#### Lematización con diccionarios ####  para hacer esto activamos el paquete rvest


lematiza = function( frase ){
  palabra = gsub( " ", "+", frase )
  base.url = paste0( 
    "https://www.lenguaje.com/cgi-bin/lema.exe?edition_field=", 
    palabra,"&B1=Lematizar")
  lemma = read_html(base.url, encoding = "latin1") %>% 
    html_node(css = "div div div div div li") %>% 
    html_text(trim = T) 
  lemma = ifelse(lemma=='RaÃ­z del sustantivo "suma".',frase,lemma)
  return(lemma)
}
lematiza('aprendió')

#### Lematización con UDPIPE ####

model_sp = udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")
tidy_ph5 <- udpipe_annotate(model_sp,
                              ph_lemma$text) %>% 
  as_tibble()


ejemplo_tidy5 = udpipe_annotate(model_sp,
                               ejemplo_phasel$text) %>% 
  as_tibble()

tidy_ph5 = tidy_ph5 %>% 
  anti_join(stopwords_es, by=c("lemma"="Token")) %>% 
  filter(upos!="PUNCT")

ejemplo_tidy5 = ejemplo_tidy5 %>% 
  anti_join(stopwords_es, by=c("lemma"="Token")) %>% 
  filter(upos!="PUNCT")

saveRDS(tidy_ph5, "tidy_ph5.RDS")


#### Primer vistazo a los datos ####

tidy_ph5 <- read_rds('tidy_ph5.rds')   ## se carga el archivo guardado

tidy_ph5 %>% 
  count(upos) %>% 
  ggplot()+
  geom_col(aes(x=reorder(upos,n),y=(n/1000),fill=upos))+
  labs(x="Etiqueta POS", y="Frecuencia (en miles)")+
  coord_flip()+
  theme(legend.position = "none", text=element_text(size=13))+
  labs(
    title = "Figura 5: Distribución de etiquetas POST",
    subtitle = paste0(nrow(tidy_ph5)," palabras analizadas"),
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

tidy_ph5 %>% 
  filter(upos %in% c('NOUN','PROPN','VERB','ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot()+
  geom_col(aes(x=reorder_within(lemma,n,upos),y=n,fill=lemma))+
  scale_x_reordered()+
  labs(x="Lemma", y="Frecuencia")+
  facet_wrap(vars(upos), scales = "free", ncol = 2)+
  coord_flip()+
  theme(legend.position = "none", text=element_text(size=13))+
  labs(
    title = "Figura 6: Análisis por etiquetas POST",
    subtitle = paste0(nrow(tidy_ph5)," palabras analizadas"),
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )



#### TOPICOS #####

tweets <- ph_lemma$text

writeLines(as.character(tweets[[45707]]))

tweets <- chartr('??????','aeioun',tweets)# Quitar las tildes

tweets <- iconv(tweets, to = "ASCII", sub = "")  

tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)  # Remove the "RT" (retweet) and usernames 
tweets = gsub("http.+ |http.+$", " ", tweets)  # Remove html links
tweets = gsub("http[[:alnum:]]*", "", tweets)
tweets = gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets = gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets = gsub("^ ", "", tweets)  # Leading blanks
tweets = gsub(" $", "", tweets)  # Lagging blanks
tweets = gsub(" +", " ", tweets) # General spaces 
tweets = gsub("[[:cntrl:]]", " ", tweets) # saltos de linea y tabulaciones

tweets = tolower(tweets) #convertimos todo a minúsculas
tweets = removeWords(tweets, words = stopwords("spanish"))
tweets = removePunctuation(tweets)
tweets = removeNumbers(tweets)
tweets = stripWhitespace(tweets)

writeLines(as.character(tweets[[45707]]))

tweets = unique(tweets)

corpus <- Corpus(VectorSource(tweets))

### limpieza usando la librería TM ### 
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))  
corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords, "PabloHasel")
corpus = tm_map(corpus, removeWords, "pablohasel")
corpus = tm_map(corpus, removeWords, "hasel")
corpus = tm_map(corpus, removeWords, "pablo")
corpus = tm_map(corpus, removeWords, "hasl")



memory.limit(size = 30000)    ## Tiene que ver con el límite de memoria RAM que R tiene asignado. Puedes cambiarlo con el siguiente código, en el que el argumento size debe ser proporcionado en número de MB.

#Nube de Palabras
dtm = DocumentTermMatrix(corpus)
matrix <- as.matrix(dtm) 
#words <- sort(rowSums(matrix),decreasing=TRUE) 

####

doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
dtm

freq = colSums(as.matrix(dtm))
length(freq)

ord = order(freq, decreasing = TRUE)
(freq[head(ord, n = 20)])

asoc <- findAssocs(dtm, "carcel",0.2)
asoc

plot = data.frame(words = names(freq), count = freq)
plot = subset(plot, plot$count > 100) #creating a subset of words having more than 20 frequency

wordcloud2(data=plot, color='random-light',shape = "circle",size = 1)



plot2 = subset(plot, plot$count > 350) #palabras que se repiten +150 veces
ggplot(data = plot2, aes(reorder(words, count),
                         count,label = count,fill=count)) + 
    geom_bar(stat = 'identity') +coord_flip() +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12)) +
    geom_text(aes(label=count), hjust=1.5, colour="white", size=4)+
    theme(axis.text = element_text(size=9))+
    labs(
        x = NULL, y = NULL,
        title = "Figura 7: Palabras más usadas",
        subtitle = "Conversaciones hablando de Pablo Hasel",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )



#Sentimientos ##

#### Análisis de Sentimiento #####

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
    tbl_df()

tweets_sentimiento <- data.frame(words = names(freq))

#### Cambio de nombre para poder hacer join con las palabras de los tweets
names (afinn)[1] = "words"

tuits_afinn = tweets_sentimiento %>%
    inner_join(afinn, ., by = "words") %>%
    mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))


tema_graf <-
    theme_minimal() +
    theme(text = element_text(family = "serif"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#EBEBEB", colour = NA),
          legend.position = "none",
          legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))


tuits_afinn %>%
    group_by(Tipo) %>%
    summarise(n=n()) %>% 
    ggplot() +
    aes(Tipo, (n/nrow(tuits_afinn))*100, fill = Tipo) +
    geom_col() + 
    theme_minimal() +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    theme(plot.title = element_text(face = "bold", size = 12)) +
    theme(axis.text = element_text(size=9))+
    labs(title = "Figura 8: Postivos / Negativos  conversaciones relacionadas a Pablo Hasel",
         subtitle =paste0(round((nrow(tuits_afinn) / nrow(ph_lemma))*100),"% conversaciones analizadas"), 
         caption = "\nSource: Data collected from Twitter's REST API via rtweet")+ tema_graf +
    ylab("% de conversaciones") +
    xlab ("Sentimiento")

