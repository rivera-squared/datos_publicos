library(quanteda)
library(readr)
library(tidyverse)
library(tidytext)
library(stm)
library(lubridate)

# Para este proyecto, quiero graficar (line graph) la evolución de los temas que publica
# Jay através del tiempo. Mi approach para esto es utilizar LDA para determinar los temas
# por semana. Ya cuando tenga los temas, se hace mas fácil encontrar las palabras claves
# para poder determinar los temas.

df = read_csv('https://raw.githubusercontent.com/rivera-squared/datos_publicos/main/jay_df.csv')
df$año = year(df$fecha)
df$semana = week(df$fecha)
df$dia = day(df$fecha)
df$titulo = tolower(df$titulo)

################################################################################
# Comencemos con la semana 15 que cubre 4/11/2022 - 4/15/2022
df1 = df %>%
  filter(año == 2022,
         semana == 15) 


#Custom stopwords in Spanish
palabras_para = c(stopwords("es"), "hoy",'calle','noticias','vea',
                   'resumen','escucha','abril','tras',' con calle','de hoy','que')

# Creating DFM
jf_dfm = tokens(df1$titulo,
       remove_punct = TRUE,
       remove_numbers = TRUE) %>%
  dfm() %>%
  dfm_remove(., palabras_para)

# Creating a LDA
jay_lda = stm(jf_dfm, K = 20, verbose = FALSE)
plot(jay_lda)


# Al ver la gráfica del LDA puedo tener una idea de las palabras clave para determinar
# los temas. Por el momento con este approach, solamente tengo que utilizar una palabra
# clave a la vez para poder determinar el tema. Para usar frases, tengo que usar n_grams (adelante toco el tema)

# Creando dicccionarios para poder resumir los temas en dfm
dict <- dictionary(list(Luma = c("luma"),
                        apagón = c("apagón"),
                        `Elianni Bello` = "madre",
                        covid = c("covid-19",'coronavirus'),
                        aborto = "aborto"))
                        
jf_topic_df = as.data.frame(dfm_lookup(jf_dfm, dict, valuetype = "glob"))
jf_topic_df$fecha = df1$fecha
jf_topic_df$semana = week(df1$fecha)
jf_topic_df$año = year(df1$fecha)
jf_topic_df$mes = month(df1$fecha)
jf_topic_df$dia = day(df1$fecha)

# Habrán situaciones donde necesito una frase compuesta de dos o mas palabras para poder
# incluirlas en el diccionario. Para lograrlo, necesito convertirlos en tokens, para
# usar la funcion tokens_ngrams. La idea es hacer un dfm con n_grams para luego
# hacer un left join con el dfm original del diccionario.

# Creating tokens
jf_token = tokens(df1$titulo,
       remove_punct = TRUE,
       remove_numbers = TRUE)

kwic(jf_token, "madre", valuetype = "fixed") # util para ver contexto de las palabras
# Creando n_grams
jf_ngrams = tokens_ngrams(jf_token, n = 2L, concatenator = " ")

# Seek top ngrams
topfeatures(jf_ngrams %>% dfm(), 20) 

# Creando diccionario que contenga n_grams
dict_ngram = dictionary(list(`Elizabeth Torres` = c("elizabeth torres"),
                        `Costa Sur` = 'costa sur'))

jf_topic_df_ngram = as.data.frame(dfm_lookup(jf_ngrams %>% dfm(), dict_ngram, valuetype = "glob"))

kwic(jf_ngrams, "costa sur", valuetype = "fixed") # util para ver contexto de las palabras


#################
# Al final de todos los dfms, se hace un left_join
jf_topic_df_15 = jf_topic_df %>%
  left_join(., jf_topic_df_ngram, by = "doc_id") %>%
  select(doc_id, fecha, año, semana, dia, everything()) 

################################################################################
# Segimos con la semana 14 que cubre 4/3/2022 - 4/8/2022
df_14 = df %>%
  filter(año == 2022,
         semana == 14) 

# Creating DFM
jf_dfm_14 = tokens(df_14$titulo,
                remove_punct = TRUE,
                remove_numbers = TRUE) %>%
  dfm() %>%
  dfm_remove(., palabras_para)

# Creating a LDA
jay_lda = stm(jf_dfm_14, K = 20, verbose = FALSE)
plot(jay_lda)

# Creando dicccionarios para poder resumir los temas en dfm
dict_14 <- dictionary(list(`Alcalde de Guayama` = c("guayama"),
                        Luma = c("luma"),
                        `aborto` = "aborto",
                        covid = c("covid-19",'coronavirus'),
                        aborto = "aborto"))

jf_topic_df_14 = as.data.frame(dfm_lookup(jf_dfm_14, dict_14, valuetype = "glob"))
jf_topic_df_14$fecha = df_14$fecha
jf_topic_df_14$semana = week(df_14$fecha)
jf_topic_df_14$año = year(df_14$fecha)
jf_topic_df_14$mes = month(df_14$fecha)
jf_topic_df_14$dia = day(df_14$fecha)

jf_token_14 = tokens(df_14$titulo,
       remove_punct = TRUE,
       remove_numbers = TRUE)
kwic(jf_token_14, "madre", valuetype = "fixed") # util para ver contexto de las palabras

# Me quedé en continuar expandiendo el diccionario para luego crear el diccionario de n_grams
# Buscar "junta fiscal"

