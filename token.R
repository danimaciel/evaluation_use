pacotes <- c("stringr", "openxlsx", "bibliometrix", "rscopus", "owlcarouselR", "htmltools", "slickR", "RTextTools", "textrank", "tibble", "gt", "ggplot2", "tidyverse", "dplyr", "stringi", "readr","writexl", "topicmodels", "tm", "readxl", "textplot", "XML", "readxl", "topicmodels", "caret", "tidyr", "quanteda","pdftools","stringr","NLP","curl", "tidytext", "wordcloud", "SnowballC", "stopwords", "tm", "RColorBrewer", "cluster", "factoextra", "knitr", "wordcloud2", "gridExtra", "plotly", "ggwordcloud", "webshot2", "htmlwidgets")
lapply(pacotes, require, character.only = TRUE)

setwd('C:\\Users\\danie\\Projetos R\\use_evaluation')

# # Chamando os arquivos
# scopus <- read_csv2("scopus13.csv")
# gs <- read_excel("gs71.xls")
# wos <- read_excel("wos67.xls")

# tolower - título
scopus$Title <- tolower(scopus$Title)
gs$Title <- tolower(gs$Title)
wos$`Article Title` <- tolower(wos$`Article Title`)

scopus_dupl <- scopus[duplicated(scopus$Title), ]
gs_dupl <- gs[!duplicated(gs$Title), ]
wos_dupl <- wos[duplicated(wos$`Article Title`), ]

# Juntanto títulos num único df
df_todos <- data.frame(Title = c(scopus$Title, gs_dupl$Title, wos$`Article Title`))
df_todos_dupl <- df_todos[duplicated(df_todos$Title), ]
print(df_todos_dupl)

# Contando autores

unica <- read_xlsx("database_all.xlsx")

unica$qtd_autor <- str_count(unica$author, pattern = ";") + 1

class_alta <- unica %>% 
  dplyr::filter(classificacao == "alta")

class_alta <- data.frame(lapply(class_alta, tolower))

names(class_alta)

names(class_alta)[names(class_alta) == "author"] <- "autores"

names(class_alta)

# Criando campo Década

class_alta$ano <- as.numeric(class_alta$ano)

class_alta$decada <- floor(class_alta$ano / 10) * 10

# Preparação da base

class_alta$combinada <- paste(class_alta$title, class_alta$resumo, class_alta$palavra_chave, class_alta$palavra_chave1)

# Stopword - palavras indesejadas
stopword_en <- c(stopwords("en"), "also", "can", "study", "authors", "jonh", "press", "wiley", "springerverlag", "limited", "abstract",
                 "available", "taylor", "francis", "group", "ltd", "rights", "reserved", "this", "we", "old", "one", "an", "on", "of", 
                 "the", "in", "is", "of", "for the", "to the", "of the", "in the", "of a", "in this", "of this", "on the", "et", "al", 
                 "elsevier", "all","rights reserved", "open", "access", "springer", "licensee", "business", "media", 
                 "basel", "mdpi", "switzerland", "results", "result", "license", "creative", "studies", "methods", "however",  
                 "article", "articles", "paper", "method", "model", "models", "new", "present", "project", "projects", "two", 
                 "publication", "main", "number", "published")

# Nova função para remover stopwords
remove_stopwords <- function(text, stopwords) {
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[!words %in% stopwords]
  return(paste(words, collapse = " "))
}

class_alta$combinada <- sapply(class_alta$combinada, remove_stopwords, stopwords = stopword_en)
class_alta$qtd_autor <- as.numeric(class_alta$qtd_autor)
sum(class_alta$qtd_autor)/39

# Tokenização e modelagem

classalta_tokens <- class_alta %>%
  unnest_tokens(output = palavra_combinada,
                input = combinada,
                token = "ngrams",
                n = 2,
                stopwords = stopword_en)

NFILTER <- 3

contagem_clasalta <- classalta_tokens %>%
  count(palavra_combinada,
        sort = TRUE) %>% 
  filter(n>=NFILTER)

# Base 01/10/2023

base <- read_xlsx("use_evaluation/base_0110.xls")




