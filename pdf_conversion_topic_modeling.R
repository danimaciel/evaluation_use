pacotes <- c("pdftools", "stringr", "openxlsx", "bibliometrix", "rscopus", "owlcarouselR", "htmltools", "slickR", "RTextTools", "textrank", "tibble", "gt", "ggplot2", "tidyverse", "dplyr", "stringi", "readr","writexl", "topicmodels", "tm", "readxl", "textplot", "XML", "readxl", "topicmodels", "caret", "tidyr", "quanteda","pdftools","stringr","NLP","curl", "tidytext", "wordcloud", "SnowballC", "stopwords", "tm", "RColorBrewer", "cluster", "factoextra", "knitr", "wordcloud2", "gridExtra", "plotly", "ggwordcloud", "webshot2", "htmlwidgets")
lapply(pacotes, require, character.only = TRUE)

setwd('C:\\Users\\danie\\Projetos R\\use_evaluation\\pdfs')

caminho_pasta <- "Users\\danie\\Projetos R\\use_evaluation\\pdfs"

arquivos_pdf <- list.files(pattern = ".pdf$")
all_pdfs <- lapply(arquivos_pdf, pdf_text)

document <- Corpus(VectorSource(all_pdfs))

#Convert all text to lower case
document <- tm_map(document, content_transformer(tolower))
#Remove numbers from the text 
document<-tm_map(document, removeNumbers)
#Remove stopwords in English
document<-tm_map(document, removeWords, stopwords("english"))

#Remove punctuation
document<-tm_map(document, removePunctuation, preserve_intra_word_dashes = TRUE)
#Remove white Spaces 
document<-tm_map(document, stripWhitespace)#remove white spaces
document <- tm_map(document, stemDocument)


document_text <- sapply(document, as.character)

# Escrevendo o vetor de caracteres para um arquivo
writeLines(document_text, "corpus_pdfs.txt")

document <- document[sapply(document, nchar) > 0]

dtm <- DocumentTermMatrix(document)

linhas_zeradas <- rowSums(as.matrix(dtm)) == 0 

dtm <- dtm[!linhas_zeradas, ] 

model_lda <- LDA(dtm, k = 4, control = list(seed = 1234))

beta_topics <- tidy(model_lda, matrix = "beta")
beta_topics

beta_top_terms <- beta_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 20) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

beta_top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic, scales = "free")+
  scale_y_reordered()

# Obtendo os pdfs de cada grupo - o mais forte
distribution <- posterior(model_lda)

for (i in 1:4) {
  topic_docs_indices <- which.max(distribution$topics[,i])
  topic_docs_names <- arquivos_pdf[topic_docs_indices]
  cat("Tópico", i, ": ", topic_docs_names, "\n")
}


# Itere sobre cada tópico
resultado <- data.frame(topico = integer(), pdf = character())

limiar <- 0.25 

for (i in 1:4) {
  # Identifique os índices dos documentos que têm probabilidade acima do limiar para o tópico i
  topic_docs_indices <- which(distribution$topics[,i] > limiar)
  
  # Mapeie os índices de volta aos nomes dos arquivos
  topic_docs_names <- arquivos_pdf[topic_docs_indices]
  
  # Adicione as informações ao dataframe
  tmp_df <- data.frame(topico = i, pdf = topic_docs_names)
  resultado <- bind_rows(resultado, tmp_df)
}

# Visualize o dataframe
print(resultado)

write_xlsx(resultado, "resultado_topicos.xlsx")

table(resultado$topico)


# Testando pertencimento

# Obtendo as probabilidades de cada documento para cada tópico
probabilidades <- distribution$topics 

# Obtenha o número de documentos e tópicos
num_documentos <- nrow(probabilidades)
num_topicos <- ncol(probabilidades)

# Suponha que `arquivos_pdf` é um vetor com os nomes dos documentos
for (i in 1:num_documentos) {
  cat("Documento", arquivos_pdf[i], ":\n")
  for (j in 1:num_topicos) {
    cat("  Tópico", j, ": ", round(probabilidades[i, j] * 100, 2), "%\n")
  }
  cat("\n")
}

unica %>% filter(base == "wos" & classificacao == "alta")


# Listando os trabalhos e tópicos

# Crie uma lista vazia para armazenar as linhas do dataframe
lista_df <- list()

# Loop através de cada documento
for (i in 1:num_documentos) {
  
  # Crie uma lista temporária para armazenar informações do documento atual
  temp <- list()
  
  # Adicione o nome do documento à lista temporária
  temp$artigo <- arquivos_pdf[i]
  
  # Adicione as probabilidades dos tópicos 1 a 4 à lista temporária
  for (j in 1:4) {
    nome_coluna <- paste0("topico ", j)
    temp[[nome_coluna]] <- round(probabilidades[i, j] * 100, 2)
  }
  
  # Adicione a lista temporária à lista do dataframe
  lista_df[[i]] <- temp
}

# Converta a lista em um dataframe
df_artigos_topicos <- bind_rows(lista_df)

table(resultado$topico)


file_names <- c("baker2013.pdf", "barker2007.pdf", "bemelmans-videc1989.pdf", "bornmann2013.pdf", "fiester1995_conv.pdf", "goldstein1978_conv.pdf", "horton2003.pdf", "johnson1998.pdf", "joly2016.pdf", "kendhammer2014.pdf", "kjesrud2021.pdf", "mackay2003.pdf", "meyer-krahmer1989.pdf", "ndebele2014.pdf", "pollard1983.pdf", "siegel1985.pdf")
file_names2 <- c("alborhamy2020.pdf", "brock2016.pdf", "brunet2022.pdf", "cozzens1997.pdf", "Diez2016.pdf", "jordan2008.pdf", "kuhlmann1998.pdf", "macdonald2006.pdf", "mackay1992conv.pdf", "milzow2018.pdf", "patton_horton2009.pdf", "Paul2021.pdf", "raitzer2008.pdf", "roseland2011.pdf", "stephens-chu2022.pdf", "teirlinck2012.pdf", "vakola2000.pdf")
file_names3 <- c("bogt2009.pdf", "brandon2009.pdf", "cisneros_stake_2012.pdf", "feinstein2002.pdf", "flemming1991.pdf", "hemlin1996.pdf", "Khong-ngama2013.pdf", "king1988.pdf", "Patton1975.pdf", "sanjines2018.pdf", "takur2007.pdf", "thompson1983.pdf", "weiss1998.pdf")
file_names4 <- c("brofoss1998.pdf", "chelimsky1977.pdf", "Eckhard2020.pdf", "hall1981.pdf", "hansson2006.pdf", "johnson2009.pdf", "king2018.pdf", "kupiec-et-al-2023.pdf", "leviton1981.pdf", "lipton1992conv.pdf", "luukkonen1995.pdf", "Most2010.pdf", "oreilly1980.pdf", "patton2020.pdf", "pollard1987.pdf", "pollard1987conv.pdf", "shulha_cousins_1997.pdf", "wollmann2016.pdf")


# Extraia o nome do autor e o ano
authors <- str_extract(file_names4, "^[a-z-]+")
years <- str_extract(file_names4, "\\d{4}")

capitalize <- function(x) {
  sapply(strsplit(x, " "), function(word) {
    return(paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word)))))
  })
}

authors <- capitalize(authors)
formatted_names <- paste0(authors, " (", years, ")")

autores_formatados <- as.data.frame(formatted_names)

print(formatted_names)

# Relacionando os tópicos ao df

class_alta_final_0510 <- read_excel("class_alta_final_0510.xlsx")

# Adicionar ".pdf" apenas se não estiver presente
class_alta_final_0510$arquivo <- ifelse(grepl("\\.pdf$", class_alta_final_0510$arquivo),
                                        class_alta_final_0510$arquivo,
                                   paste0(class_alta_final_0510$arquivo, ".pdf"))

# Mesclando os dataframes com base nas colunas especificadas
merged_df <- merge(class_alta_final_0510, resultado, by.x = "arquivo", by.y = "pdf", all.x = TRUE)

writexl::write_xlsx(merged_df, "classe_final_top_dec_qt_autor.xlsx")
