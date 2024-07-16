pacotes <- c("pdftools", "stringr", "openxlsx", "bibliometrix", "rscopus", "owlcarouselR", "htmltools", "slickR", "RTextTools", "textrank", "tibble", "gt", "ggplot2", "tidyverse", "dplyr", "stringi", "readr","writexl", "topicmodels", "tm", "readxl", "textplot", "XML", "readxl", "topicmodels", "caret", "tidyr", "quanteda","pdftools","stringr","NLP","curl", "tidytext", "wordcloud", "SnowballC", "stopwords", "tm", "RColorBrewer", "cluster", "factoextra", "knitr", "wordcloud2", "gridExtra", "plotly", "ggwordcloud", "webshot2", "htmlwidgets")
lapply(pacotes, require, character.only = TRUE)

setwd("C:/Users/danie/Projetos R/use_evaluation/revisao")

# PDF files folder
arquivos_pdf <- list.files(pattern = "\\.pdf$")

# Extracting texts from each pdf and removing unwanted pages
all_pdfs <- lapply(arquivos_pdf, function(pdf) {
  pages <- pdf_text(pdf)
  if (length(pages) > 2) {
    pages[-c(1, length(pages))]
  } else {
    pages  # Does not remove anything if there are only two pages
  }
})

# Flatten the page list into a single text vector
texto_limpas <- unlist(all_pdfs)

# Creating the corpus
document <- Corpus(VectorSource(texto_limpas))

# Processing texts
document <- tm_map(document, content_transformer(tolower))
document <- tm_map(document, removeNumbers)
document <- tm_map(document, removePunctuation)
document <- tm_map(document, stripWhitespace)

# Removing stopwords 
data("stop_words")
stopwords_tidy <- stop_words$word[stop_words$lexicon %in% c("snowball", "SMART")]
document <- tm_map(document, removeWords, stopwords_tidy)

# Removing empty documents 
document <- document[sapply(document, nchar) > 0]

# Creating matrix Document-Term
dtm <- DocumentTermMatrix(document)

# Checking for zeroed lines and remove
if (any(rowSums(as.matrix(dtm)) == 0)) {
  dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
}

# Executing LDA with 4 groups
model_lda <- LDA(dtm, k = 4, method = "Gibbs", control = list(seed = 1234, burnin = 1000, iter = 2000, thin = 100))

# Extraction of terms from topics
beta_topics <- tidy(model_lda, matrix = "beta")

# Selection of terms with highier beta
beta_top_terms <- beta_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 40) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Viewing topics
beta_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

 
document_topic_dist <- posterior(model_lda)$topics

document_topic <- apply(document_topic_dist, 1, which.max)

for (topic in unique(document_topic)) {
  cat("Documentos mais representativos para o tópico", topic, ":\n")
  top_docs_indices <- order(document_topic_dist[,topic], decreasing = TRUE)[1:5]
  cat(arquivos_pdf[top_docs_indices], "\n\n")
}

resultado <- data.frame(topico = integer(), pdf = character())
limiar <- 0.25 

for (i in 1:4) {
  topic_docs_indices <- which(document_topic_dist[,i] > limiar)
  topic_docs_names <- arquivos_pdf[topic_docs_indices]
  tmp_df <- data.frame(topico = i, pdf = topic_docs_names)
  resultado <- bind_rows(resultado, tmp_df)
}

print(resultado)

resultado2 <- resultado[!is.na(resultado$pdf) & resultado$pdf != "", ]

writexl::write_xlsx(resultado2, "resultado_modelagem2.xlsx")


