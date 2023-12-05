pacotes <- c("stringdist", "stringr", "openxlsx", "bibliometrix", "rscopus", "owlcarouselR", "htmltools", "slickR", "RTextTools", "textrank", "tibble", "gt", "ggplot2", "tidyverse", "dplyr", "stringi", "readr","writexl", "topicmodels", "tm", "readxl", "textplot", "XML", "readxl", "topicmodels", "caret", "tidyr", "quanteda","pdftools","stringr","NLP","curl", "tidytext", "wordcloud", "SnowballC", "stopwords", "tm", "RColorBrewer", "cluster", "factoextra", "knitr", "wordcloud2", "gridExtra", "plotly", "ggwordcloud", "webshot2", "htmlwidgets")
lapply(pacotes, require, character.only = TRUE)

# Calling the file

base <- read_excel("database_all.xlsx")

# Filtering by Abstract
base_filtrada <- base %>%
  filter(is.na(abstract) | abstract == "[No abstract available]")

base <- base %>%
  filter(!is.na(abstract) & abstract != "[No abstract available]")

# Checking duplicities

base$`Article Title` <- tolower(base$`Article Title`)
base$`Clean Title` <- gsub("[;:\\-\\.\\.\\.]", "", base$`Article Title`)

base_dupl <- base[duplicated(base$`Clean Title`), ]

base <- base[!duplicated(base$`Clean Title`), ] #149 titles without duplication

base$qtd_autor <- str_count(base$Authors, pattern = ";") + 1

base$decada <- floor(base$`Publication Year` / 10) * 10

# Manually cleaning

writexl::write_xlsx(base, "base.xlsx")

# Clean Database - manually verification extracting 14 titles - 6 duplicates; 8 incompleteness

base <- read_excel("base.xlsx", sheet = 1)


