pacman::p_load("readxl", "dplyr", "tidyverse", "networkD3", "webshot", "htmlwidgets")

setwd("C:/Users/danie/Projetos R/use_evaluation/revisao")
dir.create("analise_final")

dados <- read_excel("analise_final/final_data.xlsx")

areas <- dados %>%
  separate_rows(`field_oecd (2007)`, sep = "; ") %>%
  mutate(`field_oecd (2007)` = str_trim(`field_oecd (2007)`)) 

usos <- areas %>%
  separate_rows(use_weiss, sep = "; ") 

users <- usos %>%
  separate_rows(users, sep = "; ") %>%
  mutate(users = str_trim(users))

users$users <- sapply(users$users, tools::toTitleCase)

tipos_usuarios <- as.data.frame(table(users$users))

method <- areas %>%
  separate_rows(method, sep = "; ") %>%
  mutate(`method` = str_trim(`method`)) 


writexl::write_xlsx(areas, "analise_final/areas_longo.xlsx")
writexl::write_xlsx(usos, "analise_final/usos_longo.xlsx")
writexl::write_xlsx(users, "analise_final/usuarios.xlsx")
writexl::write_xlsx(method, "analise_final/methods.xlsx")

names(areas)

table(areas$type_approach, areas$`field_oecd (2007)`, areas$decade)

#sankey

areas_sankey <- areas %>%
  mutate(across(c(type_evaluation, decade, `field_oecd (2007)`), str_trim))

links1 <- areas_sankey %>%
  count(type_evaluation, decade, name = "value") %>%
  rename(source = type_evaluation, target = decade)

links2 <- areas_sankey %>%
  count(decade, `field_oecd (2007)`, name = "value") %>%
  rename(source = decade, target = `field_oecd (2007)`)

links <- bind_rows(links1, links2)

nodes <- data.frame(name = unique(c(links$source, links$target)))

links <- links %>%
  mutate(source = match(source, nodes$name) - 1,
         target = match(target, nodes$name) - 1)

sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        sinksRight = FALSE, 
                        fontSize = 16)  # Ajuste o tamanho da fonte aqui

sankey


# concatenaded authors by fields

autores_concatenados <- areas %>%
  group_by(type_evaluation, decade, `field_oecd (2007)`) %>%
  summarise(authors_concatenated = paste(upper_lower, collapse = "; "))

# users by fields and evaluation type
usuarios_areas_tipo_eval <- users %>%
  group_by(type_evaluation, decade, `field_oecd (2007)`, users) %>%
  summarise(authors_concatenated = paste(upper_lower, collapse = "; "))

df_count <- usuarios_areas_tipo_eval %>%
  group_by(type_evaluation, `field_oecd (2007)`, users, decade, authors_concatenated) %>%
  summarise(count = n(), .groups = "drop")

df_wide <- df_count %>%
  pivot_wider(
    names_from = decade,
    values_from = count,
    values_fill = list(count = NA)  # Preenche com NA onde não há dados
  )

df_wide_prep <- df_wide %>%
  mutate(authors_concatenated = sapply(strsplit(authors_concatenated, ";\\s*"), function(x) {
    paste(unique(x), collapse = "; ")
  }))


df_wide_unicos <- df_wide %>%
  distinct(authors_concatenated, .keep_all = TRUE)

writexl::write_xlsx(df_wide_unicos, "analise_final/usuarios_area_evaluation.xlsx")

length(unique(areas$affiliation))

# counting fields 
contagem <- areas %>%
  count(decade, type_evaluation, `field_oecd (2007)`, type_approach)

df_wide_contagem <- contagem %>%
  distinct(`field_oecd (2007)`, .keep_all = TRUE)

writexl::write_xlsx(contagem, "contagem_areas_approach.xlsx")
