library(duckdb)
library(dplyr)
library(stringr)
library(glue)

# TELECHARGEMENT DES FICHIERS -------------------------

url_table_logement <- "https://static.data.gouv.fr/resources/recensement-de-la-population-fichiers-detail-logements-ordinaires-en-2020-1/20231023-123618/fd-logemt-2020.parquet"
url_table_individu <- "https://static.data.gouv.fr/resources/recensement-de-la-population-fichiers-detail-individus-localises-au-canton-ou-ville-2020-1/20231023-122841/fd-indcvi-2020.parquet"
url_doc_logement <- "https://www.data.gouv.fr/fr/datasets/r/c274705f-98db-4d9b-9674-578e04f03198"
url_doc_individu <- "https://www.data.gouv.fr/fr/datasets/r/1c6c6ab2-b766-41a4-90f0-043173d5e9d1"

options(timeout = max(300, getOption("timeout")))

if (!file.exists("FD_LOGEMT_2020.parquet")){
    download.file(url_table_logement, "FD_LOGEMT_2020.parquet")
}
if (!file.exists("FD_INDCVI_2020.parquet")){
    download.file(url_table_individu, "FD_INDCVI_2020.parquet")
}
if (!file.exists("dictionnaire_variables_logemt_2020.csv")){
    download.file(url_doc_logement, "dictionnaire_variables_logemt_2020.csv")
}
if (!file.exists("dictionnaire_variables_indcvi_2020.csv")){
    download.file(url_doc_individu, "dictionnaire_variables_indcvi_2020.csv")
}


# CREATION DE LA DATABASE ----------------------------------------

con <- dbConnect(duckdb())

dbExecute(
  con,
  glue_sql(  
    'CREATE OR REPLACE VIEW table_logement AS ',
    'SELECT * FROM read_parquet("FD_LOGEMT_2020.parquet")',
    .con=con
  )
)
table_logement <- tbl(con, "table_logement")
table_individu <- tbl(con, glue('read_parquet("FD_INDCVI_2020.parquet")'))


# PREMIERES EXPLORATIONS GRAPHIQUES ------------------------------

library(ggplot2)

pyramide_ages <- table_individu %>%
  filter(DEPT %in% c('11', '31', '34')) %>%
  group_by(AGED, departement = DEPT) %>%
  summarise(individus = sum(IPONDI), .groups = "drop") %>%
  arrange(departement, AGED) %>%
  collect()

ggplot(pyramide_ages, aes(x = AGED, y = individus)) +
  geom_bar(aes(fill = departement), stat = "identity") +
  geom_vline(xintercept = 18, color = "grey", linetype = "dashed") +
  facet_wrap(~departement, scales = "free_y", nrow = 3) +
  theme_minimal() +
  labs(y = "Individus recensés", x = "Âge")

# + 1 carte


