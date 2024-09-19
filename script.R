library(duckdb)
library(dplyr)
library(stringr)
library(glue)
library(dplyr)
library(cartiflette)

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
documentation_logement <- readr::read_csv2("dictionnaire_variables_logemt_2020.csv")
documentation_individus <- readr::read_csv2("dictionnaire_variables_indcvi_2020.csv")


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


# Résidences principales et secondaires

departements <- carti_download(
    values="France",
    crs=4326,
    borders="DEPARTEMENT",
    vectorfile_format="geojson",
    filter_by="FRANCE_ENTIERE_DROM_RAPPROCHES",
    source="EXPRESS-COG-CARTO-TERRITOIRE",
    year=2022,
)

parc_locatif <- table_logement %>%
  mutate(DEPT = substring(COMMUNE, 1, 3)) %>%
  mutate(
    DEPT = if_else(
      starts_with(DEPT, "97"),
      DEPT,
      substring(DEPT, 1, 2)
    )
  ) %>%
  group_by(DEPT, CATL) %>%
  summarise(n = sum(IPONDL)) %>%
  ungroup() %>%
  collect()





parc_locatif_sf <- departements %>%
  inner_join(
    parc_locatif,
    by = c("INSEE_DEP" = "DEPT"),
    relationship = "many-to-many" # on a des clés dupliquées dans le fond cartiflette (e.g. Ile de France) et dans le dataframe (4 valeurs par dep)
  ) %>%
  group_by(INSEE_DEP) %>%
  mutate(p = n/sum(n)) %>%
  ungroup

# Résidences secondaires
ggplot(parc_locatif_sf %>% filter(CATL == "3")) +
  geom_sf(aes(fill = p), color = "white") +
  scale_fill_fermenter(
        n.breaks = 5, 
        palette = "RdPu",
        direction = 1,
        labels = scales::label_percent(
          scale_cut = scales::cut_short_scale()
        )
  ) +
  theme_void() +
  labs(
    fill = "Part dans le\nparc de logement (%)",
    title = "Cartographie des résidences secondaires",
    caption = "Source: Insee, Fichiers détails du recensement de la population"
  )

# Logements vacants
ggplot(parc_locatif_sf %>% filter(CATL == "4")) +
  geom_sf(aes(fill = p), color = "white") +
  scale_fill_fermenter(
        n.breaks = 5, 
        palette = "RdPu",
        direction = 1,
        labels = scales::label_percent(
          scale_cut = scales::cut_short_scale()
        )
  ) +
  theme_void() +
  labs(
    fill = "Part dans le\nparc de logement (%)",
    title = "Cartographie des résidences secondaires",
    caption = "Source: Insee, Fichiers détails du recensement de la population"
  )


# 

iris_uu_marseille = carti_download(
    values = c("00759"),
    crs = 4326,
    borders="IRIS",
    vectorfile_format="geojson",
    filter_by="UNITE_URBAINE",
    source="CONTOUR-IRIS",
    provider="Cartiflette",
    path_within_bucket="test",
    dataset_family="production",
    simplification = "0",
    territory="france", filename = "value",
    year=2023)


