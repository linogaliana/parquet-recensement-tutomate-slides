library(duckdb)
library(dplyr)
library(stringr)
library(glue)
library(dplyr)
library(cartiflette)
library(ggplot2)
library(sf)
library(gt)


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

departements <- carti_download(
    values="France",
    crs=4326,
    borders="DEPARTEMENT",
    vectorfile_format="geojson",
    filter_by="FRANCE_ENTIERE_DROM_RAPPROCHES",
    source="EXPRESS-COG-CARTO-TERRITOIRE",
    year=2022,
)

# Distribution des âges
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

# Répartition des plus de 60 ans
  # Part des plus de 60 ans par département
  population_60_plus <- table_individu %>%
    group_by(DEPT) %>%
    summarise(
      total_population = sum(IPONDI), # Population totale
      population_60_plus = sum(IPONDI[AGED > 60]) # Population de plus de 60 ans
    ) %>%
    mutate(pourcentage_60_plus = population_60_plus / total_population * 100) %>%
    collect()

  # Joindre les données au fond de carte des départements
  departements_60_plus_sf <- departements %>%
    inner_join(
      population_60_plus,
      by = c("INSEE_DEP" = "DEPT")
    )

  # Carte 
  ggplot(departements_60_plus_sf) +
    geom_sf(aes(fill = pourcentage_60_plus)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) + 
    theme_minimal() + 
    labs(
      title = "Part des personnes de plus de 60 ans par département",
      caption = "Source: Insee, Fichiers détails du recensement de la population"
    )

  ggsave("carte_part_60ans_plus_dept.png", width = 10, height = 8)



# Résidences principales et secondaires
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

# MODE DE TRANSPORT x AGE
transports_age <- table_individu %>%
  mutate(
    DEPT = if_else(
      starts_with(DEPT, "97"),
      DEPT,
      substring(DEPT, 1, 2)
    )
  ) %>%
  filter(!(TRANS %in% c("1", "Z"))) %>% #on fait un parmi les transports
  group_by(DEPT, AGEREVQ, TRANS) %>%
  summarise(n = sum(IPONDI)) %>%
  ungroup() %>%
  collect()


transports_age <- transports_age %>%
  group_by(DEPT, AGEREVQ) %>%
  mutate(p = n/sum(n))


transports_age <- transports_age %>%
  filter(DEPT == 75)
  
transports_age <- transports_age %>%
  inner_join(
    y = documentation_individus %>% filter(COD_VAR == "TRANS"),
    by = c("TRANS" = "COD_MOD")
  )
  


ggplot(transports_age) +
  geom_line(aes(x = as.numeric(AGEREVQ), y = p, color = factor(LIB_MOD))) +
  scale_x_continuous(limits = c(20,70))


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


# Un tableau de statistiques descriptives
  # Part des plus de 60 ans
  part_population_60_plus <- table_individu %>%
    group_by(DEPT) %>%
    summarise(
      total_population = sum(IPONDI), # Population totale
      population_60_plus = sum(IPONDI[AGED > 60]) # Population des plus de 60 ans
    ) %>%
    mutate(pourcentage_60_plus = population_60_plus / total_population * 100) %>%
    collect()

  # Résidences secondaires
  part_residences_secondaires <- parc_locatif %>%
    filter(CATL == "2") %>%
    select(DEPT, n) %>%
    group_by(DEPT) %>%
    summarise(total_residences_secondaires = sum(n)) %>%
    mutate(pourcentage_residences_secondaires = 100 * total_residences_secondaires / sum(total_residences_secondaires)) %>%
    ungroup()

  # Logements vacants
  part_logements_vacants <- parc_locatif %>%
    filter(CATL == "4") %>%
    select(DEPT, n) %>%
    group_by(DEPT) %>%
    summarise(total_logements_vacants = sum(n)) %>%
    mutate(pourcentage_logements_vacants = 100 * total_logements_vacants / sum(total_logements_vacants)) %>%
    ungroup()

  # Calcul des statistiques descriptives : moyenne, min, max pour chaque variable
  statistiques_descriptives <- tibble(
    variable = c("Part des 60 ans et plus (%)", "Part des résidences secondaires (%)", "Part des logements vacants (%)"),
    moyenne = c(
      mean(part_population_60_plus$pourcentage_60_plus, na.rm = TRUE),
      mean(part_residences_secondaires$pourcentage_residences_secondaires, na.rm = TRUE),
      mean(part_logements_vacants$pourcentage_logements_vacants, na.rm = TRUE)
    ),
    minimum = c(
      min(part_population_60_plus$pourcentage_60_plus, na.rm = TRUE),
      min(part_residences_secondaires$pourcentage_residences_secondaires, na.rm = TRUE),
      min(part_logements_vacants$pourcentage_logements_vacants, na.rm = TRUE)
    ),
    maximum = c(
      max(part_population_60_plus$pourcentage_60_plus, na.rm = TRUE),
      max(part_residences_secondaires$pourcentage_residences_secondaires, na.rm = TRUE),
      max(part_logements_vacants$pourcentage_logements_vacants, na.rm = TRUE)
    )
  )

# Générer la table de stats desc avec gt
table_stats_desc <- statistiques_descriptives %>%
  gt() %>%
  tab_header(
    title = "",
    subtitle = ""
  ) %>%
  cols_label(
    variable = "",
    moyenne = "Moy",
    minimum = "Min",
    maximum = "Max"
  ) %>%
  fmt_number(
    columns = c(moyenne, minimum, maximum),
    decimals = 1  # Formater toutes les colonnes avec 1 décimale
  ) %>%
  # Personnaliser l'apparence de la table
  tab_options(
    heading.title.font.size = px(18),  # Taille du titre réduite à 18px
    table.border.top.color = "transparent",  # Bordure supérieure transparente (au-dessus du titre)
    table.border.bottom.color = "black", # Bordure inférieure en noir
    table.border.bottom.width = px(2),   # Épaisseur de la bordure inférieure
    column_labels.border.bottom.color = "black", # Bordure sous les noms de colonnes
    column_labels.border.bottom.width = px(1),   # Épaisseur de la bordure sous les noms de colonnes
    table_body.hlines.style = "none",  # Supprimer les lignes horizontales dans le corps
    table_body.vlines.style = "none"   # Supprimer les lignes verticales dans le corps
  ) %>%
  # Supprimer les bordures latérales internes
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "transparent",
      weight = px(0)
    ),
    locations = cells_body()
  )

# Afficher la table
table_stats_desc

