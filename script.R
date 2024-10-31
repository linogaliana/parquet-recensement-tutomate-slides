


# TELECHARGEMENT DES FICHIERS -------------------------



# CREATION DE LA DATABASE ----------------------------------------





# PREMIERES EXPLORATIONS GRAPHIQUES ------------------------------





# Graphique: MODE DE TRANSPORT x AGE
# Comptage des modes de transport principaux
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

# Part des modes de transport par âge et département
transports_age <- transports_age %>%
  group_by(DEPT, AGEREVQ) %>%
  mutate(p = n/sum(n))

# Filtre sur un département
transports_age <- transports_age %>%
  filter(DEPT == 75)

# Ajout du libellé du mode de transport 
transports_age <- transports_age %>%
  inner_join(
    y = documentation_individus %>% filter(COD_VAR == "TRANS"),
    by = c("TRANS" = "COD_MOD")
  )

# Graphique
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


# Tableau: statistiques descriptives
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

# Calculer les statistiques descriptives : moyenne, min, max pour chaque variable
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

