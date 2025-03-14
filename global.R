# global.R

# Configuration initiale
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Fonction d'installation sécurisée
safe_install <- function(pkg) {
 {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        try({
            install.packages(pkg, 
                           dependencies = TRUE,
                           quiet = TRUE,
                           type = "binary")
        }, silent = TRUE)
    }
}

# Installation des packages dans l'ordre des dépendances
packages_order <- c(
    # Packages de base
    "shiny",
    "shinydashboard",
    
    # Dépendances spatiales
    "terra",
    "sp",
    "raster",
    
    # Autres packages
    "dplyr",
    "DT",
    "readxl",
    "writexl",
    "ggplot2",
    "tidyr",
    "sf",
    "leaflet",
    "wordcloud2"
)

# Installation et chargement des packages
for(pkg in packages_order) {
    message(sprintf("Installation de %s...", pkg))
    safe_install(pkg)
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        warning(sprintf("Impossible de charger %s", pkg))
        if(pkg %in% c("leaflet", "sf")) {
            next
        }
    }
}

## DATA ------------------------------------

happiness_survey <- tryCatch(
    readxl::read_excel("data/happiness_survey.xlsx"),
    error = function(e) {
        message("Erreur lors de la lecture du fichier: ", e$message)
        return(NULL)
    }
)

if (!is.null(happiness_survey)) {
    # Initialisation des colonnes
    happiness_survey[c("familles", "amis", "relations_amoureuses", "relations_sociales")] <- 0

    # Sélection et renommage des colonnes
    happiness_survey <- happiness_survey[, 2:24]
    colnames(happiness_survey) <- c("sexe", "age", "departement", "heureux_ou_non", "importance_a", 
                                  "travail_epanouissement", "importance_argent", "ville_nature", 
                                  "temps_personnel", "activites_sportives", "activites_creative", 
                                  "sante", "developpement_personnel", "reve_enfant", "endroit_reve", 
                                  "heureux_temps_perso", "heureux_quotidien", "but_vie", 
                                  "etre_heureux_selon_vous", "familles", "amis", 
                                  "relations_amoureuses", "relations_sociales")

    # Création des sous-ensembles
    happiness_survey_oq <- happiness_survey %>% select(c(14:19))

    # Conversion du département en entier
    happiness_survey$departement <- as.integer(happiness_survey$departement)

    # Ajout des régions
    happiness_survey <- happiness_survey %>% 
        mutate(region = case_when(
            departement  == 29 | departement == 22 | departement  == 56 | departement  == 35 ~ "Bretagne",
            departement  == 44 | departement == 53 | departement  == 49 | departement  == 72 | departement == 85 ~ "Pays de la loire", 
            departement  == 17 | departement == 79 | departement == 86 | departement == 16  | departement == 87  | departement == 23  | departement == 19  | departement == 24  | departement == 33  | departement == 47  | departement == 40  | departement == 64 ~ "Nouvelle Aquitaine",
            departement  == 46 | departement  == 12 |  departement  == 48 |  departement  == 30 |  departement  == 34 |  departement  == 81 |  departement  == 82 |  departement  == 32 |  departement  == 31 |  departement  == 11 |  departement  == 09 |  departement  == 65 |  departement  == 66 ~ "Occitanie", 
            departement  == 50 | departement  == 14 |  departement  == 61  |  departement  == 27  |  departement  == 76 ~ "Normandie", 
            departement  == 37 | departement  == 41 | departement  == 28 | departement  == 45 | departement  == 36 | departement  == 18 ~ "Centre Val de Loire",
            departement  == 78 | departement  == 95 | departement  == 91 | departement  == 77 | departement  == 75 | departement  == 94 | departement  == 92 | departement  == 93 ~ "Ile de France",
            departement  == 62 | departement  == 59 | departement  == 80 | departement  == 02 | departement  == 60 ~ "Haut de France", 
            departement  == 51 | departement  == 10 | departement  == 08 | departement  == 55 | departement  == 52 | departement  == 54 | departement  == 57 | departement  == 88 | departement  == 67 | departement  == 68 ~ "Grand Est",
            departement  == 89 | departement  == 21 | departement  == 58 | departement  == 71 | departement  == 39 | departement  == 25 | departement  == 70 ~ "Bourgogne Franche Comte", 
            departement  == 03 | departement  == 63 | departement  == 15 | departement  == 43 | departement  == 42 | departement  == 69 | departement  == 07 | departement == 26 | departement == 38 | departement == 01 | departement == 74 | departement == 73 ~ "Auvergne Rhones Alpes",
            departement  == 13 | departement  == 84 | departement  == 04 | departement  == 83 | departement  == 05 | departement  == 06 ~ "Provence Alpes Cote d'Azur",
            departement == 974 ~ "La Reunion", 
            departement == 973 ~ "Guyane", 
            departement == 972 ~ "Martinique", 
            departement == 971 ~ "Guadeloupe",
            departement == 976 ~ "Mayotte",
            TRUE ~ "NA"
        ))

    # Ajout des tranches d'âge
    happiness_survey <- happiness_survey %>%
        mutate(tranche_age = case_when(
            age > 1 & age <= 18 ~ "1-18 ans", 
            age > 18 & age <= 25 ~ "18-25 ans", 
            age > 25 & age <= 50 ~ "25-50 ans", 
            age > 50 & age <= 75 ~ "50-75 ans", 
            TRUE ~ "75 ans et plus"
        ))

    happiness_survey_out_oq <- happiness_survey %>% select(-c(14:19))
}

# Questions
questions <- c(
    "À quoi ces individus accordent-ils plus leur importance pour être heureux ?",
    "L'argent est-il un critère spécifique pour être heureux ?", 
    "Sont-ils plus intéressés par les activités sportives ou les activités créatives ?", 
    "Accordent-ils autant d'importance au travail qu'au temps personnel pour être heureux ?"
)

# Définir l'encodage pour les caractères spéciaux
try({
    Sys.setlocale("LC_ALL", "French_France.UTF-8")
}, silent = TRUE)
