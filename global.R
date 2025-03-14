# global.R

# Configuration du dépôt CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Packages requis
required_packages <- c(
    "shiny",
    "shinydashboard",
    "wordcloud2",
    "dplyr",
    "leaflet",
    "DT",
    "readxl",
    "writexl",
    "ggplot2",
    "tidyr",
    "sf"
)

# Installation et chargement des packages
for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    }
    library(pkg, character.only = TRUE, quietly = TRUE)
}

# Charger les packages avec gestion d'erreur
tryCatch({
    load_packages()
}, error = function(e) {
    message("Erreur lors du chargement des packages : ", e$message)
    stop(e)
})

## Configuration de base
options(
    encoding = "UTF-8",
    shiny.maxRequestSize = 30*1024^2
)

## DATA ------------------------------------

happiness_survey <- tryCatch(
    readxl::read_excel("data/happiness_survey.xlsx"),
    error = function(e) {
        message("Erreur lors de la lecture du fichier: ", e$message)
        return(NULL)
    }
)

if (is.null(happiness_survey)) {
    stop("Impossible de charger les données. Vérifiez le fichier happiness_survey.xlsx")
}

# Reste de votre code avec une meilleure gestion des erreurs
tryCatch({
    # Initialisation des colonnes avec vectorisation
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

    # Le reste de votre code pour le traitement des données...
    happiness_survey_oq <- happiness_survey %>% select(c(14:19))
    happiness_survey$departement <- as.integer(happiness_survey$departement)

    # Ajout des régions
    happiness_survey <- happiness_survey %>% 
        mutate(region = case_when(
            # Votre code existant pour les régions...
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

}, error = function(e) {
    message("Erreur lors du traitement des données : ", e$message)
    stop(e)
})

# Questions
questions <- c(
    "À quoi ces individus accordent-ils plus leur importance pour être heureux ?",
    "L'argent est-il un critère spécifique pour être heureux ?", 
    "Sont-ils plus intéressés par les activités sportives ou les activités créatives ?", 
    "Accordent-ils autant d'importance au travail qu'au temps personnel pour être heureux ?"
)

# Définir l'encodage pour les caractères spéciaux
tryCatch({
    Sys.setlocale("LC_ALL", "French_France.UTF-8")
}, error = function(e) {
    message("Attention : Problème d'encodage des caractères spéciaux")
})

# Vérification finale
if (!exists("happiness_survey") || !exists("happiness_survey_out_oq")) {
    stop("Initialisation incomplète des données")
}
