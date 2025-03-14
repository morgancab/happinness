# dependencies.R

# Définir le miroir CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Liste des packages requis
packages <- c(
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

# Installer les packages manquants
for(pkg in packages) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}
