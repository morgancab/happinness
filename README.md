# 😊 Analyse du Bonheur - Application Shiny

## 📝 Description
Cette application Shiny permet d'analyser les résultats d'une enquête sur le bonheur. Elle offre des visualisations interactives et des analyses de données pour explorer les réponses des participants.

## ✨ Fonctionnalités

### 1. 🧹 Nettoyage des Données
- Import de fichiers Excel ou CSV
- Nettoyage automatique des données
- Exportation des données nettoyées

### 2. 🔍 Filtres
- Filtre par sexe
- Filtre par âge
- Filtre par département

### 3. 📊 Analyses Statistiques
- Visualisation de la répartition par sexe (camembert)
- Carte de France interactive montrant la distribution géographique des réponses
- Distribution des âges des répondants

### 4. 📝 Analyses Textuelles
- Nuage de mots pour les réponses à "Qu'est-ce qui vous rend heureux ?"
- Graphique en barres des réponses à "Quel est votre but dans la vie ?"

## 🚀 Installation

### Prérequis
R et RStudio doivent être installés sur votre machine.

### Packages Requis
```R
list.of.packages <- c(
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

# Installation des packages
install.packages(list.of.packages)
