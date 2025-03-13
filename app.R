
list.of.packages <- c("shiny", "shinydashboard", "wordcloud2", "dplyr", "leaflet", 
                     "DT", "readxl", "writexl", "ggplot2", "tidyr", "sf", "viridis")  
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, library, character.only = TRUE)


###UI###

ui <- dashboardPage(
  skin="yellow",
  dashboardHeader(title = span(img(src="img/smile.png", width = 60)) ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Nettoyage du jeu de données", tabName = "clean", icon = icon("hand-sparkles")),
      menuItem("Filtres", tabName = "filtre", icon = icon("filter")),
      menuItem("Analyses statistiques", tabName = "stat", icon = icon("chart-pie")),
      menuItem("Analyses textuelles", tabName = "text", icon = icon("spell-check")),
      menuItem("Rapport", tabName = "report", icon = icon("file-word"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    ")),
    
    tabItems(
      tabItem(tabName = "clean",
              tags$br(),
              img(id = "logo", src="img/happiness.png", width = 140),
              tags$br(),
              tags$br(),
              h1(strong("Nettoyage du jeu de données")),
              h2("Importez votre dataset et laissez la magie opérer ! "),
              h3("Vous pouvez importer votre jeu de données au format Excel ou CSV."),
              h4("Lorsque que le fichier sera importé il sera automatiquement nettoyé, voici une visualisation de ce jeu de données nettoyé."),
              tags$br(),
              fileInput("file", "Importer un fichier"),
              tableOutput("files"),
              dataTableOutput("clean_data"),
              h3("Une fois le fichier importez, vous pouvez le téléchargé nettoyé au format que vous souhaitez."),
              downloadButton("dl", "Download")
      ),
      
      tabItem(tabName = "filtre",
              h1(strong("Choose filters you want to apply on your analyzes")),
              tags$br(),
              radioButtons("radio", "Sexe",
                           selected="Les deux",
                           choiceNames = list(
                             icon("venus-mars"), icon("venus"), icon("mars")),
                           choiceValues = list("Les deux", "Femme", "Homme")),
              selectInput("age", "Age", choices = c("All", "0-18", "19-30", "31-45", "46-60", "60-75", "Over 75")),
              selectInput("department", "Department", choices=c("All", "38", "Autres"))
      ),
      
      tabItem(tabName = "stat",
              h1(strong("Analyses de questions fermées")),
              plotOutput("pie"),
              plotOutput("hist"),
              plotOutput("map")
      ),
      
      tabItem(tabName = "text",
              h1(strong("Analysis of open questions")),
              tags$br(),
              h3(" - What makes you happy ?"),
              wordcloud2Output("word_cloud"),
              h3(" - What is your life's goal ?"),
              plotOutput("bar")
      ),
      
      tabItem(tabName = "report",
              h1(strong("Download your report")),
              tags$br(),
              p(strong("Select analyzes you want to add in your report :")),
              p("- Age"),
              p("- Gender"),
              p("- Depratment"),
              p("- What makes you happy ?"),
              p("- What is your life's goal ?"),
              p("- Add all the questions"),
            fluidPage(
              downloadButton("downloadData","Download"))
      )
    )))



###SERVER###

server <- function(input, output) {
  
  output$files <- renderTable(input$file)
  
  dataset <- reactive({
    infile <- input$file
    if(is.null(infile))
      return(NULL)
    if(!is.null(infile)){
    data = read_excel(infile$datapath)}
    age = grep("âge|age", colnames(data), ignore.case = T)
    colnames(data)[which(names(data) == colnames(data[,grep("âge|age", colnames(data), ignore.case = T)]))] <- "Age"
    for (i in 1:length(data$Age)) {
      #replace par NULL les age <2 et >115

      if (data$Age[i] %in% c(2:115)==FALSE){
        data$Age[i]<- NA}
    }

    # Verification variable department ----------------------------


    i = grep("département|departement", colnames(data), ignore.case = T)

    colnames(data)[which(names(data) == colnames(data[,i]))] <- "Departement"


    data$Departement <- gsub("\\..*","",data$Departement)
    data$Departement = as.integer(data$Departement)


    for (i in 1:length(data$Departement)) {

      #transform code postal en department

      if (nchar(data$Departement[i])==5){
        data$Departement[i] = substr(data$Departement[i],0,2)}

      #replace par NULL les departments non valides

      if (data$Departement[i] %in% c(1:95,971:976)==FALSE){
        data$Departement[i]<- NA}
      
      # if (nchar(data$Departement[i])==1){
      #   data$Departement[i] = paste(0,data$Departement[i])
      # }
      }

    # Recodage variable à choix multiple -------------------------
    ## 1 colonne pour chaque proposition de réponse, codage booléen

    n=grep("Cochez", colnames(data))

    ## Votre famille
    for (i in 1: nrow(data)){
      if (grepl("Votre famille", data[i,n])){
        data$Famille[i] <- 1
      }else{
        data$Famille[i] <- 0
      }}

    ## Vos amis
    for (i in 1: nrow(data)){
      if (grepl("Vos amis", data[i,n])){
        data$Amis[i] <- 1
      }else{
        data$Amis[i] <- 0
      }}

    ##  Vos relations amoureuses
    for (i in 1: nrow(data)){
      if (grepl("Vos relations amoureuses", data[i,n])){
        data$Relations_amoureuses[i] <- 1
      }else{
        data$Relations_amoureuses[i] <- 0
      }}

    ## Vos relations sociales de manière générale
    for (i in 1: nrow(data)){
      if (grepl("Vos relations sociales de manière générale", data[i,n])){
        data$Relations_sociales[i] <- 1
      }else{
        data$Relations_sociales[i] <- 0
      }}


    # Détecter les questions ouvertes -------------------------------------

    sexe = grep("sexe", colnames(data), ignore.case=T )
    colnames(data)[which(names(data) == colnames(data[,sexe]))] <- "Sexe"

    ouverte = grep("qu|comment", colnames(data), ignore.case = T)
    ouverte = subset(ouverte, !ouverte==sexe)

    # Supprimer les réponses de moins de 3 caractères + réponses inutiles

    for (i in ouverte[1]:ouverte[length(ouverte)]){
      for (j in 1:nrow(data)){
        if(nchar(data[j,i]) <= 3){
          data[j,i] <- NA
        }
        if(grepl("aucun|no se|un j|burger king|ne sais pas", data[j,i], ignore.case = T)){
          data[j,i] <- NA
        }
      }}

    
    data
    })

output$clean_data <- renderDataTable({ dataset() })
  
 output$dl <- downloadHandler(
     filename = function() {
       paste('data-', Sys.Date(), '.csv', sep='')
     },
     
     content = function(con) {
       write.csv(dataset(), con)
     })
 
 output$downloadData <- downloadHandler(
        filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
        write.csv(dataset(), con)
        })
 
                          ### Analyses stats ###
 
 #### camenbert
 
 
 output$pie <- renderPlot({
    # S'assurer que les données sont disponibles
    req(dataset())
    
    # Obtenir les données
    data <- dataset()
    
    # Debug: vérifier les données
    print("Données disponibles:")
    print(str(data))
    
    # Filtrer selon le département sélectionné
    filtered_data <- if (input$department == "All") {
        data$Sexe
    } else if (input$department == "Autres") {
        data$Sexe[data$Departement != 38]
    } else {
        data$Sexe[data$Departement == 38]
    }
    
    # Créer la table de fréquences
    freq_table <- table(filtered_data)
    props <- prop.table(freq_table) * 100
    
    # Debug: vérifier les proportions
    print("Proportions calculées:")
    print(props)
    
    # Créer le camembert avec ggplot2
    data_pie <- data.frame(
        category = names(props),
        value = as.numeric(props)
    )
    
    ggplot(data_pie, aes(x = "", y = value, fill = category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(title = "Proportion de répondants selon leur sexe",
             fill = "Sexe") +
        geom_text(aes(label = paste0(round(value, 1), "%")),
                  position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c("red", "orange"))
})

 
 #### histogramme

output$hist <- renderPlot({
    req(dataset())
    data <- dataset()
    
    # Vérifier si la colonne Age existe
    if(!"Age" %in% names(data)) {
        return(ggplot() + 
               geom_text(aes(x = 0, y = 0, label = "Données d'âge non disponibles"), size = 6) +
               theme_void())
    }
    
    # Supprimer les valeurs NA et non numériques
    data$Age <- as.numeric(as.character(data$Age))
    data <- data[!is.na(data$Age),]
    
    # Définir les breaks de manière à ce qu'ils soient uniques
    breaks <- c(0, 18, 30, 45, 60, 75, max(data$Age, na.rm = TRUE))
    
    ggplot(data, aes(x = Age)) +
        geom_histogram(breaks = breaks,
                       fill = "green",
                       color = "black") +
        scale_x_continuous(breaks = breaks) +
        labs(title = "Histogramme des âges",
             x = "Age",
             y = "Fréquence") +
        theme_minimal()
})

 
 #### Map
 
  output$map <- renderPlot({
    # S'assurer que les données sont disponibles
    req(dataset())
    
    # Obtenir les données
    data <- dataset()
    
    # Charger les données géographiques directement depuis GitHub
    france_map <- sf::st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson")
    
    # Calculer les sommes par département
    dept_counts <- as.data.frame(table(data$Departement))
    colnames(dept_counts) <- c("code", "count")
    dept_counts$code <- as.character(dept_counts$code)
    
    # Fusionner avec la carte
    france_map <- merge(france_map, dept_counts, by = "code", all.x = TRUE)
    
    # Remplacer les NA par 0
    france_map$count[is.na(france_map$count)] <- 0
    
    # Créer la carte avec l'échelle de couleurs bleu clair à bleu foncé
    ggplot(data = france_map) +
        geom_sf(aes(fill = count)) +
        scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Nombre de réponses") +
        theme_minimal() +
        labs(title = "Répartition des réponses par département") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              legend.position = "right")
})






  
  output$word_cloud <- renderWordcloud2({
    # Vérifier que dataset() n'est pas null
    req(dataset())
    
    # Récupérer les données
    data <- dataset()
    
    # Vérifier quelle colonne contient les réponses à analyser
    colonne_texte <- "Qu'est ce qui vous rend heureux au quotidien ?"  # Ajustez le nom de la colonne
    
    # Préparation du texte
    texte <- data[[colonne_texte]]
    texte <- tolower(texte)  # Mettre en minuscules
    texte <- gsub("[[:punct:]]", " ", texte)  # Enlever la ponctuation
    texte <- gsub("[[:digit:]]", " ", texte)  # Enlever les chiffres
    texte <- gsub("\r?\n|\r", " ", texte)  # Enlever les retours à la ligne
    texte <- gsub("\\s+", " ", texte)  # Normaliser les espaces
    
    # Suppression des accents
    texte <- iconv(texte, to = "ASCII//TRANSLIT")
    
    # Tokenization et comptage
    words <- unlist(strsplit(texte, " "))
    words <- words[words != ""]  # Enlever les chaînes vides
    
    # Supprimer les mots vides (stopwords)
    stopwords_fr <- c("le", "la", "les", "un", "une", "des", "et", "est", "en", "de", "du", "dans", "pour", "par", "que", "qui", "ce", "ces", "mais", "ou", "où", "donc")  # Ajoutez d'autres mots si nécessaire
    words <- words[!words %in% stopwords_fr]
    
    # Créer le dataframe pour wordcloud2
    word_freq <- as.data.frame(table(words))
    colnames(word_freq) <- c("word", "freq")
    word_freq <- word_freq[order(-word_freq$freq),]
    
    # Filtrer les mots peu fréquents si nécessaire
    word_freq <- word_freq[word_freq$freq > 1,]
    
    # Debug : afficher les premiers mots et leurs fréquences
    print(head(word_freq, 10))
    
    # Créer le nuage de mots
    wordcloud2(word_freq, size = 1, minRotation = -pi/6, maxRotation = pi/6)
})

  
  output$bar <- renderPlot({ 
    # Vérifier que dataset() n'est pas null
    req(dataset())
    
    # Récupérer les données
    data <- dataset()
    
    # Vérifier quelle colonne contient les réponses à analyser
    colonne_texte <- "Quel est votre but dans la vie ?"  # Ajustez le nom de la colonne
    
    # Préparation du texte
    texte <- data[[colonne_texte]]
    texte <- tolower(texte)  # Mettre en minuscules
    texte <- gsub("[[:punct:]]", " ", texte)  # Enlever la ponctuation
    texte <- gsub("[[:digit:]]", " ", texte)  # Enlever les chiffres
    texte <- gsub("\r?\n|\r", " ", texte)  # Enlever les retours à la ligne
    texte <- gsub("\\s+", " ", texte)  # Normaliser les espaces
    
    # Suppression des accents
    texte <- iconv(texte, to = "ASCII//TRANSLIT")
    
    # Tokenization et comptage
    words <- unlist(strsplit(texte, " "))
    words <- words[words != ""]  # Enlever les chaînes vides
    
    # Supprimer les mots vides (stopwords)
    stopwords_fr <- c("le", "la", "les", "un", "une", "des", "et", "est", "en", "de", "du", "dans", 
                     "pour", "par", "que", "qui", "ce", "ces", "mais", "ou", "où", "donc", "vie",
                     "etre", "avoir", "faire")  # Ajoutez d'autres mots si nécessaire
    words <- words[!words %in% stopwords_fr]
    
    # Créer le dataframe pour le graphique
    word_freq <- as.data.frame(table(words))
    colnames(word_freq) <- c("word", "freq")
    word_freq <- word_freq[order(-word_freq$freq),]
    
    # Prendre les 10 premiers mots
    top_words <- head(word_freq, 10)
    
    # Créer le graphique
    par(mar = c(8, 4, 4, 2))  # Ajuster les marges pour les labels
    barplot(top_words$freq, 
            names.arg = top_words$word,
            las = 2,  # Rotation verticale des labels
            col = "lightblue",
            main = "Mots les plus fréquents",
            ylab = "Fréquence",
            cex.names = 0.8)  # Taille des labels
})


 
}


###APP###

shinyApp(ui, server)

