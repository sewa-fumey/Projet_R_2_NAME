

# 📋 Installation des packages nécessaires (à exécuter une seule fois)
# install.packages(c("shiny", "shinydashboard", "DT", "plotly", "tidyverse", "janitor"))

# 📚 Chargement des librairies
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(janitor)

# 📁 Chargement et préparation des données
df <- read.csv("donnees_medicales.csv")
df <- clean_names(df)

# 🔄 Transformation des données
df$gender <- factor(df$gender, levels = c(0, 1), labels = c("Femme", "Homme"))
df$result <- factor(df$result, levels = c("negative", "positive"), labels = c("Sain", "Pathologique"))
df <- df %>% distinct()
df <- df %>% filter(ck_mb < 50)  # Filtrage des valeurs extrêmes

# 🎨 Interface Utilisateur (UI)
ui <- dashboardPage(
  # En-tête du dashboard
  dashboardHeader(title = "Dashboard Médical Interactif"),
  
  # Barre latérale avec filtres
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue d'ensemble", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Analyse détaillée", tabName = "detailed", icon = icon("microscope")),
      menuItem("Données brutes", tabName = "data", icon = icon("table"))
    ),
    
    # 🎛️ Filtres dynamiques
    h4("Filtres", style = "color: white; padding-left: 15px;"),
    
    # Filtre par genre
    checkboxGroupInput("gender_filter", 
                      "Genre :",
                      choices = levels(df$gender),
                      selected = levels(df$gender)),
    
    # Filtre par diagnostic
    checkboxGroupInput("result_filter", 
                      "Diagnostic :",
                      choices = levels(df$result),
                      selected = levels(df$result)),
    
    # Filtre par âge
    sliderInput("age_filter", 
               "Tranche d'âge :",
               min = min(df$age, na.rm = TRUE),
               max = max(df$age, na.rm = TRUE),
               value = c(min(df$age, na.rm = TRUE), max(df$age, na.rm = TRUE)),
               step = 1),
    
    # Filtre par fréquence cardiaque
    sliderInput("heart_rate_filter", 
               "Fréquence cardiaque :",
               min = min(df$heart_rate, na.rm = TRUE),
               max = max(df$heart_rate, na.rm = TRUE),
               value = c(min(df$heart_rate, na.rm = TRUE), max(df$heart_rate, na.rm = TRUE)),
               step = 1)
  ),
  
  # Corps principal du dashboard
  dashboardBody(
    tabItems(
      # 📊 Onglet Vue d'ensemble
      tabItem(tabName = "overview",
        fluidRow(
          # Boîtes d'information
          valueBoxOutput("total_patients"),
          valueBoxOutput("avg_age"),
          valueBoxOutput("pathological_rate")
        ),
        
        fluidRow(
          box(width = 6, title = "Distribution de l'âge par diagnostic", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("age_distribution")),
          
          box(width = 6, title = "CK-MB selon le diagnostic", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("ckms_boxplot"))
        ),
        
        fluidRow(
          box(width = 12, title = "Statistiques résumées", 
              status = "info", solidHeader = TRUE,
              DT::dataTableOutput("summary_stats"))
        )
      ),
      
      # 🔬 Onglet Analyse détaillée
      tabItem(tabName = "detailed",
        fluidRow(
          box(width = 6, title = "Corrélation Age vs Fréquence Cardiaque", 
              status = "warning", solidHeader = TRUE,
              plotlyOutput("correlation_plot")),
          
          box(width = 6, title = "Distribution de la glycémie", 
              status = "warning", solidHeader = TRUE,
              plotlyOutput("blood_sugar_plot"))
        ),
        
        fluidRow(
          box(width = 6, title = "Répartition par genre", 
              status = "success", solidHeader = TRUE,
              plotlyOutput("gender_pie")),
          
          box(width = 6, title = "Fréquence cardiaque par diagnostic", 
              status = "success", solidHeader = TRUE,
              plotlyOutput("heart_rate_violin"))
        )
      ),
      
      # 📋 Onglet Données brutes
      tabItem(tabName = "data",
        fluidRow(
          box(width = 12, title = "Données filtrées", 
              status = "primary", solidHeader = TRUE,
              DT::dataTableOutput("filtered_data"),
              downloadButton("download_data", "Télécharger les données", 
                           class = "btn-primary"))
        )
      )
    )
  )
)

# 🔧 Serveur (logique backend)
server <- function(input, output, session) {
  
  # 🎯 Données réactives basées sur les filtres
  filtered_data <- reactive({
    df %>%
      filter(
        gender %in% input$gender_filter,
        result %in% input$result_filter,
        age >= input$age_filter[1] & age <= input$age_filter[2],
        heart_rate >= input$heart_rate_filter[1] & heart_rate <= input$heart_rate_filter[2]
      )
  })
  
  # 📊 Boîtes d'information
  output$total_patients <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Patients totaux",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_age <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$age, na.rm = TRUE), 1),
      subtitle = "Âge moyen",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$pathological_rate <- renderValueBox({
    rate <- round(sum(filtered_data()$result == "Pathologique") / nrow(filtered_data()) * 100, 1)
    valueBox(
      value = paste0(rate, "%"),
      subtitle = "Taux pathologique",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  # 📈 Graphiques interactifs
  output$age_distribution <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = age, fill = result)) +
      geom_histogram(bins = 15, position = "dodge", alpha = 0.8) +
      labs(title = "Distribution de l'âge selon le diagnostic", 
           x = "Âge", y = "Nombre de patients") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$ckms_boxplot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = result, y = ck_mb, fill = result)) +
      geom_boxplot(alpha = 0.8) +
      labs(title = "CK-MB selon le diagnostic", 
           x = "Diagnostic", y = "CK-MB (ng/mL)") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$correlation_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = age, y = heart_rate, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Corrélation Age vs Fréquence Cardiaque", 
           x = "Âge", y = "Fréquence cardiaque") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$blood_sugar_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = blood_sugar, fill = result)) +
      geom_density(alpha = 0.6) +
      labs(title = "Distribution de la glycémie", 
           x = "Glycémie", y = "Densité") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$gender_pie <- renderPlotly({
    gender_counts <- filtered_data() %>% 
      count(gender) %>%
      mutate(percentage = round(n/sum(n)*100, 1))
    
    plot_ly(gender_counts, labels = ~gender, values = ~n, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hovertemplate = "%{label}: %{value} patients<br>%{percent}<extra></extra>") %>%
      layout(title = "Répartition par genre")
  })
  
  output$heart_rate_violin <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = result, y = heart_rate, fill = result)) +
      geom_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.8) +
      labs(title = "Distribution de la fréquence cardiaque", 
           x = "Diagnostic", y = "Fréquence cardiaque") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # 📋 Tableaux de données
  output$summary_stats <- DT::renderDataTable({
    filtered_data() %>%
      group_by(result) %>%
      summarise(
        Nombre = n(),
        Age_Moyen = round(mean(age, na.rm = TRUE), 1),
        Age_Mediane = round(median(age, na.rm = TRUE), 1),
        Frequence_Cardiaque_Moyenne = round(mean(heart_rate, na.rm = TRUE), 1),
        Glycemie_Moyenne = round(mean(blood_sugar, na.rm = TRUE), 1),
        CK_MB_Moyenne = round(mean(ck_mb, na.rm = TRUE), 2),
        .groups = 'drop'
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$filtered_data <- DT::renderDataTable({
    filtered_data()
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # 💾 Téléchargement des données
  output$download_data <- downloadHandler(
    filename = function() {
      paste("donnees_medicales_filtrees_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# 🚀 Lancement de l'application
shinyApp(ui = ui, server = server)