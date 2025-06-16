
"""Programme Shiny pour un Dashboard Médical Interactif"""

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



# ================  Filtrage des valeurs aberrantes ==========================================================================================
df <- df %>% 
  filter(!(result == "Pathologique" & ck_mb > 20)) %>%  # CK-MB > 20 pour pathologiques
  filter(troponin <= 2.5)  # Troponine > 2.5

# 🚨 Filtrage des valeurs selon critères médicaux spécifiques
print(paste("Nombre de lignes avant filtrage:", nrow(df)))

df <- df %>%
 filter(
   troponin <= 0.35,                    # Troponine ≤ 1.0 ng/mL
   ck_mb <= 10,                        # CK-MB ≤ 10
   diastolic_blood_pressure <= 100,    # Pression diastolique ≤ 100 mmHg
   blood_sugar <= 300,                 # Glycémie ≤ 300 mg/dL
   systolic_blood_pressure <= 170,     # Pression systolique ≤ 170 mmHg
   heart_rate <= 110                   # Fréquence cardiaque ≤ 110 bpm
 )

print(paste("Nombre de lignes après filtrage:", nrow(df)))

#=============================================================================================================================================



# 🎨 Interface Utilisateur (UI)
ui <- dashboardPage(
  # En-tête du dashboard
  dashboardHeader(title = "Dashboard Médical Interactif"),
  
  # Barre latérale avec filtres
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue d'ensemble", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Analyses par diagnostic", tabName = "by_diagnosis", icon = icon("stethoscope")),
      menuItem("Corrélations", tabName = "correlations", icon = icon("project-diagram")),
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
               max = 900,
               value = c(min(df$heart_rate, na.rm = TRUE), 900),
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
          
          box(width = 6, title = "Répartition par genre", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("gender_pie"))
        ),
        
        fluidRow(
          box(width = 12, title = "Statistiques résumées", 
              status = "info", solidHeader = TRUE,
              DT::dataTableOutput("summary_stats"))
        )
      ),
      
      # 🔬 Onglet Analyses par diagnostic
      tabItem(tabName = "by_diagnosis",
        fluidRow(
          box(width = 6, title = "Fréquence cardiaque par diagnostic", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("heart_rate_violin")),
          
          box(width = 6, title = "Pression systolique par diagnostic", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("systolic_bp_violin"))
        ),
        
        fluidRow(
          box(width = 6, title = "Pression diastolique par diagnostic", 
              status = "success", solidHeader = TRUE,
              plotlyOutput("diastolic_bp_violin")),
          
          box(width = 6, title = "Glycémie par diagnostic", 
              status = "success", solidHeader = TRUE,
              plotlyOutput("blood_sugar_violin"))
        ),
        
        fluidRow(
          box(width = 6, title = "CK-MB par diagnostic", 
              status = "warning", solidHeader = TRUE,
              plotlyOutput("ckms_boxplot")),
          
          box(width = 6, title = "Troponine par diagnostic", 
              status = "danger", solidHeader = TRUE,
              plotlyOutput("troponin_violin"))
        )
      ),
      
      # 📈 ===============  Onglet Corrélations  ====================================================================================
      tabItem(tabName = "correlations",
        fluidRow(
          box(width = 6, title = "Fréquence cardiaque vs Pression systolique", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("hr_vs_sys")),
          
          box(width = 6, title = "Fréquence cardiaque vs Pression diastolique", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("hr_vs_dia"))
        ),
        
        fluidRow(
          box(width = 6, title = "Fréquence cardiaque vs Glycémie", 
              status = "info", solidHeader = TRUE,
              plotlyOutput("hr_vs_sugar")),
          
          box(width = 6, title = "Fréquence cardiaque vs CK-MB", 
              status = "info", solidHeader = TRUE,
              plotlyOutput("hr_vs_ckms"))
        ),
        
        fluidRow(
          box(width = 6, title = "Fréquence cardiaque vs Troponine", 
              status = "warning", solidHeader = TRUE,
              plotlyOutput("hr_vs_trop")),
          
          box(width = 6, title = "Pression systolique vs Pression diastolique", 
              status = "warning", solidHeader = TRUE,
              plotlyOutput("sys_vs_dia"))
        ),
        
        fluidRow(
          box(width = 6, title = "Pression systolique vs Glycémie", 
              status = "success", solidHeader = TRUE,
              plotlyOutput("sys_vs_sugar")),
          
          box(width = 6, title = "Pression systolique vs CK-MB", 
              status = "success", solidHeader = TRUE,
              plotlyOutput("sys_vs_ckms"))
        ),
        
        fluidRow(
          box(width = 6, title = "Pression systolique vs Troponine", 
              status = "danger", solidHeader = TRUE,
              plotlyOutput("sys_vs_trop")),
          
          box(width = 6, title = "Pression diastolique vs Glycémie", 
              status = "danger", solidHeader = TRUE,
              plotlyOutput("dia_vs_sugar"))
        ),
        
        fluidRow(
          box(width = 6, title = "Pression diastolique vs CK-MB", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("dia_vs_ckms")),
          
          box(width = 6, title = "Pression diastolique vs Troponine", 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("dia_vs_trop"))
        ),
        
        fluidRow(
          box(width = 6, title = "Glycémie vs CK-MB", 
              status = "info", solidHeader = TRUE,
              plotlyOutput("sugar_vs_ckms")),
          
          box(width = 6, title = "Glycémie vs Troponine", 
              status = "info", solidHeader = TRUE,
              plotlyOutput("sugar_vs_trop"))
        ),
        
        fluidRow(
          box(width = 6, title = "CK-MB vs Troponine", 
              status = "warning", solidHeader = TRUE,
              plotlyOutput("ckms_vs_trop"))
        )
      ),
      #============================================================================================================================
      
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
  
  # 📈 ========================================  Graphiques par diagnostic  =======================================================
  output$age_distribution <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = age, fill = result)) +
      geom_histogram(bins = 15, position = "dodge", alpha = 0.8) +
      labs(title = "Distribution de l'âge selon le diagnostic", 
           x = "Âge", y = "Nombre de patients") +
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
  
  # Graphiques par diagnostic (violin plots)
  output$heart_rate_violin <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = result, y = heart_rate, fill = result)) +
      geom_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.8) +
      labs(title = "Distribution de la fréquence cardiaque", 
           x = "Diagnostic", y = "Fréquence cardiaque (bpm)") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$systolic_bp_violin <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = result, y = systolic_blood_pressure, fill = result)) +
      geom_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.8) +
      labs(title = "Distribution de la pression systolique", 
           x = "Diagnostic", y = "Pression systolique (mmHg)") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$diastolic_bp_violin <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = result, y = diastolic_blood_pressure, fill = result)) +
      geom_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.8) +
      labs(title = "Distribution de la pression diastolique", 
           x = "Diagnostic", y = "Pression diastolique (mmHg)") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$blood_sugar_violin <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = result, y = blood_sugar, fill = result)) +
      geom_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.8) +
      labs(title = "Distribution de la glycémie", 
           x = "Diagnostic", y = "Glycémie (mg/dL)") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$ckms_boxplot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = result, y = ck_mb, fill = result)) +
      geom_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.8) +
      labs(title = "Distribution du CK-MB", 
           x = "Diagnostic", y = "CK-MB (ng/mL)") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$troponin_violin <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = result, y = troponin, fill = result)) +
      geom_violin(alpha = 0.8) +
      geom_boxplot(width = 0.2, alpha = 0.8) +
      labs(title = "Distribution de la troponine", 
           x = "Diagnostic", y = "Troponine (ng/mL)") +
      theme_minimal() +
      scale_fill_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # 📈 =========================================  Graphiques de corrélation (15 comparaisons)  ========================================================
  output$hr_vs_sys <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = heart_rate, y = systolic_blood_pressure, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Fréquence cardiaque (bpm)", y = "Pression systolique (mmHg)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$hr_vs_dia <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = heart_rate, y = diastolic_blood_pressure, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Fréquence cardiaque (bpm)", y = "Pression diastolique (mmHg)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$hr_vs_sugar <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = heart_rate, y = blood_sugar, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Fréquence cardiaque (bpm)", y = "Glycémie (mg/dL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$hr_vs_ckms <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = heart_rate, y = ck_mb, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Fréquence cardiaque (bpm)", y = "CK-MB (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$hr_vs_trop <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = heart_rate, y = troponin, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Fréquence cardiaque (bpm)", y = "Troponine (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$sys_vs_dia <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = systolic_blood_pressure, y = diastolic_blood_pressure, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Pression systolique (mmHg)", y = "Pression diastolique (mmHg)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$sys_vs_sugar <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = systolic_blood_pressure, y = blood_sugar, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Pression systolique (mmHg)", y = "Glycémie (mg/dL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$sys_vs_ckms <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = systolic_blood_pressure, y = ck_mb, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Pression systolique (mmHg)", y = "CK-MB (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$sys_vs_trop <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = systolic_blood_pressure, y = troponin, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Pression systolique (mmHg)", y = "Troponine (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$dia_vs_sugar <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = diastolic_blood_pressure, y = blood_sugar, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Pression diastolique (mmHg)", y = "Glycémie (mg/dL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$dia_vs_ckms <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = diastolic_blood_pressure, y = ck_mb, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Pression diastolique (mmHg)", y = "CK-MB (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$dia_vs_trop <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = diastolic_blood_pressure, y = troponin, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Pression diastolique (mmHg)", y = "Troponine (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$sugar_vs_ckms <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = blood_sugar, y = ck_mb, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Glycémie (mg/dL)", y = "CK-MB (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$sugar_vs_trop <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = blood_sugar, y = troponin, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Glycémie (mg/dL)", y = "Troponine (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  output$ckms_vs_trop <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = ck_mb, y = troponin, color = result)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "CK-MB (ng/mL)", y = "Troponine (ng/mL)") +
      theme_minimal() +
      scale_color_manual(values = c("Sain" = "#2E8B57", "Pathologique" = "#DC143C"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # 📋 =================================================  Tableaux de données  ==========================================================
  output$summary_stats <- DT::renderDataTable({
    filtered_data() %>%
      group_by(result) %>%
      summarise(
        Nombre = n(),
        Age_Moyen = round(mean(age, na.rm = TRUE), 1),
        Age_Mediane = round(median(age, na.rm = TRUE), 1),
        Frequence_Cardiaque_Moyenne = round(mean(heart_rate, na.rm = TRUE), 1),
        Pression_Systolique_Moyenne = round(mean(systolic_blood_pressure, na.rm = TRUE), 1),
        Pression_Diastolique_Moyenne = round(mean(diastolic_blood_pressure, na.rm = TRUE), 1),
        Glycemie_Moyenne = round(mean(blood_sugar, na.rm = TRUE), 1),
        CK_MB_Moyenne = round(mean(ck_mb, na.rm = TRUE), 2),
        Troponine_Moyenne = round(mean(troponin, na.rm = TRUE), 3),
        .groups = 'drop'
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$filtered_data <- DT::renderDataTable({
    filtered_data()
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # 💾 =====================================  Téléchargement des données  =======================================================
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