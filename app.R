#! Load packages
#! --------------

library(tidyverse)
library(patchwork)
library(shiny)
library(shinythemes)

#! Data repository
dir_data <- file.path("data")

#! Plot functions
dir_plot_functions <- file.path("R/utils.R")
source(dir_plot_functions)

#! User interface
#! --------------

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Post-traitement des prévisions du modèle GR5H_RI"),

  sidebarLayout(
    sidebarPanel(
      h4("Paramètres globaux"),
      selectInput("period", "Période", choices = c("P1", "P2", "P1 et P2" = "P1_P2"), selected = "P1"),
      selectInput("mode", "Mode des résultats", choices = c("Calage", "Évaluation"), selected = "Évaluation"),
      hr(),
      conditionalPanel(
        condition = "input.main_tabs == 'Scatter plot'",
        h4("Options"),
        selectInput("metric_1", "Métrique de performance", choices = c("NSE", "KGE", "C2MP"), selected = "NSE"),
        h5("Axe x"),
        selectInput("model_x", "Modèle", choices = c("GR5H_RI", "Tangara", "MLP", "Random Forest", "XGBoost"), selected = "MLP"),
        selectInput("cal_method_x", "Méthode de calage", choices = c("WsRf", "Ref", "OL"), selected = "Ref"),
        h5("Axe y"),
        selectInput("model_y", "Modèle", choices = c("GR5H_RI", "Tangara", "MLP", "Random Forest", "XGBoost"), selected = "GR5H_RI"),
        selectInput("cal_method_y", "Méthode de calage", choices = c("WsRf", "Ref", "OL"), selected = "WsRf")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Boxplot'",
        h4("Options"),
        selectInput("metric_2", "Métrique de performance", choices = c("NSE", "KGE", "C2MP"), selected = "NSE"),
        selectInput("cal_method", "Méthode de calage", choices = c("WsRf", "Ref", "OL"), selected = "WsRf")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Temps de réponse'",
        h4("Options"),
        selectInput("metric_3", "Métrique de performance", choices = c("NSE", "KGE", "C2MP"), selected = "NSE"),
        selectInput("cal_method_2", "Méthode de calage", choices = c("WsRf", "Ref", "OL"), selected = "WsRf"),
        selectInput("mask", "Masque temporel", choices = c("Performance globale", "Évènements de crue"), selected = "Performance globale"),
        selectInput("Tr", "Temps de réponse", choices = c("Tr [1, 9] h", "Tr [10, 18] h", "Tr [19, 128] h"), selected = "Tr [19, 128] h")
      )
    ),
    mainPanel(
      tabsetPanel(id = "main_tabs",
        tabPanel(
          "Home",
          br(),
          div(style = "text-align: justify;", HTML("
            <p>Cette application présente les principaux résultats d'une étude sur l'application des techniques de post-traitement visant à améliorer la performance du modèle de prévision hydrologique GR5H_RI <a href='https://theses.hal.science/tel-04028903' target='_blank'>(Astagneau, 2022)</a>.</p>
          "))
        ),
        tabPanel(
          "Scatter plot",
          br(),
          plotOutput("scatter_plot_nse_kge_c2mp", height = "500", width = "100%"),
          plotOutput("scatter_plot_cont_table", height = "700", width = "100%")
        ),
        tabPanel(
          "Boxplot",
          br(),
          plotOutput("boxplot_nge_kge_c2mp", height = "600px", width = "100%"),
          plotOutput("boxplot_nse_kge_c2mp_cal_method", height = "600px", width = "100%"),
          plotOutput("boxplot_ct", height = "600px", width = "100%")
        ),
        tabPanel(
          "Temps de réponse",
          br(),
          plotOutput("boxplot_tr_cal_method", height = "600px", width = "100%"),
          plotOutput("boxplot_tr_long_WsRf_OL", height = "600px", width = "100%")
        )
      )
    )
  )
)

#! Server
#! ------

server <- function(input, output, session) {

  results_NSE_KGE_C2MP <- shiny::reactive({
    read_rds(file.path(dir_data, paste0("results_NSE_KGE_C2MP_", input$period, ".rds")))
  })

  results_CT <- shiny::reactive({
    read_rds(file.path(dir_data, paste0("results_cont_table_", input$period, ".rds")))
  })

  scatter_plot_NSE_KGE_C2MP <- shiny::reactive({
    scatter_plot_nse_kge_c2mp(
      results_NSE_KGE_C2MP(), input$metric_1, input$mode, 
      input$model_x, input$cal_method_x, input$model_y, input$cal_method_y
    )
  })
  output$scatter_plot_nse_kge_c2mp <- renderPlot(scatter_plot_NSE_KGE_C2MP(), res = 96)

  scatter_plot_CT <- shiny::reactive({
    scatter_plot_cont_table(
      results_CT(), input$mode, input$model_x, input$cal_method_x, input$model_y, input$cal_method_y
    )
  })
  output$scatter_plot_cont_table <- renderPlot(scatter_plot_CT(), res = 96)

  boxplot_NSE_KGE_C2MP <- shiny::reactive({
    boxplot_nge_kge_c2mp(
      results_NSE_KGE_C2MP(), input$metric_2, input$cal_method
    )
  })
  output$boxplot_nge_kge_c2mp <- renderPlot(boxplot_NSE_KGE_C2MP(), res = 96)

  boxplot_NSE_KGE_C2MP_cal_method <- shiny::reactive({
    boxplot_nse_kge_c2mp_cal_method(
      results_NSE_KGE_C2MP(), input$mode, input$metric_2
    )
  })
  output$boxplot_nse_kge_c2mp_cal_method <- renderPlot(boxplot_NSE_KGE_C2MP_cal_method(), res = 96)

  boxplot_CT <- shiny::reactive({
    boxplot_cont_table(
      results_CT(), input$mode, input$cal_method
    )
  })
  output$boxplot_ct <- renderPlot(boxplot_CT(), res = 96)

  boxplot_Tr_cal_method <- shiny::reactive({
    boxplot_tr_cal_method(
      results_NSE_KGE_C2MP(), input$metric_3, input$mode, input$cal_method_2, input$mask
    )
  })

  output$boxplot_tr_cal_method <- renderPlot(boxplot_Tr_cal_method(), res = 96)

  boxplot_Tr_long_WsRf_OL <- shiny::reactive({
    boxplot_tr_long_WsRf_OL(
      results_NSE_KGE_C2MP(), input$mode, input$metric_3, input$mask, input$Tr
    )
  })

  output$boxplot_tr_long_WsRf_OL <- renderPlot(boxplot_Tr_long_WsRf_OL(), res = 96)



}

shiny::shinyApp(ui, server)
