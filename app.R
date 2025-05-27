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
            
            <p>Cette étude s'appuie sur la base de données développée par <a href='https://www.sciencedirect.com/science/article/pii/S0022169424015154' target='_blank'>Astagneau et al. (2024)</a>, qui compile des informations sur un large échantillon de 687 bassins situés en France métropolitaine.</p>
            
            <p>Les prévisions de débits générées par le modèle GR5H_RI pour ces bassins sont corrigées à l’aide de techniques de post-traitement basées sur l'utilisation de modèles de machine learning, tels que XGBoost, Random Forest et MLP (Multilayer Perceptron), afin d’améliorer la précision des prévisions et de mieux capter les dynamiques hydrologiques observées.</p>

            <p>Les résultats sont disponibles pour différents horizons de prévision (3h, 6h, 12h et 24h), et selon trois options de calage du modèle GR5H_RI :</p>

            <ul>
              <li><strong>Ref :</strong> méthode de calage dite de référence. Tous les paramètres du modèle sont ajustés en mode de simulation (sans assimilation des débits observés). Le modèle intègre les dernières observations à chaque pas de temps de prévision pour mettre à jour ses états internes.</li>

              <li><strong>WsRf :</strong> méthode de calage mixte. Certains paramètres du modèle sont calibrés en mode de simulation (ceux liés au bilan hydrique) et d'autres en fonction d'un horizon de calage spécifique. Le modèle intègre les dernières observations à chaque pas de temps de prévision pour mettre à jour ses états internes.</li>

              <li><strong>OL :</strong> prévisions en mode de simulation, avec les paramètres calibrés selon la méthode de référence, mais sans assimilation des débits observés pour la mise à jour des états internes du modèle.</li>
            </ul>

            <p>Un découpage temporel habituel est mis en place pour suivre une approche de calage-contrôle : deux sous-périodes indépendantes, contenant environ le même nombre de données de débits observés, sont définies pour chaque bassin versant, et sont désignées sous les noms de « P1 » et « P2 ». Ainsi, les résultats des prévisions peuvent être obtenus de deux manières :</p>

            <ul>
              <li><strong>En mode de calage :</strong> le modèle hydrologique est utilisé pour prévoir les débits de la même sous-période utilisée pour le calage de ses paramètres.</li>
              <li><strong>En mode d’évaluation :</strong> le modèle hydrologique est appliqué pour prévoir les débits de la sous-période indépendante de celle utilisée pour le calage de ses paramètres.</li>
            </ul>

            <h4>Modèles de post-traitement</h4>
            <p>Les corrections des débits sont obtenues à l'aide de plusieurs modèles de machine learning, incluant :</p>

            <ul>
              <li>Des arbres décisionnels (<em>XGBoost, Random Forest</em>),</li>
              <li>Des réseaux de neurones artificiels (<em>MLP – Multilayer Perceptron</em>).</li>
            </ul>

            <p>En complément, la méthode de Tangara, actuellement utilisée pour la correction des prévisions du modèle <a href='https://webgr.inrae.fr/outils/modeles-hydrologiques/modele-de-prevision-hydrologique-grp' target='_blank'>GRP</a>, a également été prise en compte pour l'évaluation des résultats à titre de comparaison.</p>

            <h4>Méthodologie de correction des débits</h4>
            <p>Les corrections sont réalisées via un des modèles de machine learning choisis. L'objectif de ces modèles est de prédire les erreurs commises par le modèle GR5H_RI à un horizon de prévision spécifié pour chaque bassin de la base de données. Les modèles sont entraînés sur une des sous-périodes (P1 ou P2) et validés sur la sous-période indépendante de celle utilisée pour l'entraînement.</p>

            <p><strong>Variables :</strong></p>
            <ul>
              <li><strong>Prédictives :</strong> 
                <ul>
                  <li>erreur commise par le modèle à l’instant <em>t-1</em> (prévision faite à <em>t-2</em>),</li>
                  <li>erreur commise par le modèle à l’instant <em>t</em> (prévision faite à <em>t-1</em>),</li>
                  <li>débit observé à <em>t-1</em>,</li>
                  <li>débit observé à <em>t</em>.</li>
                  <li>gradient des débits observé <em>Qt - Qt-1</em>.</li>
                </ul>
              </li>
              <li><strong>Cible :</strong> erreur commise par le modèle à l’instant <em>t+H</em>, soit la différence entre la prévision faite à <em>t</em> pour <em>t+H</em> et le débit observé à <em>t+H</em>.</li>
            </ul>

            <hr>
            <p style='text-align: center;'><strong>Auteurs :</strong> Gustavo Gabbardo & François Bourgin</p>
            <p style='text-align: center;'>
              <i class='fa fa-envelope'></i> 
              <a href='mailto:gustavo.gabbardodosreis@inrae.fr'>gustavo.gabbardodosreis@inrae.fr</a> &nbsp;|&nbsp;
              <i class='fa fa-envelope'></i> 
              <a href='mailto:francois.bourgins@inrae.fr'>francois.bourgins@inrae.fr</a>
            </p>
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
