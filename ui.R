library(shiny)
library(shinydashboard)
library(markdown)
library(shinyjs)

ui <- shinyUI(dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML('.content-wrapper { background-color: #4e4e4e; } ' ))),
    
    includeCSS("css/style.css"),
    ## HOME PAGE
    tabItem(tabName = "home",
            h1("Jeu du lancer de pièces infinies"),
            
            fluidRow(
              
              box(
              width = 12,
              title = "Simulation", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              HTML(paste(
                "<p>On lance une pièce (idéale) jusqu’à obtenir face. Dès qu’on obtient face, on gagne 2<sup>n</sup> € où n est le nombre de lancers effectués</p>",
                "<p>Pour ce jeu, on peut mettre une mise de départ m</p>",
                "<p>On choisit le nombre n de tirages</p>",
                "<p>On simulera alors n joueurs ayant joués au jeu. On pourra observer leurs gains et la moyenne des gains sur les n tirages</p>"
                ))
              ),
            ),
            
            fluidRow(
              box(
                background = "black",
                numericInput("m", "Mise de départ", value = 0, min=0),
              ),
              box(
                background = "black",
                numericInput("n", "Nombre de tirages", value = 1, min=0),
              ),
            ),
            
            fluidRow(
              column(width=2),
              column(width=8,
                actionButton("go", "Lancer la simulation", class="btn-primary btn-lg btn-block")
              ),
              column(width=2)
            ),
            
            HTML("<p></p>"),
            
            fluidRow(
              htmlOutput("result"),
            ),
            
            HTML("<p></p>"),
          
            fluidRow(
              HTML("<footer class='footer text-faded text-center py-5'> <div class='container'><p class='m-0 small'>Copyright © Jeu du lancer de pièces infinies <br>Page réalisée par Hamza ABDOULHOUSSEN</p></div> </footer>")
            )
            
    )
  )
))

  