library ( shiny )
library(ggplot2)

shinyServer ( function (input , output ) {
  
  # Alert bloc for variables
  Check_variable <- function() {
    blocN = '';
    blocM = '';
    if (is.na(input$n)) {
      blocN = 'N'
    }
    else { if (input$n <= 0) {
      blocN = 'Neg'
    }}
    if (is.na(input$m)) {
      blocM = 'M'
    }
    return(c("N"=blocN, "M"=blocM))
  }
  
  CreateAlerte <- function(message) {
    return(
      HTML(paste("<div class='alert'> <span class='closebtn' onclick=",'"',"this.parentElement.style.display='none';",'"',
                 ">&times;</span> <strong>",
                 message,
                 "</strong> </div>"))
    )
  }
  
  # Creation of data
  LancerPiece <- function() {
    return(rbinom(1, 1, 0.5))
  }
  
  Gain <- function(gain_precedent) {
    X = LancerPiece()
    if (X == 0) {
      if (gain_precedent == 0) {
        return(Gain(1))
      }
      else {
        return(Gain(2*gain_precedent))
      }
    }
    else {
      return(gain_precedent)
    }
  }
  
  Tirage <- function(n) {
    T = c()
    for (i in 1:n) {
      T = c(T, Gain(0))
    }
    return(T)
  }
  
  Simulate <- eventReactive(input$go, {
    HTML(paste(
    box(background = "black", width=12,
        HTML("<h2> Résultats :</h2>" ),
        
        fluidRow(
          infoBox("Mise de départ", width = 6, input$m, icon = icon("list"), color="blue", fill=TRUE),
          infoBox("Nombre de tirages", width = 6, input$n, icon = icon("list"), color="blue", fill=TRUE),
        ),
        
        {
        check = Check_variable();
        blocN = "";
        blocM = "";
        
        if (check["N"] == "" && check["M"] == "") {
          
          # Tirage
          T = Tirage(input$n);
          couleur = "#F8766D";
          fluidRow(
            tabBox(
              tabPanel(title="Histogramme", 
                 renderPlot({
                   ggplot() + aes(T) + geom_histogram(fill=couleur) + geom_vline(xintercept = mean(T), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Gain") + scale_y_continuous(name="Proportion")
                 })
              ),
              tabPanel(title="Boîte à moustache",
                 renderPlot({
                   ggplot() + aes(T) + geom_boxplot(fill=couleur) + scale_x_continuous(name="Gain") + scale_y_continuous(name="Proportion")
                 })
              )
            ),
            
            box(
              title = "info", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              HTML(
                paste(
                  paste('<p class="info">',"moyenne : ", round(mean(T), digits=2) ),
                  paste("min : ", min(T)),
                  paste("1er quartile : ", quantile(T, 0.25)),
                  paste("mediane : ", median(T)),
                  paste("3eme quartile : ", quantile(T, 0.75)),
                  paste("max : ", max(T)),
                  paste("ecart-type :", sqrt(var(T))),
                  sep="<br>"
                )
            ))
          )
        }
        else {
        if (check["N"] == "N") {
          blocN = CreateAlerte("Attention, le nombre de tirages (n) n'est pas défini")
        }
        if (check["N"] == "Neg") {
          blocN =CreateAlerte("Attention, le nombre de tirages (n) est inférieur ou égal à 0")
        }
        if (check["M"] == "M") {
          blocM = CreateAlerte("Attention, la mise de départ (m) est non defini")
        }
        HTML(paste(blocM, blocN))
        }
        }
        
    )))
  })
  
  output$result <- renderText({
    Simulate()
  })


})
