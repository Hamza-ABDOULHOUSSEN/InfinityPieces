library ( shiny )
library(ggplot2)

shinyServer ( function (input , output ) {
  
  CreateAlerte <- function(message) {
    return(
      HTML(paste("<div class='alert'> <span class='closebtn' onclick=",'"',"this.parentElement.style.display='none';",'"',
                 ">&times;</span> <strong>",
                 message,
                 "</strong> </div>"))
    )
  }
  
  AlertBloc <- reactive({
    blocN = HTML('');
    blocM = HTML('');
    if (is.na(input$n)) {
      blocN = CreateAlerte("Attention, le nombre de tirages (n) n'est pas défini")
    }
    else { if (input$n <= 0) {
      blocN = CreateAlerte("Attention, le nombre de tirages (n) est inférieur ou égal à 0")
    }}
    if (is.na(input$m)) {
      blocM = CreateAlerte("Attention, la mise de départ (m) est non defini")
    }
    return(HTML(paste(blocM, blocN)))
  })
  
  Simulate <- eventReactive(input$go, {
    HTML(paste(
    box(background = "black", width=12,
        HTML("<h2> Résultats :</h2>" ),
        
        fluidRow(
          infoBox("Mise de départ", width = 6, input$m, icon = icon("list"), color="blue", fill=TRUE),
          infoBox("Nombre de tirages", width = 6, input$n, icon = icon("list"), color="blue", fill=TRUE),
        ),
        
        AlertBloc()
        
    )))
  })
  
  output$result <- renderText({
    Simulate()
  })


})
