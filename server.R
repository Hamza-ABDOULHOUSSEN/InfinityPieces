library ( shiny )
library(ggplot2)

shinyServer ( function (input , output ) {
  
  Simulate <- eventReactive(input$go, {
    HTML(paste(
    box(background = "black", width=12,
        HTML("<h2> Résultats :</h2>" ),
        
        fluidRow(
          infoBox("Mise de départ", width = 6, input$m, icon = icon("list"), color="blue", fill=TRUE),
          infoBox("Nombre de tirages", width = 6, input$n, icon = icon("list"), color="blue", fill=TRUE)
        )
        
    )))
  })
  
  # Simulate if n is null or negative
  Simulate_nev <- eventReactive(input$go, {
    HTML(paste(
      box(background = "black", width=12,
          HTML("<h2> Résultats :</h2>" ),
          
          fluidRow(
            infoBox("Mise de départ", width = 6, input$m, icon = icon("list"), color="blue", fill=TRUE),
            infoBox("Nombre de tirages", width = 6, input$n, icon = icon("list"), color="blue", fill=TRUE),
            HTML(paste(
              '<div class="alert alert-warning alert-dismissible fade show" role="alert">',
              '<strong>n <= 0 </strong> Attention, le nombre de tirages est inférieur ou égal à 0',
              '<button type="button" class="close" data-dismiss="alert" aria-label="Close">',
              '<span aria-hidden="true">&times;</span>',
              '</button>',
              '</div>',
            ))
          )
      ),

      ))
  })
  
  output$result <- renderText({
    if (input$n <= 0) {
      Simulate_nev()
    }
    Simulate()
    
  })


})
