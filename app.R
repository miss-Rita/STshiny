# App

set.seed(1234)

ui <- source("C:\\Users\\b1477\\Desktop\\test\\shiny\\ui.R")

server <- source("C:\\Users\\b1477\\Desktop\\test\\shiny\\server.R")

# Run the application 
shinyApp(ui = ui, server = server)

