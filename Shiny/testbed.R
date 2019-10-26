library(shiny)
library(networkD3)

links <- read.table(header = T, text = '
source target value
0 1 1
0 2 1
')

nodes <- read.table(header = T, text = '
name group
dad 1
son1 1
son2 1
')

son1 <- read.table(text = '
name John
age 18
')

son2 <- read.table(text = '
name Mark
age 14
')

ui <- shinyUI(fluidPage(
  fluidRow(
    column(4, forceNetworkOutput("force")),
    column(4, DT::dataTableOutput("table"))
  )
))

server <- shinyServer(function(input, output) {
  
  output$force <- renderForceNetwork({
    forceNetwork(Links = links, Nodes = nodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = 1, opacityNoHover = 1, 
                 clickAction = 'Shiny.onInputChange("id", d.name)')
  })
  
  output$table <- DT::renderDataTable(DT::datatable(get(input$id)))
  
})

shinyApp(ui = ui, server = server)