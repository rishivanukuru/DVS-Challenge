 library(shinythemes)
 library(shinyBS)


 library(shiny)
 library(networkD3)


 myLinks <-read.csv("./Data/MyLinks1.csv")
 myNodes <-read.csv("./Data/MyNodes1.csv")
 myNodes2 <- read.csv("./Data/MyNodes2.csv")

 myNodes3 <- read.csv("./Data/NodesFinal.csv")

 nodeInfo <- read.csv("./Data/NodesFinal.csv")
 
 nodesFinal <- read.csv("./Data/NodesFinal.csv")

#ui <- fluidPage(mainpanel(forceNetworkOutput("force")))

#

#ui <- dashboardPage(
#  dashboardHeader(),
#  dashboardBody(
#    fluidRow(
#      tabBox(width = 12, tabPanel("Viz",forceNetworkOutput("force")))
#    )
#  )
#)

# ui <- fillPage(theme = shinytheme("slate"),forceNetworkOutput("force",height = "100%"))

#ui <- dashboardPage(
#  
#dashboardHeader(),
#  dashboardSidebar(
#    sidebarPanel(
      
#textOutput("selected_var"),
#      uiOutput("url")
      
      
#    )
#    ),
  
#  dashboardBody(
    
    
#    fluidRow(
      
#      tabBox(width = 12, height = NULL,  
              
#              tabPanel("Test", value = 1, 
                       
                       
                       
                       
#                       forceNetworkOutput("force")
                       
                       
#                       )
#              
              
#              )
      
#    )
    
#  )
  
  
  
  
  
#)



ui <- fluidPage( 
                 
  theme = shinytheme("darkly"),
  

                 titlePanel(
                   
                   div(
                     h2("What We Viz With", align = "center"),
                     h4("Tools used by members of the Data Visualization Society", align = "center"),
                     h4("2019", align = "center")
                     
                   )
                   
                 ),
                 
                 
                 sidebarLayout(
                   
                   position = "right",
                   
                   
                   
                   
                   
                   sidebarPanel(
                     

                     
                     
                     
                     #p(em("Who does Data Viz?"), em("Where do they do it?"), em("What do they do it for?"), em("What tools do they use?")), hr(),
                     
                     #p("The answers to these questions, and many others posed as part of the 2019 Data Visualisation Community Survey, are growing more and more diverse over the years."),
                     
                     #p("This visualisation explores the diversity in 'What We Viz With'."),
                     
                     #p("In all, the respondents of the survey listed 180 distinct tools that they use while making visualisations, and all of those tools are represented here as a force-directed network."),
                     
                     #hr(),
                     
                     #h4("Details:"),
                     
                     #p("Hover over a tool to see what other tools have been used along with them. Click the nodes in the network to get more information about each tool."), 
                     
                     #p("Click and drag the nodes around to see things better. You can pan by clicking the background, or zoom in and out of the network by scrolling"), 
                     
                     #hr(),
                     
                  
                       
                       h4("Click on a tool node to see more information."),
                       
                       hr(),
                       
                     
                     conditionalPanel(
                       
                       condition = "input.id",
                       
                       h3(textOutput("selected_tool")),
                       
                       textOutput("selected_info"),
                       
                       textOutput("selected_num"),
                       
                       br(),
                       uiOutput("url"),
                       
                       hr()
                       
                     ),
                     
                     
                     p("Made by Rishi Vanukuru"),
                     p(em("Tools used:" , a(href = "https://shiny.rstudio.com", "R Shiny",target="_blank"), "with the",  a(href = "https://christophergandrud.github.io/networkD3/","networkD3 package",target="_blank"))),
                     p(em("Datasets available", a(href = "https://github.com/vanukuru/DVS-Challenge" ,"here",target="_blank") )),
                     
                     hr()
                     
                 
                     
                     
                     
                   ),
                   
                   
                   mainPanel(
                     #tabsetPanel(type = "tabs",
                     #tabPanel("Diamond", forceNetworkOutput("force"))    )
                     
                     fillPage(
                       
                       tags$style(type = "text/css", "#force {height: calc(90vh) !important;}"),
                       
                       forceNetworkOutput("force", width = "100%")
                       
                       
                     )
                     
                   )
                   
                   
                 )
                 
                 
                 
                 
)


#
# output$selected_var <- renderText({ 
# "You have selected this"
# })
#

server <- function(input, output) {
  
  observeEvent(input$id,{
    output$url <-renderUI(a(href=nodeInfo[nodeInfo[,1]==input$id, 5], paste0("More about " , input$id),target="_blank"))
  })
  
  output$force <- renderForceNetwork(
    
    {
      
      MyClickScript <- 
        '      d3.select(this).select("circle").transition()
.duration(200)
.attr("r", 20);
      
      Shiny.onInputChange("id", d.name);
      
      '
      
     # myNodes3$name <- with(myNodes3, paste(", details: ", info ))
      
      
     fn <- forceNetwork(Links = myLinks, 
                   Nodes = nodesFinal, 
                   Source = "source",
                   Target = "target", 
                   Value = "value", 
                   NodeID = "name",
                   Nodesize = 'size',
                   Group = "group",
                   radiusCalculation = "((d.nodesize)*13/1000)+5",
                   linkDistance = JS("function(d){return (d.value * 13/800)+4;}"), 
                   #linkWidth = JS("function(d){return ((d.value * 13/400));}"),
                   linkColour = "#333333",
                   fontSize = 10,
                   fontFamily = "sans-serif",
                   colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10)"),
                   #colourScale = viridis,
                   opacity = 0.7, 
                   zoom = T, 
                   bounded = F,
                   charge = -180, 
                   opacityNoHover = 0.9,
                   clickAction = MyClickScript)
     
     fn$x$nodes$hyperlink <- nodeInfo$info
     fn
     
    }
  )
  
  
  output$linkTest <- renderText(input$id)

  output$selected_tool <- renderText({ 
    input$id
  })
    
  output$selected_var <- renderText({ 
    paste("Tool: ", input$id)
  })
  
  output$selected_num <- renderText({ 
    paste(nodeInfo[nodeInfo[,1]==input$id, 3], " respondent(s) use ", input$id )
  })
  
  
  output$selected_info <- renderText({ 
    paste("Category:",nodeInfo[nodeInfo[,1]==input$id, 4])
  })
  
}

shinyApp(ui = ui, server = server, options = list(height = 1080))



