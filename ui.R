####################################################
#      Text Network App    #
####################################################

library("shiny")
library("igraph")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  headerPanel("Text Network App"),
  # Input in sidepanel:
  sidebarPanel(
    
    h5(p("Data Input")),
    fileInput("file", "Upload Data)"),
    #fileInput("file1", "Upload Demographics data (csv file with header))"),
    # selectInput("mode","Mode of Graph",c("directed", "undirected","max", "min", "upper",
    #                                      "lower", "plus"),"undirected"),
    # htmlOutput("yvarselect"),
    # sliderInput("cex", "Data point labels font size", min = 0.1,  max = 3, value = 1,round = FALSE),
    # sliderInput("cex2", "Vertex Size", min = 0.1,  max = 20, value = 5,round = FALSE),
    
    numericInput("npoint", "Number of max Nodes in graph", 50),
    sliderInput("cutoff", "Threshold for connection in graph", 0,1,.95),
    
    numericInput("nodes", "Number of Central Nodes in COG graph", 4),
    numericInput("connection", "Number of Max Connection with Central Node in COG graph", 5),
    
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Doc-Doc Network",plotOutput("graph1", height = 800, width = 840)),
                tabPanel("Term-Term Network",plotOutput("graph2", height = 800, width = 840)),
                tabPanel("Doc-Doc COG",plotOutput("graph3", height = 800, width = 840)),
                tabPanel("Term-Term COG",plotOutput("graph4", height = 800, width = 840))
                # tabPanel("Network Centralities",dataTableOutput("centdata"))
    )
  ) 
) 
)

