  library(shiny)
  library(shinydashboard)
  library(DT)
  library(shinyjs)
  library(sodium)
  
  
  
  
  #########################################
  # Application Navbar 
  #########################################
  header <- dashboardHeader( title = "AIMax",
                             #uiOutput("loggedinuser") ,
                             uiOutput("logoutbtn"))
  
  
  
  #########################################
  # Application Sidebar 
  #########################################
  sidebar <- dashboardSidebar(uiOutput("sidebarpanel"),collapsed = T) 
  
  
  
  #########################################
  # Application Body 
  #########################################
  body <- dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
                        tags$head(tags$link(rel="shortcut icon", href="Favicon-R.png")),
                        tags$style(HTML(".content-wrapper{height: 95vh; overflow-y:auto;}")),
                        tags$head(tags$style(HTML(".skin-blue .main-header .navbar{background-color: #1c05ad !important;} .skin-blue .main-header .logo{background-color: #1c05ad !important;}"))),
                        shinyjs::useShinyjs(), uiOutput("body"))
  
  
  
  #########################################
  # UI Function Call
  #########################################
  
  ui<-dashboardPage(header, sidebar, body, skin = "blue")