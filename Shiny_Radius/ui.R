#########################################
#Importing the required libraries required
#########################################

#Shiny libraries
library(shiny) 
library(shinydashboard)
library(shinyjs)

#It creates an HTML widget to display R data objects with DataTables
library(DT)

#User password encryption and decryption library
library(sodium)

#SQLite Database interface library
library(RSQLite)

#Date function libraries
library(lubridate)

library(plotly)


#########################################
# Setting the Working Directory
#########################################
# setwd("D:\\Shiny_Radius\\")


#########################################
# Setting the Database Connection
#########################################
conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")












#########################################
# Main login screen 
#########################################
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 tags$img(src="background2.png",style="display: block; margin-left: auto;
                          margin-right: auto;"),
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center")))
                     ))
                     )


#########################################
# Application Navbar 
#########################################
header <- dashboardHeader( title = "Radius 2.0",
                           #uiOutput("loggedinuser") ,
                           uiOutput("logoutbtn"))








#########################################
# Application Sidebar 
#########################################
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"),collapsed = T) 








#########################################
# Application Body 
#########################################
body <- dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),shinyjs::useShinyjs(), uiOutput("body"))








#########################################
# Main Page Static Body
#########################################
MainPageRow1 <- fluidRow( 
  box(
    h3("Sales"),
    plotlyOutput("salesplot"),
    width = 4,height = 400,status = "info"
  )
  ,box(
    h3("Promotion"),actionButton("button", "GET DETAILED MARKETING  ANALYSIS"),
    width = 8,height = 400,background = 'blue'
  )
)

MainPageRow2 <- fluidRow( 
  box(
    h3("ROI Analysis"),
    width = 3,height = 400,background = 'purple'
  )
  ,box(
    h3("Marketing Mix"),
    width = 9,height = 400,background = 'orange'
  )
  
  
)





#########################################
# Marketing Analysis Body
#########################################
MarketPageRow1 <- fluidRow( 
  box(
    h3("Geography Wise Marketing Activity"),
    width = 4,height = 300,background = 'teal',textOutput("SliderText")
  )
  ,box(
    h3("Channel wise Marketing Activity"),
    width = 4,height = 300,background = 'red'
  )  ,box(
    h3("Franchise Marketing Activity"),
    width = 4,height = 300,background = 'lime'
  )
)

MarketPageRow2 <- fluidRow(style="padding-top:5px;",align="center",  
  box(
    column(6,h3("TRADITIONAL MEDIA MARKETING")),
    column(6,h3("DIGITAL MEDIA")),
    width = 12,height = 75,background = 'yellow'
  )
  
  
  
)


MarketPageRow3 <- fluidRow( 
  box(
    column(3,
           h3("Region wise Call Activity")),
    width = 3,height = 350,background = 'red' 
  ),
  box(width = 9,height = 350,background = 'green',
      column(9,
             h3("Graph")
             # plotOutput(countplot)
  )
  )
  
)








#########################################
# UI Function Call
#########################################

ui <- dashboardPage(header, sidebar, body, skin = "green")






#########################################
# File Used in the Sales Summary
#########################################




#########################################
# Server for Conditional Computation
#########################################




#########################################
# Run App Function Call
#########################################
# runApp(list(ui = ui, server = server), launch.browser = TRUE)


#########################################
# Disconnecting the DB after Use
#########################################
# dbDisconnect(conn)

