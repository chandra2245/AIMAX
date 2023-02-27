library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(RSQLite)

setwd("D://Shiny_radius")

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 img(src='background2.PNG',style="
                                      width: 70%;
                       display: block;
                       margin-left: auto;
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



header <- dashboardHeader( title = "Radius 2.0",uiOutput("loggedinuser") ,uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  

  credentials  =  dbGetQuery(conn, "SELECT * FROM USER")
  

  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username==Username))==1) { 
            pasmatch  <- credentials["password"][which(credentials$username==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$loggedinuser<-renderUI({ 
    req(USER$login)
    tags$div(input$userName,
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
    })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$div(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"user_type"][which(credentials$username==input$userName)]=="advanced") {
        sidebarMenu(
          menuItem("File Upload", tabName = "FileUpload", icon = icon("th")),
          menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
          
        )
      }
      else{
        sidebarMenu(
          menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
        )
        
      }
    }
  })
  
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"user_type"][which(credentials$username==input$userName)]=="advanced") {
        tabItems(
          tabItem(
            tabName ="dashboard",
            fluidRow(
              box(width = 12, dataTableOutput('results'))
            ))
          ,
          tabItem(
            tabName ="About", class = "active",
            h2("This is second tab")
          )
        )
      } 
      else {
        tabItem(
          tabName ="dashboard", class = "active",
          fluidRow(
            box(width = 12, dataTableOutput('results'))
          ))
        
      }
      
    }
    else {
      loginpage
    }
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  }

runApp(list(ui = ui, server = server), launch.browser = TRUE)

