#########################################
# IMPORT LIBRARIES
#########################################

library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(shinyjs)
library(sodium)
library(RSQLite)
#install.packages("ggplot2")
library(ggplot2)
library(plotly)
library(lubridate)
library(readxl)
library(stringr)
library(maps)
library(dplyr)
library(leaflet)
library(htmltools)
library(sqldf)
library(ECharts2Shiny)
library(forecast)
library(flexdashboard)
library(tidyr)
library(VIM)
library(missForest)
library(corrplot)
# library(MVN)
library(mice)
library(DMwR2) 
library(car) 
library(glmnet) 
library(psych)
library(caret) 
library(pls)
library(tidyverse) 
library(TTR)
library(BBmisc)
library(rgdal)
library(googleVis)
library(shinycssloaders)





#########################################
# WORKING DIRECTORY
#########################################

# setwd("D:\\Shiny_Radius\\")
options(warn=-1) 




options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)





#########################################
# Load User data
#########################################

loadUserData <- function() {
  # Connect to the database
  conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
  data  =  dbGetQuery(conn, "SELECT * FROM USER")

  dbDisconnect(conn)
  data
}



#########################################
# FRANCHISE DATA
#########################################


franchise_names<-function(){
  conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
  comp_data = dbGetQuery(conn, "SELECT * FROM Sales where upper(Competitor_name) = 'HUMIRA'" ) ## change for selecting brand
  comp_data$DATE = as.Date(as.numeric(comp_data$DATE), origin = "1899-12-30")
  comp_data$DATE = format(comp_data$DATE,"%Y%m")
  comp_data_final = sqldf('select upper(BRAND) as BRAND,FRANCHISE_NAME,DATE,sum(SALES) as SALES
                            from comp_data group by BRAND,FRANCHISE_NAME,DATE')
  franchise <- unique(comp_data_final$FRANCHISE_NAME) 
  dbDisconnect(conn)
  return(franchise)
  
}




#########################################
# LOGIN PAGE BODY
#########################################


loginpage <- div(id = "loginpage", style = "width: 100%; margin: 0 auto; padding: 1px;height: 1000px;",
                 tags$h2("AIMax", class = "text-center", style = "padding-top: 0;color:#fff; font-weight:600;font-size: 350%;margin: 3%;"),
                 tags$h2("Next Gen Marketing Analytics Solution", class = "text-center", style = "padding-top: 0;color:#fff; font-weight:600;font-size: 300%;"),
                 
                 div(id = "loginpop",
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
                     )))
                     )









#########################################
# File Upload Page Body
#########################################
FileUploadRow <- fluidRow( 
  box(
    selectInput(inputId = 'filetype',label = 'Select File Type',choices = c("CALL ACTIVITY","EMAIL","PROMO DISPLAY","PROMO SEARCH","SPEND METRICS","SALES"),selected = "SALES"),
    tags$hr(),
    downloadButton("downloadData", "Download Template",style = "color: white; background-color:#3c8dbc;"),
    tags$hr(),
    fileInput('file1', h4('Choose xlsx file to upload'),
              accept = c(".xlsx")
    ),
    tags$hr(),
    
    sliderInput(inputId='sheetno', label="Sheet Number", min=1, max=10, value=1, step =1),
    textInput(inputId = 'nullvalues',label='NA Values',value = 'NA'),
    tags$hr(),
    actionButton("filesave", "Save File", style = "color: white; background-color:#3c8dbc;
                 padding: 10px 15px; width: 150px; cursor: pointer;
                 font-size: 18px; font-weight: 600;"),
    tags$hr(),
    width = 3,height = 800,background = "black"
    )
  ,box(
    h4('Table Output'),
    shinyjs::hidden(div(id = "fileUpOut",
    DT::dataTableOutput('contents'))),
    width = 9,height = 800,status = "info"
  )
  )








#########################################
# Main Page Body
#########################################

MainPageRow1 <- fluidRow( 
                           box(width = 4,height = 120,background = 'aqua',
                               selectInput(inputId = "Brand_P1", label = h3("Select Brand"),
                                           choices = list("Brand A" = "MAVYRET", "Brand B" = "HUMIRA",
                                                          "Brand C" = "LUPRON DEPOT","Brand D"="CREON"), selected = "HUMIRA")),
                           box(width = 4 ,height = 120,background = 'teal',sliderInput("DateFilter_P1",
                                                                                      label= h3("Date Filter"),
                                                                                      width='100%',
                                                                                      step=31,
                                                                                      ticks = FALSE,
                                                                                      min = as.Date("2016-01-01","%Y-%m-%d"),
                                                                                      max = as.Date("2016-12-01","%Y-%m-%d"),
                                                                                      value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                                                                                      timeFormat="%b %Y"))
                           ,
                           box(width = 4,height = 120,background = 'aqua',
                               radioButtons(
                                 "analysis_type",
                                 h3("Analysis Type"),
                                 choices = c("Month","Quarter","Year"),
                                 selected = "Month",
                                 inline = "True",
                                 width = NULL,
                                 choiceNames = NULL,
                                 choiceValues = NULL
                               )
                           )
)

MainPageRow2 <- fluidRow( 
                  box(h3("Sales Analysis"),
                  uiOutput("saleoutputs"),
                  width = 3,height = 600,status = "info"
                  )
                          ,box(
                            h3("Marketing Analysis"),
                            uiOutput("plotoutputs"),
                            width = 9,height = 600,status = "info"
                          )
                          
)

MainPageRow3 <- fluidRow( 
                          box(
                              column(3,
                                 div(style="text-align: center;",actionButton(inputId = "MarketSimulator_pagebtn",label = "Channel Impact Simulator",width = "98%",style = "color: white; background-color:#1c05ad;
                                  padding: 10px 15px; width: 200px; cursor: pointer;
                                                                              font-size: 16px; font-weight: 500;"))),
                              column(3,
                                     div(style="text-align: center;",actionButton(inputId = "Newanalysis_pagebtn",label = "Brand Performance Analysis",width = "98%",style = "color: white; background-color:#1c05ad;
                                  padding: 10px 15px; width: 200px; cursor: pointer;
                                                                              font-size: 16px; font-weight: 500;"))),
                              column(3,
                                     div(style="text-align: center;",actionButton(inputId = "Newanalysis2_pagebtn",label = "Marketing Mix",width = "98%",style = "color: white; background-color:#1c05ad;
                                  padding: 10px 15px; width: 200px; cursor: pointer;
                                                                              font-size: 16px; font-weight: 500;"))),
                          column(3,
                                 div(style="text-align: center;",actionButton(inputId = "ROIanalysis_pagebtn",label = "ROI Analysis",width = "98%",style = "color: white; background-color:#1c05ad;
                                  padding: 10px 15px; width: 200px; cursor: pointer;
                                                                              font-size: 16px; font-weight: 500;"))),
                          
                          width = 12,status = "info")
                          
)






#########################################
# Marketing Analysis Body
#########################################



MarketPageRow1<- fluidRow( tags$style(type="text/css",
                                      ".recalculating {opacity: 1.0;}" ), box(
  h3("Email Click through Rate"),
  withSpinner(htmlOutput("gauge"),type = 1),
  width = 2,height = 375,status = "info"
),


                           box(
                                    h3("Target Customer Types for Email Marketing"),
                                    withSpinner(plotlyOutput("barplot"),type = 1),
                             width = 5,height = 375,status = "info"
                           ),
                           box(
                                    h3("Commonly used type of Display ADs"),
                                    withSpinner(plotlyOutput("barplot_media"),type = 1),
                             width = 5,height = 375,status = "info"
                           )
)


MarketPageRow2<- fluidRow(                              
box(
                                           h3("Region wise Display Activity"),
                                           withSpinner(leafletOutput("promodisplaymap",height = 330),type = 1),
                                    width = 6,height = 400,status = "info"),
                                  box(
                                           h3("Region wise Call Activity"),
                                           withSpinner(leafletOutput("callactivitymap",height = 330),type = 1),
                                    width = 6,height = 400,status = "info"))





#########################################
# Marketing Analysis Body
#########################################





New_PageRow2<- fluidRow( tags$style(type="text/css",
                                      ".recalculating {opacity: 1.0;}" ), 
                           box(
                                        h3("Rank wise Analysis"),
                                        # plot_ly(y = ~rnorm(50), type = "box", name="Low")%>% add_trace(y = ~rnorm(50, 1.15), name="Medium")%>% add_trace(y = ~rnorm(50, 1.5), name="High")%>% add_trace(y = ~c(rnorm(50, 1.5),rnorm(50),rnorm(50, 1.25), rnorm(50, 1.15)), name="Total")%>% layout(yaxis = list(title = "")),
                                        withSpinner(plotlyOutput("rank_plot"),type = 1),
                                        width = 6,height = 500,status = "info"
                                      ),
                           
                           
                           box(
                             h3("Investment Marketing Mix"),
                             # plot_ly(
                             #   x = c("Display", "Email", "Search", "Call"), y = c("Display", "Email", "Search", "Call"),
                             #   z = matrix(rnorm(16), nrow = 4, ncol = 4), type = "heatmap"
                             # ),
                             withSpinner(plotlyOutput("investment_mix_plot"),type = 1),
                             width = 6,height = 500,status = "info"
                           )
)









#########################################
# ROI Analysis Body
#########################################



ROIPageRow1<- fluidRow(  
                       column(2),box(    h3("Overall Activities ROI"),
                               plotlyOutput("overallROI"),
                               width = 8,height = 800,status = "info", style = "padding: 100px;"),column(2)#,

                            
                           
                           # box(
                           #   h3("Email Activity ROI"),
                           #   plotlyOutput("emailROI"),
                           #   width = 4,height = 400,status = "info"
                           # ),
                           # box(
                           #   h3("Display Activity ROI"),
                           #   plotlyOutput("displayROI"),
                           #   width = 4,height = 400,status = "info"
                           # )
)


ROIPageRow2<- fluidRow(
                           column(2),                            
  box(
    h3("Search Activity ROI"),
    plotlyOutput("searchROI"),
    width = 4,height = 400,status = "info"),
  box(  h3("Call Activity ROI"),
 plotlyOutput("callROI"),
   width = 4,height = 400,status = "info"),

  column(2))







#########################################
# Marketing SIMULATOR Body
#########################################
SimulatorHeader<- fluidRow(column(3),infoBoxOutput("brandBox",width = 3), 
                                 infoBox(icon=icon("cogs",lib = "font-awesome"),color = "aqua",actionButton("executemodel", "Run Simulator", style = "color: white; background-color:#3c8dbc;
                                              padding: 10px 15px; width: 150px; cursor: pointer;
                                              font-size: 18px; font-weight: 600;"),fill=T,width = 3))
SimulatorRow1 <- fluidRow(      
  tabsetPanel(id="plotpanel",
              tabPanel("Market Simulator",value = '1',
                       fluidRow(
                         box(   
                           radioButtons("value_input", label = h4("Input type"),
                                         choices = list("Value in $", "Percentage"), 
                                         selected = "Value in $",inline = "TRUE"),               
                                       
                         sliderInput("email", "Email Activity:",
                                                   min = -100, max = 100,
                                                   value = 0),
                                       textInput("email_value",label = NULL,placeholder = "Value for Email"),
                                       sliderInput("call", "Call Activity:",
                                                   min = -100, max = 100,
                                                   value = 0),
                                       textInput("call_value",label = NULL,placeholder = "Value for Call"),
                                       
                                       sliderInput("search", "Search Activity:",
                                                   min = -100, max = 100,
                                                   value = 0),
                                       textInput("Search_value",label = NULL,placeholder = "Value for Search"),
                                       
                                       sliderInput("display", "Display Activity:",
                                                   min = -100, max = 100,
                                                   value = 0),
                                       textInput("display_value",label = NULL,placeholder = "Value for Display"),
                                       tags$p("Initial prediction based default 0 value of the parameters. Change the slider to update the prediction"),width = 3,height = 800,background = "black"),
                                     
                                     # Output: Table summarizing the values entered ----
                                     box(textOutput("text1"),
                                       plotlyOutput("simPlot"),
                                       plotlyOutput("Accuracy"),width = 9,height = 800,status = "info")
                       )),
              tabPanel("Competitor Analysis",value = '2',
                       # selectInput(inputId = "compBrand", label = "Select Brand", choices = competitor_brand),
                       fluidRow(box(
                                       selectInput(inputId = "market", label = "Select Market", choices = franchise_names()),
                                       sliderInput("pred", "Prediction time (months):",
                                                   min = 1, max = 6,pre = "+",
                                                   value = 1),tags$p("Change the slider for the prediction of next +n month"),width = 3,height = 800,background = "black"),
                                     box(textOutput("text2"),
                                       plotOutput(outputId = "mktStr"),
                                       loadEChartsLibrary(),
                                       absolutePanel( id = "test", top = 3, left = 30, width = 800, height = 1000, draggable = TRUE),
                                       deliverChart(div_id = "test"),
                                       width = 9,height = 800,status = "info")
                                )
              )
  )
  
)









#########################################
# Max File Size
#########################################

options(shiny.maxRequestSize = 20*1024^2)











server <- function(input, output, session) {
 
 
  #########################################
  # LOGIN PAGE AUTHENTICATION
  #########################################  
  
  login = FALSE
  USER <- reactiveValues(login = login)
  

  

  observeEvent(input$button, {
    updateTabItems(session, "tabs", "Marketing_Analysis")
    
  })
  

  observeEvent(input$MarketingPage_switch, {
    updateTabItems(session, "tabs", "Marketing_Analysis")
    
  })
  
  observeEvent(input$MarketSimulator_pagebtn,{
    updateTabItems(session, "tabs", "Simulator")
  })
  
  observeEvent(input$Newanalysis_pagebtn,{
    updateTabItems(session, "tabs", "New_Analysis")
  })
  
  observeEvent(input$Newanalysis2_pagebtn,{
    updateTabItems(session, "tabs", "New_Analysis_2")
  })
  
  observeEvent(input$ROIanalysis_pagebtn,{
    if(input$executemodel>0)
      updateTabItems(session, "tabs", "ROI_Analysis")
    else
      alert("Please click Run Simulator in Market Simulator page")
  })
  
  observe({ 
    if (USER$login == FALSE) {
      
      if (!is.null(input$login)) {
        
        if (input$login > 0) {
          
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          
          credentials  =loadUserData()  
          
          if(length(which(credentials$username==Username))==1) { 
            
            pasmatch  <- credentials["password"][which(credentials$username==Username),]
            pasverify <- password_verify(pasmatch, Password)
            
            if(pasverify) {
              
              USER$login <- TRUE
            } 
            else {
              
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } 
          else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
    
  })
  
  
  
  
  
  
  
  
  
  #########################################
  # Displaying Logged in User to Navbar
  #########################################

  #output$loggedinuser<-renderUI({ 
  #  
  #  req(USER$login)
  #  tags$h2(input$userName,
  #         class = "dropdown", 
  #          style = "background-color: #eee !important; border: 0;
  #          font-weight: bold; margin:5px; padding: 10px;")
  #  })
  
  
  
  
  
  
  
  #########################################
  # Logout Button
  #########################################
  

  output$logoutbtn <- renderUI({
    
    req(USER$login)
    tags$div(a(icon("sign-out-alt",lib = "font-awesome"), "Logout", 
               href="javascript:window.location.reload(true)"),
             class = "dropdown", 
             style = "background-color: #eee !important; border: 0;
             font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  
  
  
  
  
  
  
  
  #########################################
  # Sidebar Panel dynamic according to the logged in user
  #########################################
  

  output$sidebarpanel <- renderUI({
    
    if (USER$login == TRUE ){ 
      
      credentials  =  loadUserData() 
      
      #If the logged in user is advanced then File upload option is also displayed
      if (credentials[,"user_type"][which(credentials$username==input$userName)]=="advanced") {
        sidebarMenu(id="tabs",
                    tags$head(tags$style(HTML(".inactiveLink {
                                         pointer-events: none;
                                         cursor: default;
                                          }
                                         
                                        .skin-blue .main-header .navbar .navbar-static-top {
                                         background-color: #1c05ad !important;
                                        }

                                        .skin-blue .main-header .logo{
                                         background-color: #1c05ad !important;
                                          }"))),
                    menuItem("File Upload", tabName = "FileUpload", icon = icon("file-alt",lib = "font-awesome")),
                    menuItem("Home Page", tabName = "MainPage", icon = icon("dashboard")),
                    menuItem("Marketing Analysis", tabName = "Marketing_Analysis", icon = icon("search-location",lib = "font-awesome")),
                    menuItem("ROI Analysis", tabName = "ROI_Analysis", icon = icon("funnel-dollar",lib = "font-awesome")),
                    menuItem("Channel Impact Simulator", tabName = "Simulator", icon = icon("microchip",lib = "font-awesome")),
                    menuItem("Brand Performance Analysis", tabName = "New_Analysis", icon = icon("search-location",lib = "font-awesome")),
                    menuItem("Marketing Mix", tabName = "New_Analysis_2", icon = icon("search-location",lib = "font-awesome"))
                    
                    
        )
      }
      else{
        sidebarMenu(id="tabs",
                    tags$head(tags$style(".inactiveLink {
                                         pointer-events: none;
                                         cursor: default;
                                        }
                                         
                                         .skin-blue .main-header .navbar .navbar-static-top {
                                         background-color: #1c05ad !important;
                                         }
                                         
                                         .skin-blue .main-header .logo{
                                         background-color: #1c05ad !important;
                                         }")),
                    menuItem("Home Page", tabName = "MainPage", icon = icon("dashboard")),
                    menuItem("Marketing Analysis", tabName = "Marketing_Analysis", icon = icon("search-location",lib = "font-awesome")),
                    menuItem("ROI Analysis", tabName = "ROI_Analysis", icon = icon("funnel-dollar",lib = "font-awesome")),
                    menuItem("Channel Impact Simulator", tabName = "Simulator", icon = icon("microchip",lib = "font-awesome")),
                    menuItem("Brand Performance Analysis", tabName = "New_Analysis", icon = icon("search-location",lib = "font-awesome")),
                    menuItem("Marketing Mix", tabName = "New_Analysis_2", icon = icon("search-location",lib = "font-awesome"))
                    
                    
        )
        
      }
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################################
  # BodY Panel dynamic according to the logged in user
  #########################################
  

  output$body <- renderUI({
    if (USER$login == TRUE ) {

      credentials  =  loadUserData() 
      #If the logged in user is advanced then File upload option is also displayed
      if (credentials[,"user_type"][which(credentials$username==input$userName)]=="advanced") {
        
        tabItems(
          tabItem(
            tabName ="FileUpload",class = "active",
            h2("File Upload Page"),FileUploadRow
            
          ),
          tabItem(
            tabName ="MainPage",h2("Home Page"),
            MainPageRow1,MainPageRow2,MainPageRow3),
          
          
          
          tabItem(tabName = "Marketing_Analysis",h2("Marketing Analysis Page"),
                  MarketPageRow1,MarketPageRow2
          ),
          # ROI Page
          tabItem(tabName = "ROI_Analysis",h2("ROI Analysis Page"),
                  ROIPageRow1#,ROIPageRow2
          ),
          # New Analysis
          tabItem(tabName = "New_Analysis",h2("Brand Performance Analysis Page"),
                  uiOutput("New_PageRow1"),New_PageRow2
          ),
          tabItem(tabName = "New_Analysis_2",h2("Marketing Mix Recommendation"),
                  uiOutput("New_Page2Row_1"),uiOutput("New_Page2Row_2"),uiOutput("New_Page2Row_3"),uiOutput("New_Page2Row_4")
          ),
          tabItem(
            tabName ="Simulator", h2("Channel Impact Simulator Page"),
            SimulatorHeader,SimulatorRow1
            
          )
        )
      } 
      else {
        
        tabItems(
          
          tabItem(
            
            tabName ="MainPage", class = "active",h2("Home Page"),
            MainPageRow1,MainPageRow2,MainPageRow3),
          
          
          
          tabItem(tabName = "Marketing_Analysis",h2("Marketing Analysis Page"),
                  fillPage( MarketPageRow1,MarketPageRow2)
          ),
          # ROI Page
          tabItem(tabName = "ROI_Analysis",h2("ROI Analysis Page"),
                  ROIPageRow1#,ROIPageRow2
          ),
          # New Analysis
          tabItem(tabName = "New_Analysis",h2("Brand Performance Analysis Page"),
                  uiOutput("New_PageRow1"),New_PageRow2
          ),
          tabItem(tabName = "New_Analysis_2",h2("Marketing Mix Recommendation"),
                  uiOutput("New_Page2Row_1"),uiOutput("New_Page2Row_2"),uiOutput("New_Page2Row_3"),uiOutput("New_Page2Row_4")
          ),
          tabItem(
             tabName ="Simulator", h2("Channel Impact Simulator Page"),
             SimulatorHeader,SimulatorRow1
          
        ))
        
      }
 
    }
    else {
      loginpage
    }
  })
  

  
  
  

  output$brandBox_1 <- renderInfoBox({
    infoBox(
      "Selected Brand", ifelse(input$Brand_P1=="HUMIRA","BRAND B",ifelse(input$Brand_P1=="MAVYRET","BRAND A",ifelse(input$Brand_P1=="LUPRON DEPOT","BRAND C",ifelse(input$Brand_P1=="CREON","BRAND D")))), icon = icon("prescription-bottle-alt",lib = "font-awesome"),
      color = "purple",fill = TRUE
    )
  })
  
  output$brandBox_2 <- renderInfoBox({
    infoBox(
      "Selected Brand", ifelse(input$Brand_P1=="HUMIRA","BRAND B",ifelse(input$Brand_P1=="MAVYRET","BRAND A",ifelse(input$Brand_P1=="LUPRON DEPOT","BRAND C",ifelse(input$Brand_P1=="CREON","BRAND D")))), icon = icon("prescription-bottle-alt",lib = "font-awesome"),
      color = "purple",fill = TRUE
    )
  })
  
  output$New_Page2Row_1 <- renderUI({
    fluidRow(
      infoBoxOutput("brandBox_1",width = 4),
      box(width = 4,height = 90,background = 'aqua',
          textInput("target_sales_tb", placeholder="Enter Amount you want to Invest (in millions)", label = tagList("Investment in $"))),
          infoBox(icon=icon("cogs",lib = "font-awesome"),color = "aqua",actionButton("get_recommendation", "Get Recommendation", style = "color: white; background-color:#3c8dbc;
                                                                                     padding: 10px 15px; width: 190px; cursor: pointer;
                                                                                     font-size: 18px; font-weight: 600;"),fill=T,width = 4)
      
    )
  })
  
  output$New_Page2Row_2 <- renderUI({
    fluidRow(
      column(4),
      box(width = 4,height = 260,background = 'aqua',
          # radioButtons("channel_input", label = h3("Choose channels you want to invest in"),
          #              choices = list("Search", "Display", "Email","Call Activity"), 
          #              selected = 1)
          h3("Customise Channel Investment"),
          column(3,
          checkboxGroupInput("channel_input", label = NULL,choices = list("Search", "Display", "Email","Call Activity"), width = "100%")),
          column(9,
          textInput("call_invest", placeholder= "Fix Investment in Search $", label = NULL, width = "100%"),
          textInput("email_invest", placeholder= "Fix Investment in Display $", label = NULL, width = "100%"),
          textInput("Display_invest", placeholder= "Fix Investment in Email $", label = NULL, width = "100%"),
          textInput("Search_invest", placeholder= "Fix Investment in Call Activity $", label = NULL, width = "100%"))
          
          ),
      # ,
      # box(width = 4,height = 235,background = 'aqua',
      #     h4("Customize Channel-wise Investment"),
      #         textInput("call_invest", placeholder= "Fix Investment in $", label = NULL),
      #         textInput("email_invest", placeholder= "Fix Investment in $", label = NULL),
      #         textInput("Display_invest", placeholder= "Fix Investment in $", label = NULL),
      #         textInput("Search_invest", placeholder= "Fix Investment in $", label = NULL)
      # ),
      column(4)
      
      )
  })
  
  
  
  output$New_Page2Row_3 <- renderUI({
    fluidRow(
      column(12,
             box(width = 12,height = 325,
             h3(paste0("Current Investment of ",ifelse(input$Brand_P1=="HUMIRA","BRAND B",ifelse(input$Brand_P1=="MAVYRET","BRAND A",ifelse(input$Brand_P1=="LUPRON DEPOT","BRAND C",ifelse(input$Brand_P1=="CREON","BRAND D")))), " in $"),
                DT::dataTableOutput('investment_contents')
             )
             )
      )
    )
  })
  
  output$New_Page2Row_4 <- renderUI({
    fluidRow(
      column(12,
             box(width = 12,height = 325,
                 h3(paste0("Optimal Recommendation for ",ifelse(input$Brand_P1=="HUMIRA","BRAND B",ifelse(input$Brand_P1=="MAVYRET","BRAND A",ifelse(input$Brand_P1=="LUPRON DEPOT","BRAND C",ifelse(input$Brand_P1=="CREON","BRAND D")))) , " in $"),
                    DT::dataTableOutput('optimal_investment_contents')
                 )
             )
      )
    )
  })
  
  
    
  output$investment_contents <- DT::renderDT(data.frame(Channels=c("Search","Display","Call","Email","Total"),Investment=c("19158190473.1","49179280636","792042211","31744561","51888436510451"))
  ,options=list(paging = F,searching=F,lengthChange=F,dom="t"))
  
  output$optimal_investment_contents <- DT::renderDT(data.frame(Option=c("1","2","3"),Search=c("118896940.7","236544238.8","112626107.3")
                                                                ,Display=c("117691904.1","231863552.7","110386934.1")
                                                                ,Email=c("116681968.7","231592208.5","110246985.7")
                                                                ,Call=c("115752369.1","0","119914185.5")
                                                                ,Recommendation=c("89%","86%","83%")
                                                                           )
                                             ,options=list(paging = F,searching=F,lengthChange=F,dom="t"), selection = list(target = "row", selected = c(1)),rownames= FALSE)
  
  
  
  #########################################
  # FILE CONTENT DISPLAY
  #########################################
  observeEvent(input$file1, {
    shinyjs::toggle(id = "fileUpOut")
  })  
  
  output$contents <- DT::renderDT({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    readxl::read_excel(inFile$datapath,
                       sheet = input$sheetno,na = input$nullvalues)
  })
  
  
  
  
  
  
  
  
  
  
  #########################################
  # Disable ROI and Simulator
  #########################################   
  
  
  observeEvent(input$Brand_P1, {
    showElement(id="text1")
    showElement(id="text2")
    hideElement(id ="simPlot" )
    hideElement(id ="Accuracy" )
    hideElement(id ="mktStr" )
    hideElement(id ="test" )
    hideElement(id = "New_Page2Row_3")
    hideElement(id = "New_Page2Row_4")
    updateSliderInput(session,'email',value = 0)
    updateSliderInput(session,'call',value = 0)
    updateSliderInput(session,'search',value = 0)
    updateSliderInput(session,'display',value = 0)
    updateSliderInput(session,'pred',value = 1)
    
    addCssClass(selector = "a[data-value='ROI_Analysis']", class = "inactiveLink")
    
  })

  
  
  
  
  
  
  
  

  
  #########################################
  # Model Execution
  #########################################  
  observeEvent(input$executemodel, {
  
    InputBrand<-as.character(input$Brand_P1)
    
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Model Execution", value = 0)
    hideElement(id="text1")
    hideElement(id="text2")
    

    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    dbListTables(conn)
    
    # Call Activity Table Load 
    Call_Activity = dbGetQuery(conn, "SELECT * FROM CALL_ACTIVITY")
    Call_Activity[,"DATE"] = as.Date(as.numeric(Call_Activity$DATE), origin = "1899-12-30")
    # Column Name change
    names(Call_Activity)[1] = "TERRITORY"
    names(Call_Activity)[2] = "BRAND"
    names(Call_Activity)[3] = "CALL_DUR"
    names(Call_Activity)[4] = "Call_ACT_DT"
    # Changing BRAND name to upper case
    Call_Activity$BRAND = toupper(Call_Activity$BRAND)
    # Selecting HUMIRA, CREON, MAVYRET, LUPROn DEPOT BRAND Only
    Call_Activity_Final = subset(Call_Activity, (Call_Activity$BRAND %in% c("HUMIRA","CREON","LUPRON DEPOT","MAVYRET")))
    # Making DateID column
    Call_Activity_Final$DATE_ID = format(Call_Activity_Final$Call_ACT_DT,"%Y%m")
    # Summation of call duration group by DATE_ID
    Call_Activity_Final = sqldf('select BRAND,DATE_ID,sum(CALL_DUR) as CALL_DUR 
                                from Call_Activity_Final group by BRAND,DATE_ID')
    
    
    progress$inc(1/10, detail ="Reading Datasets from DB")
    
    
    # Email Data
    Field_Email = dbGetQuery(conn, "SELECT * FROM EMAIL")
    Field_Email[,"DATE"] = as.Date(as.numeric(Field_Email$DATE), origin = "1899-12-30")
    # Column Name chabge
    names(Field_Email)[1] = "DATE"
    names(Field_Email)[2] = "BRAND"
    names(Field_Email)[3] = "EMAIL_SENT_COUNT"
    names(Field_Email)[4] = "EMAIL_UNIQUE_OPEN_COUNT"
    names(Field_Email)[5] = "TOTAL_EMAIL_CLICK_COUNT"
    names(Field_Email)[6] = "TOTAL_EMAIL_OPENED_COUNT"
    # Changing BRAND name to upper case
    Field_Email$BRAND = toupper(Field_Email$BRAND)
    # Selecting HUMIRA, CREON, MAVYRET, LUPROn DEPOT BRAND Only
    Field_Email_Final = subset(Field_Email, (Field_Email$BRAND %in% c("HUMIRA","CREON","LUPRON DEPOT","MAVYRET")))
    # Making DateID column
    Field_Email_Final$DATE_ID = format(Field_Email_Final$DATE,"%Y%m")
    # Summation of Field_Email group by DATE_ID
    Field_Email_Final = sqldf('select BRAND,DATE_ID,sum(EMAIL_SENT_COUNT) as EMAIL_SENT,
                              sum(TOTAL_EMAIL_CLICK_COUNT) as EMAIL_CLICK,
                              sum(TOTAL_EMAIL_OPENED_COUNT) as EMAIL_OPENED 
                              from Field_Email_Final group by BRAND,DATE_ID')
    
    
    
    
    # Promo Display
    Promo_Display = dbGetQuery(conn, "SELECT * FROM PROMO_DISPLAY")
    Promo_Display[,"DATE"] = as.Date(as.numeric(Promo_Display$DATE), origin = "1899-12-30")
    names(Promo_Display)[1] = "BRAND"
    names(Promo_Display)[2] = "PD_SPEND"
    names(Promo_Display)[3] = "PD_PLANNED_SPEND"
    names(Promo_Display)[4] = "PD_ACT_CNT"
    names(Promo_Display)[5] = "PD_ESTIMATE_SPEND"
    names(Promo_Display)[6] = "PD_MEDIA_TYPE"
    names(Promo_Display)[7] = "PD_DATE"
    names(Promo_Display)[8] = "PD_TERRITORY"
    # Changing BRAND name to upper case
    Promo_Display$BRAND = toupper(Promo_Display$BRAND)
    # Selecting HUMIRA, CREON, MAVYRET, LUPROn DEPOT BRAND Only
    Promo_Display_Final = subset(Promo_Display, (Promo_Display$BRAND %in% c("HUMIRA","CREON","LUPRON DEPOT","MAVYRET")))
    # Making DateID column
    Promo_Display_Final$DATE_ID = format(Promo_Display_Final$PD_DATE,"%Y%m")
    # Summation of Promo_Display group by DATE_ID
    Promo_Display_Final = sqldf('select BRAND,DATE_ID,sum(PD_SPEND) as PD_SPEND ,
                                sum(PD_ACT_CNT) as PD_ACT_CNT
                                from Promo_Display_Final group by BRAND,DATE_ID')
    

    # Promo Search
    
    Promo_Search = dbGetQuery(conn, "SELECT * FROM PROMO_SEARCH")
    Promo_Search[,"DATE"] = as.Date(as.numeric(Promo_Search$DATE), origin = "1899-12-30")
    names(Promo_Search)[1] = "BRAND"
    names(Promo_Search)[2] = "DATE"
    names(Promo_Search)[3] = "PS_OUTLET"
    names(Promo_Search)[4] = "PS_ACT_CNT"
    names(Promo_Search)[5] = "PS_SPEND"
    names(Promo_Search)[6] = "PS_PLANNED_SPEND"
    names(Promo_Search)[7] = "PS_ESTIMATE_SPEND"
    
    #unique(Promo_Search$BRAND) #"HUMIRA"   "ELAGOLIX" "CREON"  "LUPRON DEPOT"
    # Changing BRAND name to upper case
    Promo_Search$BRAND = toupper(Promo_Search$BRAND)
    # Selecting HUMIRA, CREON, MAVYRET, LUPROn DEPOT BRAND Only
    Promo_Search_Final = subset(Promo_Search, (Promo_Search$BRAND %in% c("HUMIRA","CREON","LUPRON DEPOT","MAVYRET")))
    # Making DateID column
    Promo_Search_Final$DATE_ID = format(Promo_Search_Final$DATE,"%Y%m")
    # Summation of call duration group by DATE_ID
    Promo_Search_Final = sqldf('select BRAND,DATE_ID,sum(PS_ACT_CNT) as PS_ACT_CNT,
                               sum(PS_SPEND) as PS_SPEND
                               from Promo_Search_Final group by BRAND,DATE_ID')
    

    # Final DImension Table
    Final_Metric_Data = merge(merge(merge(
      Call_Activity_Final,
      Field_Email_Final,all = TRUE),
      Promo_Display_Final, all = TRUE),
      Promo_Search_Final, all = TRUE)
    
    # SALES data
    
    Final_Sales_Data = dbGetQuery(conn, "SELECT * FROM SALES")
    Final_Sales_Data[,"DATE"] = as.Date(as.numeric(Final_Sales_Data$DATE), origin = "1899-12-30")
    #unique(Final_Sales_Data$BRAND)
    Final_Sales_Data$DATE_ID = format(as.POSIXct(Final_Sales_Data$DATE,format='%m/%d/%Y'),"%Y%m")
    # Sales Data of aggregation
    Final_Sales_Data = sqldf('select BRAND, DATE_ID, sum(SALES) as SALES from Final_Sales_Data group by BRAND, DATE_ID')
    

    

    MMM_Dataset = merge(Final_Metric_Data,Final_Sales_Data)
    #---------------------------------- Data Loading Done -------------------------------
    
    ## 1. Reading Dataset
    #MMM_Dataset <<- MMM_Dataset # read.csv("Final_Dataset.csv", stringsAsFactors = F)
    
    
    
    ## As of Now Only HUMIRA as others have high missing values #########
    ## In final code this should be a variable based on selection at shiny app ##
    Final_Dataset_Brand <- subset(MMM_Dataset,MMM_Dataset$BRAND %in% c(InputBrand))
    Final_Dataset_Brand <- subset(Final_Dataset_Brand,Final_Dataset_Brand$DATE_ID>"201801" & Final_Dataset_Brand$DATE_ID<"201907") 
    Final_Dataset <- Final_Dataset_Brand[,-c(1,2)] 
    
    
    progress$inc(1/10, detail = "Outlier Treatment")
    ## 2. Outlier Treatment
    outlierMMM <- function(v){
       val <- v
       outlier <- boxplot.stats(v)$out
       ifelse(val %in% outlier, "!" ,val)
    }
    Final_Dataset[,1:(ncol(Final_Dataset)-1)] <- apply(Final_Dataset[,1:(ncol(Final_Dataset)-1)], 2, outlierMMM)
    for (i in 1:(ncol(Final_Dataset)-1)){
        Final_Dataset[,i] <- as.numeric(ifelse(Final_Dataset[,i]=="!",median(Final_Dataset[,i]),Final_Dataset[,i]))
    }

	
    progress$inc(1/10, detail = "Missing Value Treatment")
    #3. Missing Value Treatment
    missingColumns = names((which(colMeans(is.na(Final_Dataset)) > 0.5)))
    missingColumns = as.data.frame(missingColumns)
    if (nrow(missingColumns)>0){
      b = unique(brandData$BRAND)
      stop("File is invalid: Missing Data" )
      #Final_Dataset = Final_Dataset[,-which(colMeans(is.na(Final_Dataset)) > 0.5)]
    }else {Final_Dataset = Final_Dataset}
    
    if (anyNA(Final_Dataset)){
      miceMod <- mice(Final_Dataset,method = "rf")
      miceOutput <- complete(miceMod)
      Final_Dataset_No_Missing <- as.data.frame(miceOutput)
    }else{Final_Dataset_No_Missing <- Final_Dataset}
    
    if (anyNA(Final_Dataset_No_Missing)){
      knnOutput <- knnImputation(Final_Dataset_No_Missing)
      Final_Dataset_No_Missing <- as.data.frame(knnOutput)
    }else{Final_Dataset_No_Missing <- Final_Dataset_No_Missing}
    
    tmp <<- Final_Dataset_No_Missing
    tmp$BRAND <-InputBrand
    tmp$DATE_ID <- Final_Dataset_Brand$DATE_ID 
    
    
    
    
    progress$inc(1/10, detail = "Standardize predictors")
    # # 4. Standardize predictors
    #means <<- sapply(Final_Dataset_No_Missing[,1:ncol(Final_Dataset_No_Missing)-1],mean)
    #stdev <<- sapply(Final_Dataset_No_Missing[,1:ncol(Final_Dataset_No_Missing)-1],sd)
    #train.scaled <- as.data.frame(scale(Final_Dataset_No_Missing[,1:ncol(Final_Dataset_No_Missing)-1],center=means,scale=stdev))
    #train.scaled$SALES <- Final_Dataset_No_Missing$SALES
    #Final_Dataset_No_Missing <- train.scaled
    
    progress$inc(1/10, detail = "Split the data into training and test set")
    
    
    Final_Dataset_No_Missing <- sqldf('select CALL_DUR,EMAIL_SENT,EMAIL_CLICK,EMAIL_OPENED,PD_ACT_CNT,PS_ACT_CNT,SALES from Final_Dataset_No_Missing') 
    
    
	#5. Split the data into training and test set
    set.seed(20)
    training.samples <- Final_Dataset_No_Missing$SALES %>% createDataPartition(p = 0.75, list = FALSE)
    train.data  <- Final_Dataset_No_Missing[training.samples, ]
    test.data <- Final_Dataset_No_Missing[-training.samples, ]
    
    
    progress$inc(1/10, detail = "Linear Regression and check Data Linearity")
    #6. Linear Regression and check Data Linearity  

    
    MMM.LR <<- lm(SALES~., data = train.data)
    # regression condition check
    # a. linear relationship between outome and independent variables
    #plot(MMM.LR,1)
    
    # b. Normality of Residuals
    #plot(MMM.LR,2)
    
    # c. heteroscedasticity
    # Breush Pagan Test
    x1_LR <- lmtest::bptest(MMM.LR)  # Breusch-Pagan test
    # NCV Test
    x2_LR <- car::ncvTest(MMM.LR)  # non-constant variance
    if (x1_LR$p.value <0.05 || x2_LR$p<0.05){
      # BoxCox Transformation
      salesBCMod = caret::BoxCoxTrans(Final_Dataset_No_Missing$SALES) 
      bcDataset = cbind(Final_Dataset_No_Missing, sales_new = predict(salesBCMod, Final_Dataset_No_Missing$SALES))
      lmMod_bc <<- lm(sales_new ~ .-SALES, data=bcDataset)
      lmModel <<- "lmMod_bc"
    } else{
      lmModel <<- "MMM.LR"
    }
    if(lmModel == "lmMod_bc" ){
      predictions <- lmMod_bc %>% predict(test.data)
      RMSE_LR <- RMSE(predictions, test.data$SALES)
      RSQ_LR <- cor(predictions, test.data$SALES)^2
    }
    if(lmModel == "MMM.LR" ){
      predictions <- MMM.LR %>% predict(test.data)
      RMSE_LR <- RMSE(predictions, test.data$SALES)
      RSQ_LR <- cor(predictions, test.data$SALES)^2
    }
    
    # d. Multicollinearity
    multicol = car::vif(if(lmModel=="MMM.LR"){MMM.LR}else{lmMod_bc})
    multicol = as.data.frame(multicol)
    multicolSum = as.data.frame(lapply(multicol,function(multicol) ifelse((multicol>10), 1, 0)))
    multicolSum = sum(multicolSum)
    if(multicolSum>0){print("High Chance of Multicollinearity")}
    
    
    progress$inc(1/10, detail = "Building Regularized Models")
    
    # 7. Building Regularized Models
    lambda <- 10^seq(-3, 3, length = 100)
    # Build the ridge model
    ridge <<- train(
      SALES ~., data = train.data, method = "glmnet",
      trControl = trainControl("cv", number = 10),
      tuneGrid = expand.grid(alpha = 0, lambda = lambda)
    )
    # Make predictions
    predictions <- ridge %>% predict(test.data)
    # Model prediction performance
    RMSE_ridge = RMSE(predictions, test.data$SALES)
    RSQ_ridge = cor(predictions, test.data$SALES)^2
    
    # Build the LASSO model
    lasso <<- train(
      SALES ~., data = train.data, method = "glmnet",
      trControl = trainControl("cv", number = 10),
      tuneGrid = expand.grid(alpha = 1, lambda = lambda)
    )
    
    

    # Make predictions
    predictions <- lasso %>% predict(test.data)
    # Model prediction performance
    RMSE_lasso = RMSE(predictions, test.data$SALES)
    RSQ_lasso = cor(predictions, test.data$SALES)^2
    
    # Build the ELASTIC NET model
    #set.seed(123)
    elastic <<- train(
      SALES ~., data = train.data, method = "glmnet",
      trControl = trainControl("cv", number = 10),
      tuneLength = 10
    )
    # Make predictions
    predictions <- elastic %>% predict(test.data)
    # Model prediction performance
    RMSE_elastic = RMSE(predictions, test.data$SALES)
    RSQ_elastic = cor(predictions, test.data$SALES)^2
    
    
    progress$inc(1/10, detail = "Model Selection ")
    
    
    
    #8. Model Selection 
    tableRSQ <<- data.frame( "Model_Name" = c("Regression","RIDGE", "LASSO","Elastic Net"),
                             "Model" = c(lmModel,"ridge","lasso","elastic"),
                             "RMSE" = c(RMSE_LR,RMSE_ridge,RMSE_lasso,RMSE_elastic),
                             "Rsquare" = c(RSQ_LR,RSQ_ridge, RSQ_lasso, RSQ_elastic))
    
    if(multicolSum>0){
      modeltoSelect <<- sqldf('select Model_Name, Model,min(RMSE) as RMSE,Rsquare from tableRSQ where Model!="MMM.LR"')
    }else{modeltoSelect <<- sqldf('select Model_Name, Model,min(RMSE) as RMSE, Rsquare from tableRSQ')}
    
    if(modeltoSelect$Model == "MMM.LR"){
      tableCoeff = as.data.frame(MMM.LR$coefficients)
      colnames(tableCoeff) = "Coefficient"
    }
    if(modeltoSelect$Model == "lmMod_bc"){
      tableCoeff = as.data.frame(lmMod_bc$coefficients)
      colnames(tableCoeff) = "Coefficient"
    }
    if(modeltoSelect$Model == "ridge"){
      tableCoeff = as.data.frame(as.matrix(coef(ridge$finalModel, ridge$bestTune$lambda)))
      colnames(tableCoeff) = "Coefficient"
    }
    if(modeltoSelect$Model == "lasso"){
      tableCoeff = as.data.frame(as.matrix(coef(lasso$finalModel, lasso$bestTune$lambda)))
      colnames(tableCoeff) = "Coefficient"
    }
    if(modeltoSelect$Model == "elastic"){
      tableCoeff = as.data.frame(as.matrix(coef(elastic$finalModel, elastic$bestTune$lambda)))
      colnames(tableCoeff) = "Coefficient"
    }
    
    
    # ROI Calculation
    ROI_DF <<- tmp
    # Need to add spend column from source as CALL_SPEND.EMAIL_SPEND
    ROI_DF$CALL_SPEND <- tmp$PS_SPEND/97.13
    ROI_DF$EMAIL_SPEND <- ROI_DF$CALL_SPEND/11.23
    
    ROI_DF <- sqldf('select BRAND,DATE_ID,CALL_DUR,CALL_SPEND,EMAIL_SENT,EMAIL_CLICK,
                EMAIL_OPENED,EMAIL_SPEND,PD_ACT_CNT,PD_SPEND,PS_ACT_CNT,PS_SPEND,SALES from ROI_DF')
    
    ROI_DF$ROI_CALL = 100*(tableCoeff[2,1]/(ROI_DF$CALL_SPEND/ROI_DF$CALL_DUR))
    ROI_DF$ROI_EMAIL = 100*(tableCoeff[3,1]/(ROI_DF$EMAIL_SPEND/ROI_DF$EMAIL_SENT))
    ROI_DF$ROI_DISPLAY = 100*(tableCoeff[6,1]/(ROI_DF$PD_SPEND/ROI_DF$PD_ACT_CNT))
    ROI_DF$ROI_SEARCH = 100*(tableCoeff[7,1]/(ROI_DF$PS_SPEND/ROI_DF$PS_ACT_CNT))
    ROI_DF$ROI_OVERALL <- 100*(ROI_DF$SALES/(ROI_DF$CALL_SPEND+ROI_DF$EMAIL_SPEND+ROI_DF$PD_SPEND+ROI_DF$PS_SPEND)) 
    
    
    
    progress$inc(1/10, detail = "Insert values to DB")
    
    
    # 9. Insert values to DB
    dbExecute(conn,"DELETE FROM COEFF ")
    tableCoeff$BRAND <- InputBrand
    tableCoeff$DATE <- Sys.Date()
    tableCoeff$PREDICTORS <- c("Intercept","CALL_DUR","EMAIL_SENT","EMAIL_CLICK","EMAIL_OPENED","PD_ACT_CNT","PS_ACT_CNT")
    for (m in 1:nrow(tableCoeff)){
      query <- sprintf(
        "INSERT INTO COEFF VALUES ('%s','%s','%s','%s')",
        tableCoeff$BRAND[m],
        tableCoeff$DATE[m],
        tableCoeff$PREDICTORS[m],
        tableCoeff$Coefficient[m]
      )
      # Submit the update query and disconnect
      dbExecute(conn, query)
    }
    dbExecute(conn,"DELETE FROM RSQ")
    tableRSQ$BRAND <- InputBrand
    tableRSQ$DATE <- Sys.Date()
    for (n in 1:nrow(tableRSQ)){
      query <- sprintf(
        "INSERT INTO RSQ VALUES ('%s','%s','%s','%s','%s','%s')",
        tableRSQ$BRAND[n],
        tableRSQ$DATE[n],
        tableRSQ$Model_Name[n],
        tableRSQ$Model[n],
        tableRSQ$RMSE[n],
        tableRSQ$Rsquare[n]
      )
      # Submit the update query and disconnect
      dbExecute(conn, query)
    }
    dbExecute(conn,"DELETE FROM VIF_BRAND")
    vifTable = data.frame("BRAND" = InputBrand,
                          "DATE" = Sys.Date(),
                          "vif" = multicolSum)
    for (o in 1:nrow(vifTable)){
      query <- sprintf(
        "INSERT INTO VIF_BRAND VALUES ('%s','%s','%s')",
        vifTable$BRAND[o],
        vifTable$DATE[o],
        vifTable$vif[o]
      )
      # Submit the update query and disconnect
      dbExecute(conn, query)
    }
    dbExecute(conn,"DELETE FROM MODEL_BRAND")
    modelSelected = as.data.frame(modeltoSelect$Model)
    modelSelected$BRAND <- InputBrand
    modelSelected$DATE <- Sys.Date()
    colnames(modelSelected) <- c("MODEL","BRAND","DATE")
    for (p in 1:nrow(modelSelected)){
      query <- sprintf(
        "INSERT INTO MODEL_BRAND VALUES ('%s','%s','%s')",
        modelSelected$BRAND[p],
        modelSelected$DATE[p],
        modelSelected$MODEL[p]
      )
      # Submit the update query and disconnect
      dbExecute(conn, query)
    }
    
    
    dbExecute(conn,"DELETE FROM ROI_DF")
    for (m in 1:nrow(ROI_DF)){
      query <- sprintf(
        "INSERT INTO ROI_DF VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')",
        ROI_DF$BRAND[m],
        ROI_DF$DATE_ID[m],
        ROI_DF$CALL_DUR[m],
        ROI_DF$CALL_SPEND[m],
        ROI_DF$EMAIL_SENT[m],
        ROI_DF$EMAIL_CLICK[m],
        ROI_DF$EMAIL_OPENED[m],
        ROI_DF$EMAIL_SPEND[m],
        ROI_DF$PD_ACT_CNT[m],
        ROI_DF$PD_SPEND[m],
        ROI_DF$PS_ACT_CNT[m],
        ROI_DF$PS_SPEND[m],
        ROI_DF$SALES[m],
        ROI_DF$ROI_CALL[m],
        ROI_DF$ROI_EMAIL[m],
        ROI_DF$ROI_DISPLAY[m],
        ROI_DF$ROI_SEARCH[m],
        ROI_DF$ROI_OVERALL[m]
      )
      # Submit the update query and disconnect
      dbExecute(conn, query)
    } 
    
    write.csv(ROI_DF,"ROI_data.csv")
    
    
    dbDisconnect(conn)
    
    showElement(id ="simPlot" )
    showElement(id ="Accuracy" )
    showElement(id ="mktStr" )
    showElement(id ="test" )
    
    progress$inc(1/10, detail = "Model Execution Complete")
    removeCssClass(selector = "a[data-value='ROI_Analysis']", class = "inactiveLink")


  })
  
  
  
  observeEvent(input$get_recommendation, {
    #InputBrand<-as.character(input$Brand_P1)
    
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Model Execution", value = 0)
    #hideElement(id="text1")
    #hideElement(id="text2")
    
    Sys.sleep(1.5)
    
    showElement(id = "New_Page2Row_3")
    showElement(id = "New_Page2Row_4")
    
    progress$inc(1 / 10, detail = "Model Execution Complete")
    #removeCssClass(selector = "a[data-value='ROI_Analysis']", class = "inactiveLink")
  })
  
  
  
  
  
  
  

  
  #########################################
  # FILE SAVE TO THE DATABASE
  #########################################  
  
  
  
  observeEvent(input$filesave, {
    inFile <- input$file1
    if (is.null(inFile)){
      showNotification(h3("Upload File First."),type = "error",duration = 3,closeButton = F)
      return()
    }
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    
    showNotification(h3("Upload In Progress."),id = "InProgressNot",type = "warning",closeButton = T)
    
    if(input$filetype=='CALL ACTIVITY'){
      
      c_name=c("TERRITORY_DESCRIPTION",
               "SOURCE_BRAND_NAME",
               "CALL_DUR",
               "DATE")
      
      df=readxl::read_excel(inFile$datapath,sheet = input$sheetno,na = input$nullvalues, col_names = c_name,skip = 1,col_types = "text")
     
      dbWriteTable(conn, "CALL_ACTIVITY", df,append=T)
    }
    else if(input$filetype=='EMAIL'){
      
      c_name=c("DATE",
               "DISPLAY_BRAND_NAME",
               "EMAIL_COUNT",
               "EMAIL_UNIQUE_OPEN",
               "TOTAL_EMAIL_OPENED",
               "TOTAL_EMAIL_CLICK",
               "CUSTOMER_TYPE_DESCRIPTION")
      
      df=readxl::read_excel(inFile$datapath,sheet = input$sheetno,na = input$nullvalues, col_names = c_name,skip = 1,col_types = "text")
      
      dbWriteTable(conn, "EMAIL", df,append=T)
      
    }
    else if(input$filetype=='PROMO DISPLAY'){
      
      c_name=c("BRAND_NAME",
               "SPEND_AMT",
               "PLANNED_SPEND_AMT",
               "ACTIVITY_COUNT",
               "ESTIMATE_SPEND_AMT",
               "MEDIA_TYPE",
               "DATE",
               "GEOGRAPHY")
      
      df=readxl::read_excel(inFile$datapath,sheet = input$sheetno,na = input$nullvalues, col_names = c_name,skip = 1,col_types = "text")
      
      dbWriteTable(conn, "PROMO_DISPLAY", df,append=T)

    }
    else if(input$filetype=='PROMO SEARCH'){
      c_name=c("BRAND", 
               "DATE",
               "OUTLET",
               "ACTIVITY_COUNT",
               "SPEND_AMT",
               "PLANNED_SPEND_AMT",
               "ESTIMATE_SPEND_AMT")
      
      df=readxl::read_excel(inFile$datapath,sheet = input$sheetno,na = input$nullvalues, col_names = c_name,skip = 1,col_types = "text")
      
      dbWriteTable(conn, "PROMO_SEARCH", df,append=T)
     
    }    
    else if(input$filetype=='SPEND METRICS'){
      c_name=c("BRAND", 
               "DATE",
               "VENDOR_REPORTED_ACTIVITY",
               "TOTAL_SPEND_AMT",
               "METRIC_CATEGORY",
               "METRIC_TYPE",
               "SPEND_TYPE")
      
      df=readxl::read_excel(inFile$datapath,sheet = input$sheetno,na = input$nullvalues, col_names = c_name,skip = 1,col_types = "text")
      
      dbWriteTable(conn, "SPEND_METRICS", df,append=T)
      
     
    } 
    else if(input$filetype=='SALES'){
     
       c_name=c("BRAND", 
               "DATE",
               "SALES",
               "COMPETITOR_NAME",
               "FRANCHISE_NAME")
      
      df=readxl::read_excel(inFile$datapath,sheet = input$sheetno,na = input$nullvalues, col_names = c_name,skip = 1,col_types = "text")
      
      dbWriteTable(conn, "SALES", df,append=T)

    } 
    dbDisconnect(conn)
    removeNotification(id = "InProgressNot", session = getDefaultReactiveDomain())
    # Visualize the new table after deletion
    file.copy(inFile$datapath, file.path("D:\\Shiny_Radius\\Datasets", inFile$name) )
    showNotification(h3("Upload Successful."),type = "message",duration = 3,closeButton = F)
    shinyjs::reset("file1")
    shinyjs::reset("filetype")
    shinyjs::toggle(id = "fileUpOut")
  })
  
  
  
  
  
  
  #########################################
  # DOWNLOADING THE FILE TEMPLATE
  #########################################
  
  
  
  output$downloadData <- downloadHandler(
    
   filename = function() {
      paste(input$filetype, ".xlsx", sep = "")
   },
   
   
   content <- function(file) {
     file.copy(paste("C:\\Users\\698190\\Documents\\Shiny_Radius\\upload\\",input$filetype, ".xlsx", sep = ""), file)
   }
  )


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  #########################################
  # SALES KPI
  #########################################  
  
  salesplotfunc<-function(brand){
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    
    comp_data = dbGetQuery(conn, paste0("SELECT * FROM Sales where brand='",brand,"'" ))
    comp_data$DATE = as.Date(as.numeric(comp_data$DATE), origin = "1899-12-30")
    dbDisconnect(conn)
    
    comp_data[is.na(comp_data)] <- 0
    
    DF2<-comp_data %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%group_by(month, year,BRAND) %>%summarise(total = sum(SALES))
    
    
    DF2$Date<-ymd(paste(DF2$year,DF2$month,"01", sep="-"))
    DF2 = subset(DF2, select = -c(1,2) )
    return(DF2)
  }
  
  #########################################
  # UPDATE DATE SLIDER AND SALES DATA
  #########################################
  
  observeEvent(input$Brand_P1,{
    
    Sales_Summarized <- salesplotfunc(input$Brand_P1)
    
    updateSliderInput(session,"DateFilter_P1",min = min(Sales_Summarized$Date),
                      max = max(Sales_Summarized$Date),
                      value=c(min(Sales_Summarized$Date),max(Sales_Summarized$Date)),timeFormat="%b %Y")
    
    
    
    
  })
  
  
  
  #########################################
  # SALES KPI PLOT RENDER
  #########################################
  
  output$saleoutputs <- renderUI({
    tabsetPanel(id="salespanel",
                
                tabPanel("Sales",value = '1',fluidRow(withSpinner(plotlyOutput("salesplot"),type = 1),div(style="text-align: center;",downloadButton("Save_salesplot"),style="padding-top:27px; display:center-align;"))),
                tabPanel("Sales Growth",value='2',withSpinner(plotlyOutput("growthplot"),type = 1),div(style="text-align: center;",downloadButton("Save_salesgrowthplot"),style="padding-top:27px; display:center-align;"))
    )
    
  })
  
  output$Save_salesplot <- downloadHandler(
    filename = function(){
      if(input$Brand_P1=="MAVYRET")
        brand_name <- "Brand A"
      else if(input$Brand_P1=="HUMIRA")
        brand_name <- "Brand B"
      else if(input$Brand_P1=="LUPRON DEPOT")
        brand_name <- "Brand C"
      else if(input$Brand_P1=="CREON")
        brand_name <- "Brand D"
      paste0(brand_name,"_sales.csv")
    },
    content = function(file){
      path <- paste0("www/sales_data.csv")
      file.copy(path,file)
    }
  )
  
  output$Save_salesgrowthplot <- downloadHandler(
    filename = function(){
      if(input$Brand_P1=="MAVYRET")
        brand_name <- "Brand A"
      else if(input$Brand_P1=="HUMIRA")
        brand_name <- "Brand B"
      else if(input$Brand_P1=="LUPRON DEPOT")
        brand_name <- "Brand C"
      else if(input$Brand_P1=="CREON")
        brand_name <- "Brand D"
      paste0(brand_name,"_salesgrowth.csv")
    },
    content = function(file){
      path <- paste0("www/salesgrowth_data.csv")
      file.copy(path,file)
    }
  )
  
  output$growthplot <- renderPlotly({
    
    Sales_Summarized <- salesplotfunc(input$Brand_P1)
    DF3<-Sales_Summarized%>%dplyr::filter(Date>=input$DateFilter_P1[1])%>%dplyr::filter(Date<=input$DateFilter_P1[2])%>%dplyr::arrange(Date)
    DF3$Monthly_Growth<-0
    for(i in seq(2,nrow(DF3))){
      DF3$Monthly_Growth[i]<-((DF3$ total[i]-DF3$ total[i-1])/DF3$ total[i-1])*100
    }
    DF3$Monthly_Growth <- round(DF3$Monthly_Growth,0)
    DF3$Monthly_Growth <- as.numeric(DF3$Monthly_Growth)
    save_data <- DF3[,c(3:4)]
    colnames(save_data) <- c("Date","Monthly_Growth %")
    write.csv(save_data,"www/salesgrowth_data.csv",row.names = F)
    
    ax <- list(
      title = "DATE",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = TRUE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    ay <- list(
      title = "Monthly Growth Percentage",
      showticklabels = TRUE,
      showgrid = TRUE
    )
    
    plot_ly(DF3, x = ~Date, 
            y = ~Monthly_Growth, 
            name = 'trace 0', 
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'rgb(205, 12, 24)',width = 2))%>%
      plotly::layout( height = 425,yaxis = ay,xaxis=ax)
  })
  
  
  
  output$salesplot <- renderPlotly({
    
    Sales_Summarized <- salesplotfunc(input$Brand_P1)
    DF3<-Sales_Summarized%>%dplyr::filter(Date>=input$DateFilter_P1[1])%>%dplyr::filter(Date<=input$DateFilter_P1[2])%>%dplyr::arrange(Date)
    DF3$Monthly_Growth<-0
    for(i in seq(2,nrow(DF3))){
      DF3$Monthly_Growth<-((DF3$Monthly_Growth[i]-DF3$Monthly_Growth[i-1])/DF3$Monthly_Growth[i-1])*100
    }
    
    save_data <- DF3[,c(2:3)]
    colnames(save_data) <- c("SALES in Million $","Date")
    write.csv(save_data,"www/sales_data.csv",row.names = F)
    ax <- list(
      title = "DATE",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = TRUE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    ay <- list(
      title = "SALES in Million $",
      showticklabels = TRUE,
      showgrid = TRUE
    )
    
    plot_ly(DF3, x = ~Date, 
            y = ~total, 
            name = 'trace 0', 
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'rgb(205, 12, 24)', width = 2))%>%
      plotly::layout( height = 425,xaxis = ax, yaxis = ay)
  })
  
  
  
  
  
  
  
  #########################################
  # MARKET SIMULATOR
  #########################################
  
  
  output$text1 <- renderText({"Please execute the Simulator model to view the result"})
  output$text2 <- renderText({"Please execute the Simulator model to view the result"})
  
  output$brandBox <- renderInfoBox({
    infoBox(
      "Selected Brand", ifelse(input$Brand_P1=="HUMIRA","BRAND B",ifelse(input$Brand_P1=="MAVYRET","BRAND A",ifelse(input$Brand_P1=="LUPRON DEPOT","BRAND C",ifelse(input$Brand_P1=="CREON","BRAND D")))), icon = icon("prescription-bottle-alt",lib = "font-awesome"),
      color = "purple",fill = TRUE
    )
  })
  
  competitor_brands<-function(){
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    comp_data = dbGetQuery(conn, paste0("SELECT * FROM Sales where upper(Competitor_name) = '",input$Brand_P1,"'" )) ## change for selecting brand
    comp_data$DATE = as.Date(as.numeric(comp_data$DATE), origin = "1899-12-30")
    comp_data$DATE = format(comp_data$DATE,"%Y%m")
    comp_data_final = sqldf('select upper(BRAND) as BRAND,FRANCHISE_NAME,DATE,sum(SALES) as SALES
                            from comp_data group by BRAND,FRANCHISE_NAME,DATE')

    dbDisconnect(conn) 
    return(competitor_brand)
  }
  
  market_sim_data <- function(){
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    own_brand_data = dbGetQuery(conn, paste0("SELECT * FROM Sales where upper(brand) = '",input$Brand_P1,"'") ) ## change for selecting brand
    own_brand_data$DATE = as.Date(as.numeric(own_brand_data$DATE), origin = "1899-12-30")
    own_brand_data$DATE = format(own_brand_data$DATE,"%Y%m")
    own_brand_final = sqldf('select upper(BRAND) as BRAND,DATE,sum(SALES) as SALES
                            from own_brand_data group by BRAND,DATE')
    
    df_pred = select(own_brand_final,DATE,SALES)
    df_pred %>% group_by(DATE)
    comp_data = dbGetQuery(conn, paste0("SELECT * FROM Sales where upper(Competitor_name) = '",input$Brand_P1,"'" )) ## change for selecting brand
    comp_data$DATE = as.Date(as.numeric(comp_data$DATE), origin = "1899-12-30")
    comp_data$DATE = format(comp_data$DATE,"%Y%m")
    comp_data_final <<- sqldf('select upper(BRAND) as BRAND,FRANCHISE_NAME,DATE,sum(SALES) as SALES
                            from comp_data group by BRAND,FRANCHISE_NAME,DATE')
    franchise <<- unique(comp_data_final$FRANCHISE_NAME) 
    competitor_brand  <<- unique(comp_data_final$BRAND)
    
    ##########call data#############
    call = dbGetQuery(conn, paste0("SELECT * FROM CALL_ACTIVITY where upper(SOURCE_BRAND_NAME) = '",input$Brand_P1,"'") )## change for selecting brand
    call$DATE = as.Date(as.numeric(call$DATE), origin = "1899-12-30")
    call$DATE = format(call$DATE,"%Y%m")
    call_data_final = sqldf('select upper(SOURCE_BRAND_NAME) as BRAND,DATE,sum(CALL_DUR) as CALL_DUR
                            from call group by SOURCE_BRAND_NAME,DATE')
    call_latest<<-filter(call_data_final, DATE == max(call_data_final$DATE))
    call_data_final$CALL_DUR = call_data_final$CALL_DUR / 100000
    
    ##########call data#############
    ##########PROMO_DISPLAY data#############
    display = dbGetQuery(conn, paste0("SELECT * FROM PROMO_DISPLAY where upper(BRAND_NAME) = '",input$Brand_P1,"'") )## change for selecting brand
    display$DATE = as.Date(as.numeric(display$DATE), origin = "1899-12-30")
    display$DATE = format(display$DATE,"%Y%m")
    display_final = sqldf('select upper(BRAND_NAME) as BRAND,DATE,sum(ACTIVITY_COUNT) as DISPLAY_ACTIVITY_COUNT,
                          sum(SPEND_AMT) as DISPLAY_SPEND_AMT from display group by BRAND_NAME,DATE')
    display_latest <<- filter(display_final, DATE == max(display_final$DATE))
    display_final$DISPLAY_ACTIVITY_COUNT = display_final$DISPLAY_ACTIVITY_COUNT / 100000000
    ##########PROMO_DISPLAY data#############
    ##########PROMO_SEARCH data#############
    search = dbGetQuery(conn, paste0("SELECT * FROM PROMO_SEARCH where upper(BRAND) = '",input$Brand_P1,"'")) ## change for selecting brand
    search$DATE = as.Date(as.numeric(search$DATE), origin = "1899-12-30")
    search$DATE = format(search$DATE,"%Y%m")
    search_final = sqldf('select upper(BRAND) as BRAND,DATE,sum(ACTIVITY_COUNT) as SEARCH_ACTIVITY_COUNT,
                         sum(SPEND_AMT) as SEARCH_SPEND_AMT from search group by BRAND,DATE')
    search_latest <<- filter(search_final, DATE == max(search_final$DATE))
    search_final$SEARCH_ACTIVITY_COUNT = search_final$SEARCH_ACTIVITY_COUNT / 1000000
    
    ##########PROMO_SEARCH data#############
    ##########EMAIL data#############
    email = dbGetQuery(conn, paste0("SELECT * FROM EMAIL where upper(DISPLAY_BRAND_NAME) = '",input$Brand_P1,"'")) ## change for selecting brand
    email$DATE = as.Date(as.numeric(email$DATE), origin = "1899-12-30")
    email$DATE = format(email$DATE,"%Y%m")
    email_final = sqldf('select upper(DISPLAY_BRAND_NAME) as BRAND ,DATE,sum(EMAIL_COUNT) as EMAIL_ACTIVITY_COUNT,
                        sum(TOTAL_EMAIL_OPENED) as TOTAL_EMAIL_OPENED, sum(TOTAL_EMAIL_CLICK) as TOTAL_EMAIL_CLICK 
                        from email group by DISPLAY_BRAND_NAME,DATE')
    email_latest <<- filter(email_final, DATE == max(email_final$DATE))
    email_final$EMAIL_ACTIVITY_COUNT = email_final$EMAIL_ACTIVITY_COUNT / 100
    
    ##########EMAIL data#############
    final_data = merge(own_brand_final,merge(display_final[,1:3],merge(search_final[,1:3],merge(email_final[,1:3],call_data_final))))
    dbDisconnect(conn) 
    return(final_data)
  }  

  
  
  
  output$mktStr <- renderPlot({
    df_final <- data.frame()
    x <- c("brand", "prediction")
    brand <- vector()
    prediction <- vector()
    for (comp in competitor_brand){
      cmp_data = filter(comp_data_final, BRAND  == comp)
      cmp_data = select(cmp_data,SALES )
      timeseries = ts(cmp_data, frequency = 12, start=c(2016,1))
      decomp_ts = decompose(timeseries)
      ts_sa = timeseries - decomp_ts$seasonal
      ts_forecast <- HoltWinters(ts_sa, gamma=FALSE)
      # plot(ts_forecast, main = comp)
      ts_forecast2 <- forecast(ts_forecast, h=input$pred) 
      # plot(ts_forecast2, main = comp  )
      ul = unlist(ts_forecast2[["mean"]])
      avg = tail(ul, n=1)
      brand <- append(brand, comp)
      prediction <- append(prediction,avg)
      
    }
    df_final <- data.frame(brand = brand, prediction = prediction)
    mkt_data = filter(comp_data_final, FRANCHISE_NAME == input$market)
    df_final = df_final%>%filter(brand %in% unique(mkt_data$BRAND))
    
    names(df_final)[1] = "name"
    names(df_final)[2] = "value"
    
    renderPieChart(data = df_final, 
                   div_id = "test", 
                   radius = 200,
                   center_x = "50%",
                   center_y = "30%",
                   show.label = TRUE,
                   show.legend = TRUE, show.tools = TRUE,
                   font.size.legend= 8,
                   animation = TRUE )
    
  })
  
  
  
  
  # Show the values in an HTML table ----
  output$simPlot <- renderPlotly({
    
    
    final_data<-market_sim_data()
    
    
    
    # email_const = email_clicked*email_latest$TOTAL_EMAIL_CLICK +
    #               email_open*email_latest$TOTAL_EMAIL_OPENED
    email_pred = email_latest$EMAIL_ACTIVITY_COUNT*(1+(input$email/100))
    call_pred = call_latest$CALL_DUR*(1+(input$call/100))
    search_pred = search_latest$SEARCH_ACTIVITY_COUNT*(1+(input$search/100))
    
    # search_const = search_spend*search_latest$SEARCH_SPEND_AMT
    
    display_pred = display_latest$DISPLAY_ACTIVITY_COUNT*(1+(input$display/100))
    # display_const = disp_spend * display_latest$DISPLAY_SPEND_AMT
    # sales_pred = email_pred + email_const + call_pred + search_pred + search_const
    # + display_pred + display_const + intercept
    
    
    
    data2 = data.frame( "CALL_DUR"= call_latest$CALL_DUR*(1+(input$call/100)),
                        "EMAIL_SENT" = email_latest$EMAIL_ACTIVITY_COUNT*(1+(input$email/100)),
                        "EMAIL_CLICK"=email_latest$TOTAL_EMAIL_CLICK ,
                        "EMAIL_OPENED"=email_latest$TOTAL_EMAIL_OPENED,
                        "PD_SPEND" = display_latest$DISPLAY_SPEND_AMT,
                        "PD_ACT_CNT" = display_latest$DISPLAY_ACTIVITY_COUNT*(1+(input$display/100)),
                        "PS_ACT_CNT" = search_latest$SEARCH_ACTIVITY_COUNT*(1+(input$search/100)),
                        "PS_SPEND" = search_latest$SEARCH_SPEND_AMT)
    #data2.scaled <- as.data.frame(scale(data2,center=means,scale=stdev))
    
	data2.scaled <-data2

    
    if(modeltoSelect$Model == "MMM.LR"){
      sales_pred <- predict(MMM.LR,newdata = data2.scaled)
    }
    if(modeltoSelect$Model == "lmMod_bc"){
      sales_pred <- predict(lmMod_bc,newdata = data2.scaled)
    }
    if(modeltoSelect$Model == "ridge"){
      sales_pred <- ridge %>% predict(data2.scaled)
    }
    if(modeltoSelect$Model == "lasso"){
      sales_pred <- lasso %>% predict(data2.scaled)
    }
    if(modeltoSelect$Model == "elastic"){
      sales_pred <- elastic %>% predict(data2.scaled)
    }
    
    pred = data.frame(BRAND = input$Brand_P1, DATE = "Predicted", SALES = sales_pred, 
                      DISPLAY_ACTIVITY_COUNT = display_pred / 100000000, SEARCH_ACTIVITY_COUNT = search_pred / 1000000,
                      EMAIL_ACTIVITY_COUNT = email_pred / 100, CALL_DUR = call_pred/100000) ##update for brand selection
    pred <<- pred
    final_data =rbind(final_data,pred)
    
    final_data_tempr <<- final_data
    print("final_data_tempr")
    print(final_data_tempr)
    sim_disp_col = c(rep('rgb(119,136,153)', nrow(final_data)-1), 'rgb(192,192,192)' )
    sim_search_col = c(rep('rgb(255,99,71)', nrow(final_data)-1), 'rgb(255,160,122)')
    sim_call_col = c(rep('rgb(210, 105, 30)', nrow(final_data)-1), 'rgb(210,180,140)')
    sim_email_col = c(rep('rgb(32,178,170)', nrow(final_data)-1), 'rgb(135,206,250)')
    
    
    # plot_ly(final_data, x = ~DATE, y = ~SALES, type = "scatter", mode = "lines")
    
    
    # final_data = normalize(final_data, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
    
    plot_ly(final_data, x = ~DATE, y = ~DISPLAY_ACTIVITY_COUNT, type = 'bar', name = 'Display', marker = list(color = sim_disp_col) ,text = ~paste(DISPLAY_ACTIVITY_COUNT, 'Hundred Million')) %>%
      add_trace(y = ~SEARCH_ACTIVITY_COUNT, name = 'Search', marker = list(color = sim_search_col), text = ~paste(SEARCH_ACTIVITY_COUNT,'Million')) %>%
      add_trace(y = ~EMAIL_ACTIVITY_COUNT, name = 'Email', marker = list(color = sim_email_col), text = ~paste(EMAIL_ACTIVITY_COUNT, 'Hundred')) %>%
      add_trace(y = ~CALL_DUR, name = 'Call', marker = list(color = sim_call_col),text = ~paste(CALL_DUR, 'Hundred Thousand')) %>%
      add_trace(y = ~SALES, name = "Sales", type = "scatter", mode = "lines+markers" , yaxis = "y2", marker = list(color = 'rgb(55, 50, 109)'), line = list(color = 'rgb(0, 0, 0)') ,text = ~paste(SALES,'')) %>%
      layout( yaxis = list(title = 'Activity Count'), barmode = 'stack',
              yaxis2 = list(side = 'right', overlaying = "y", title = 'Sales', showgrid = FALSE, zeroline = FALSE) ) 
    # add_annotations( y = ~SALES, text = c(rep('',nrow(final_data)-1), '80%' ), showarrow = TRUE,
    #                  arrowhead = 4,
    #                  arrowsize = .5,
    #                  ax = 20,
    #                  ay = -40, )
    
  })
  
  output$Accuracy <- renderPlotly({
    if(modeltoSelect$Rsquare >=0 & modeltoSelect$Rsquare <=0.50){
      accuracyLevel = "Poor"
      sim_acc_col = c('white','#ff0000')
    }else if(modeltoSelect$Rsquare >0.50 & modeltoSelect$Rsquare <=0.90){
      accuracyLevel = "Fair"
      sim_acc_col = c('white','#f8de00')
    }else if (modeltoSelect$Rsquare >0.90 & modeltoSelect$Rsquare <=0.99 ){
      accuracyLevel = "Good"
      sim_acc_col = c('white','#00ff00')
      
    }else if (modeltoSelect$Rsquare >0.99){
      accuracyLevel = "Very Good"
      sim_acc_col = c('white','#58a758')
    }
    
    plot_ly(
      type = 'table',
      header = list(
        values = c('<b>Prediction Accuracy</b>', accuracyLevel),
        # line = list(color = '#506784'),
        fill = list(color = sim_acc_col),
        # align = c('left','center'),
        font = list(color = c('black','white'), size = 12)
      )
    )
    
    
  })
  
  #########################################
  # MAPS
  #########################################  
  
  callmapdata<-function(brand){
    
    #########################################
    # CALL ACTIVITY MAP 
    #########################################
    
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    call_map_df = dbGetQuery(conn, paste0("SELECT * FROM CALL_ACTIVITY where upper(SOURCE_BRAND_NAME)='",brand,"'"))
    call_map_df[,"DATE"] = as.Date(as.numeric(call_map_df$DATE), origin = "1899-12-30")
    dbDisconnect(conn)
    us_lat_long_df<-maps::us.cities
    
    
    #REMOVING STATES NMAES FROM THE END
    us_lat_long_df$name<-str_sub(us_lat_long_df$name,1,-4)
    
    
    #CONVERTING TO UPPER CASE CALL DATA
    us_lat_long_df$name<-stringr::str_to_upper(us_lat_long_df$name)
    call_map_df$TERRITORY_DESCRIPTION<-stringr::str_to_upper(call_map_df$TERRITORY_DESCRIPTION)
    
    
    #CHANGING SOME PREFIX BEFORE JOIN
    call_map_df$TERRITORY_DESCRIPTION<-str_replace(call_map_df$TERRITORY_DESCRIPTION, "ST. ","SAINT ")
    call_map_df$TERRITORY_DESCRIPTION<-str_replace(call_map_df$TERRITORY_DESCRIPTION, "ST ","SAINT ")
    call_map_df$TERRITORY_DESCRIPTION<-str_replace(call_map_df$TERRITORY_DESCRIPTION, "FT ","FORT ")
    
    
    #REMOVING THE NORTH EAST SOUTH VALUES OF THE CITIES
    us_lat_long_df$name<-str_replace(us_lat_long_df$name, " [ESWN]$","")
    
    call_map_df$TERRITORY_DESCRIPTION<-str_replace(call_map_df$TERRITORY_DESCRIPTION, " [ESWN]$","")
    
    
    #SELECTING ONLY USEFUL COLUMNS
    us_lat_long_df<-us_lat_long_df%>%select(name,lat,long)
    
    
    #REMOVING DUPLICATE CITES
    us_lat_long_df<-us_lat_long_df[!duplicated(us_lat_long_df$name),]
    
    
    
    #JOINING THE DATAFRAMES
    Call_Activity_map_df=merge(x = call_map_df, y =us_lat_long_df, by.x = "TERRITORY_DESCRIPTION",by.y="name")
    return(Call_Activity_map_df)
      
  }
  
  
  output$callactivitymap <- renderLeaflet({
    Call_Activity_map_df<-callmapdata(input$Brand_P1)
    Call_Activity_map_df2<-Call_Activity_map_df%>%group_by(TERRITORY_DESCRIPTION,SOURCE_BRAND_NAME,lat,long)%>%summarise(DUR=mean(CALL_DUR,na.rm=TRUE))%>%filter(toupper(SOURCE_BRAND_NAME)==input$Brand_P1)
    
    Call_Activity_map_df2<-Call_Activity_map_df2%>%filter(TERRITORY_DESCRIPTION!='HONOLULU')%>%filter(!is.na(DUR))
  
    Call_Activity_map_df2$DUR<-floor(Call_Activity_map_df2$DUR)
    
    
    pal <- colorNumeric(palette="BrBG", domain=Call_Activity_map_df2$DUR)
    
    labs <- lapply(seq(nrow(Call_Activity_map_df2)), function(i) {
      paste0( '<p><b>City: ', Call_Activity_map_df2[i, "TERRITORY_DESCRIPTION"], '</b><p></p>Avg Duration (Mins): ', 
              Call_Activity_map_df2[i, "DUR"],'</p><p>') 
    })
    
    leaflet(data = Call_Activity_map_df2) %>% addTiles() %>%
      addCircleMarkers(lng =  ~long, lat=~lat,color = ~pal(DUR),opacity = 0.9,
                       label =  lapply(labs, htmltools::HTML),radius =6)%>%
      addLegend("bottomright", pal = pal, values = ~DUR,
                title = "Avg Duration (Mins)",
                
                opacity = 1
      )
    
  }
    
  )
  
  
  #########################################
  # PROMO DISPLAY MAP 
  #########################################
  
  promomapfunc<-function(brand){
    
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    promo_display_df <- dbGetQuery(conn, paste0("select BRAND_NAME,ESTIMATE_SPEND_AMT,DATE,GEOGRAPHY from PROMO_DISPLAY where upper(BRAND_NAME)='",brand,"'"))
    promo_display_df$DATE <- as.Date(as.numeric(promo_display_df$DATE), origin = "1899-12-30") 
    write.csv(promo_display_df,"data.csv")
    us_lat_long_df<-maps::us.cities
    dbDisconnect(conn)
    
    #REMOVING STATES NMAES FROM THE END
    us_lat_long_df$name<-str_sub(us_lat_long_df$name,1,-4)
    
    
    #CONVERTING TO UPPER CASE PROMO DATA
    us_lat_long_df$name<-stringr::str_to_upper(us_lat_long_df$name)
    promo_display_df$GEOGRAPHY<-stringr::str_to_upper(promo_display_df$GEOGRAPHY)
    
    
    unique(promo_display_df$GEOGRAPHY)
    
    #CHANGING SOME PREFIX BEFORE JOIN
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "_"," ")
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "-"," ")
    
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "ST. ","SAINT ")
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "ST ","SAINT ")
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "FT ","FORT ")
    
    
    #REMOVING THE NORTH EAST SOUTH VALUES OF THE CITIES
    us_lat_long_df$name<-str_replace(us_lat_long_df$name, " [ESWN]$","")
    
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, " [ESWN]$","")
    
    
    #SELECTING ONLY USEFUL COLUMNS
    us_lat_long_df<-us_lat_long_df%>%select(name,lat,long)
    
    
    #REMOVING DUPLICATE CITES
    us_lat_long_df<-us_lat_long_df[!duplicated(us_lat_long_df$name),]
    
    
    
    #JOINING THE DATAFRAMES
    PROMO_DISPLAY_map_df=merge(x = promo_display_df, y =us_lat_long_df, by.x = "GEOGRAPHY",by.y="name")
    
    
    
    return(PROMO_DISPLAY_map_df)
    
    
  }
  
  
  
  
  
  
  
  output$promodisplaymap <- renderLeaflet({
    PROMO_DISPLAY_map_df<-promomapfunc(input$Brand_P1)
    PROMO_DISPLAY_map_df2<-PROMO_DISPLAY_map_df%>%group_by(GEOGRAPHY,BRAND_NAME,lat,long)%>%summarise(ESTIMATE_SPEND_AMT=mean(ESTIMATE_SPEND_AMT,na.rm=TRUE))%>%filter(BRAND_NAME==input$Brand_P1)%>%
      filter(GEOGRAPHY!='HONOLULU')%>%filter(GEOGRAPHY!='JUNEAU')%>%filter(GEOGRAPHY!='ANCHORAGE')%>%filter(!is.na(ESTIMATE_SPEND_AMT))
    
    PROMO_DISPLAY_map_df2$ESTIMATE_SPEND_AMT<-floor(PROMO_DISPLAY_map_df2$ESTIMATE_SPEND_AMT)
    pal <- colorNumeric(palette="RdGy", domain=PROMO_DISPLAY_map_df2$ESTIMATE_SPEND_AMT)
    
    labs <- lapply(seq(nrow(PROMO_DISPLAY_map_df2)), function(i) {
      paste0( '<p><b>City: ', PROMO_DISPLAY_map_df2[i, "GEOGRAPHY"], '</b><p></p>Spend: ', 
              PROMO_DISPLAY_map_df2[i, "ESTIMATE_SPEND_AMT"],'</p><p>') 
    })
    
    leaflet(data = PROMO_DISPLAY_map_df2) %>% addTiles() %>%
      addCircleMarkers(lng =  ~long, lat=~lat, color = ~pal(ESTIMATE_SPEND_AMT),opacity = 0.9,
                       label =  lapply(labs, htmltools::HTML),radius = 6)%>%
      addLegend("bottomright", pal = pal, values = ~ESTIMATE_SPEND_AMT,
                title = "Avg Spend Amount",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
    
    

    
    
  }
  
  )
  
  
  #########################################
  # 698190 INTEGRATION CODE
  #########################################  
  
  output$New_PageRow1 <- renderUI({
    fluidRow(  column(2),infoBoxOutput("brandBox_2",width = 4),
               box(width = 4,height = 120,background = 'aqua',
                   selectInput(inputId = "region_filter_P1", label = h3("Select Region"),
                               choices = c(location_update()), selected = NULL)),
               column(2)
    )
  })
  
  location_update <- reactive({
    
    
    
    
    
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    promo_display_df <- dbGetQuery(conn, paste0("select GEOGRAPHY from PROMO_DISPLAY"))
    us_lat_long_df<-maps::us.cities
    dbDisconnect(conn)
    
    #REMOVING STATES NMAES FROM THE END
    us_lat_long_df$name<-str_sub(us_lat_long_df$name,1,-4)
    
    
    #CONVERTING TO UPPER CASE PROMO DATA
    us_lat_long_df$name<-stringr::str_to_upper(us_lat_long_df$name)
    promo_display_df$GEOGRAPHY<-stringr::str_to_upper(promo_display_df$GEOGRAPHY)
    
    
    unique(promo_display_df$GEOGRAPHY)
    
    #CHANGING SOME PREFIX BEFORE JOIN
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "_"," ")
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "-"," ")
    
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "ST. ","SAINT ")
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "ST ","SAINT ")
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "FT ","FORT ")
    
    
    #REMOVING THE NORTH EAST SOUTH VALUES OF THE CITIES
    us_lat_long_df$name<-str_replace(us_lat_long_df$name, " [ESWN]$","")
    
    promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, " [ESWN]$","")
    
    
    #SELECTING ONLY USEFUL COLUMNS
    us_lat_long_df<-us_lat_long_df%>%select(name,lat,long)
    
    
    #REMOVING DUPLICATE CITES
    us_lat_long_df<-us_lat_long_df[!duplicated(us_lat_long_df$name),]
    
    
    
    #JOINING THE DATAFRAMES
    PROMO_DISPLAY_map_df=merge(x = promo_display_df, y =us_lat_long_df, by.x = "GEOGRAPHY",by.y="name")
    
    location_list <- unique(PROMO_DISPLAY_map_df$GEOGRAPHY)
    
    return(location_list)
    
    
  })
  
  CTRRangeInput<-reactive({ 
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    Email_gauge <- data.frame()
    Email_gauge <- dbGetQuery(conn, "select * from EMAIL")
    Email_gauge$DATE <- as.Date(as.numeric(Email_gauge$DATE), origin = "1899-12-30")
    Email_gauge <- Email_gauge[Email_gauge$DISPLAY_BRAND_NAME == input$Brand_P1,]
    Email_gauge_temp <- data.frame()
    Email_gauge_temp <- Email_gauge %>% mutate(month = format(DATE,"%m"), year = format(DATE, "%Y")) %>% group_by(month,year,DISPLAY_BRAND_NAME)
    Email_gauge_temp$Date<-dmy(paste("01",Email_gauge_temp$month,Email_gauge_temp$year, sep="-"))
    Email_gauge <- subset(Email_gauge_temp, select = c("DISPLAY_BRAND_NAME","EMAIL_UNIQUE_OPEN","EMAIL_COUNT","Date"))
    
    Email_gauge <- subset(Email_gauge, Date >= min(as.Date(Email_gauge$Date)) & Date <= max(as.Date(Email_gauge$Date)))
    dataset <- data.frame(Email_gauge$DISPLAY_BRAND_NAME,sum(Email_gauge$EMAIL_UNIQUE_OPEN),sum(Email_gauge$EMAIL_COUNT))
    dataset <- dataset[1,]
    return(dataset)
  })
  
  output$gauge <- renderGvis({
    
    data1 <- CTRRangeInput()
    result <- data.frame()
    
    result <- round((data1[,2]/data1[,3])*100)
    df1 <- data.frame(Label = "", Value = result)
    gvisGauge(df1,
              options=list(min=0, max=100, greenFrom=80,
                           greenTo=100, yellowFrom=35, yellowTo=79.99,
                           redFrom=0, redTo=34.99, width=150, height=500));  
    
  })  
  # output$gauge <- renderGauge({
  #   data1 <- CTRRangeInput()
  #   result <- data.frame()
  #   result <- round((data1[,2]/data1[,3])*100)
  #   gauge(result,min=0,max=100,sectors = gaugeSectors(
  #     success = c(80, 100), warning = c(40, 79), danger = c(0, 39)),symbol = "%")
  #   
  #   # gvisGauge(result,options=list(min=0, max=100, greenFrom=70,
  #   #                                  greenTo=100, yellowFrom=35, yellowTo=70,
  #   #                                  redFrom=0, redTo=35, width=400, height=300))
  # })
  
  #########################################
  # Customer Types of Email Marketing
  #########################################
  
  
  dataRangeInput<-function(brand,startdate,enddate){
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    Email_Data <- data.frame()
    Email_Data <- dbGetQuery(conn, "select * from EMAIL")
    Email_Data$DATE <- as.Date(as.numeric(Email_Data$DATE), origin = "1899-12-30")
    Email_Data_temp <-aggregate(Email_Data$EMAIL_COUNT, by=list(month = format(Email_Data$DATE,"%m"), year = format(Email_Data$DATE, "%Y"),DISPLAY_BRAND_NAME=Email_Data$DISPLAY_BRAND_NAME,CUSTOMER_TYPE_DESCRIPTION=Email_Data$CUSTOMER_TYPE_DESCRIPTION), FUN=sum)
    Email_Data_temp$Date<-dmy(paste("01",Email_Data_temp$month,Email_Data_temp$year, sep="-"))
    dbDisconnect(conn)
    names(Email_Data_temp) [5] <- "Sum"
    
    Email_Data <- subset(Email_Data_temp, select = c(DISPLAY_BRAND_NAME,CUSTOMER_TYPE_DESCRIPTION,Sum,Date))
    
    Email_Data$Date <- as.Date(Email_Data$Date)
    Email_Data <- Email_Data[Email_Data$DISPLAY_BRAND_NAME == brand,]
    if(startdate <= min(as.Date(Email_Data$Date))|| enddate <= max(as.Date(Email_Data$Date)))
    {
      Email_Data <- subset(Email_Data, Date >= min(as.Date(Email_Data$Date)) & Date <= max(as.Date(Email_Data$Date))) 
    }
    else
    {
      Email_Data <- subset(Email_Data, Date >= startdate & Date <= enddate) 
    }
    dataset <- data.frame(Email_Data$CUSTOMER_TYPE_DESCRIPTION,Email_Data$Sum)
    dataset <- aggregate(dataset$Email_Data.Sum,by=list(CUSTOMER_TYPE_DESCRIPTION=dataset$Email_Data.CUSTOMER_TYPE_DESCRIPTION),FUN=sum)
    colnames(dataset)[2] <- "Sum"
    dataset <- dataset %>% top_n(5,dataset$Sum)
    dataset$Percentage <- prop.table(dataset$Sum)*100
    dataset <- dataset[order(dataset$Percentage, decreasing = TRUE),]
    return(dataset)
  }
  
  output$barplot <-renderPlotly({
    data <- dataRangeInput(input$Brand_P1,input$DateFilter_P1[1],input$DateFilter_P1[2])
    data$CUSTOMER_TYPE_DESCRIPTION <- factor(data$CUSTOMER_TYPE_DESCRIPTION, levels = data$CUSTOMER_TYPE_DESCRIPTION[order(data$Percentage, decreasing = FALSE)])
    x <- list(
      title = "Percentage"
    )
    y <- list(
      title = "Type of Customer"
    )
    # p <- plot_ly(data,x = ~Percentage, y = ~CUSTOMER_TYPE_DESCRIPTION , type = 'bar', orientation = 'h',height = 300)%>%
    #   layout(xaxis = x, yaxis = y)
    p <- data %>% plot_ly(labels = ~CUSTOMER_TYPE_DESCRIPTION, values = ~Percentage,marker = list(
                                                                                                  line = list(color = '#FFFFFF', width = 1)), height = 300) %>%
      add_pie(hole = 0.6) %>%
      layout( autosize = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    
    
  })
  
  
  
  
  output$rank_plot <-renderPlotly({
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(type = "bar",
                y = c("Brand A", "Brand B", "Brand C", "Brand D", "Brand E"),
                x = c(39, 27.4, 20.6, 11, 2),
                textposition = "inside",
                textinfo = "value+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "lightsalmon", "tan", "teal", "silver"),
                              line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) 
    fig <- fig %>%
      layout(yaxis = list(categoryarray = c("Website visit", "Downloads", "Potential customers", "Requested price", "Finalized")))
    
    fig
    
  })

  
  output$investment_mix_plot <-renderPlotly({
    #mtcars$manuf <- sapply(strsplit(rownames(mtcars), " "), "[[", 1)
     #mtcars <- as.data.frame(mtcars)[1:5,]
    
    df <- data.frame(names = c("call" , "display", "email", "Search"), values = c(0.5,0.3,0.15,0.05))
    #df <- df %>% group_by(manuf)
    #df <- df %>% summarize(count = n())
    fig <- df %>% plot_ly(labels = ~names, values = ~values,marker = list(line = list(color = '#FFFFFF', width = 1)))
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(showlegend = T,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
    
  })
  
  
  
  
  
  
  
  mediaRangeInput<-function(brand,startdate,enddate){
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    mediaData <- data.frame()
    mediaData <- dbGetQuery(conn, "select * from PROMO_DISPLAY")
    mediaData$DATE <- as.Date(as.numeric(mediaData$DATE), origin = "1899-12-30")
    mediaData_temp <-aggregate(mediaData$ACTIVITY_COUNT, by=list(month = format(mediaData$DATE,"%m"), year = format(mediaData$DATE, "%Y"),BRAND_NAME=mediaData$BRAND_NAME,MEDIA_TYPE=mediaData$MEDIA_TYPE), FUN=sum)
    mediaData_temp$Date<-dmy(paste("01",mediaData_temp$month,mediaData_temp$year, sep="-"))
    dbDisconnect(conn)
    names(mediaData_temp) [5] <- "Sum"
    mediaData <- subset(mediaData_temp, select = c(BRAND_NAME,MEDIA_TYPE,Sum,Date))
    mediaData <- mediaData[mediaData$BRAND_NAME == brand,]
    if(startdate <= min(as.Date(mediaData$Date))|| enddate <= max(as.Date(mediaData$Date)))
    {
      mediaData <- subset(mediaData, Date >= min(as.Date(mediaData$Date)) & Date <= max(as.Date(mediaData$Date))) 
    }
    else
    {
      mediaData <- subset(mediaData, Date >= startdate & Date <= enddate) 
    } 
    dataset <- data.frame(mediaData$MEDIA_TYPE,mediaData$Sum,mediaData$Date)
    dataset <- aggregate(dataset$mediaData.Sum,by=list(MEDIA_TYPE=dataset$mediaData.MEDIA_TYPE),FUN=sum)
    colnames(dataset)[2] <- "Sum"
    dataset <- dataset %>% top_n(5,dataset$Sum)
    dataset$Percentage <- prop.table(dataset$Sum)*100
    dataset <- dataset[order(dataset$Percentage, decreasing = TRUE),]
    dataset
  }
  
  output$barplot_media <-renderPlotly({
    data <- mediaRangeInput(input$Brand_P1,input$DateFilter_P1[1],input$DateFilter_P1[2])
    data$MEDIA_TYPE <- factor(data$MEDIA_TYPE, levels = data$MEDIA_TYPE[order(data$Percentage, decreasing = FALSE)])
    x <- list(
      title = "Percentage"
    )
    y <- list(
      title = "Type of Media"
    )
    colors <- c('rgb(75,0,130)', 'rgb(255,255,0)', 'rgb(255,69,0)', 'rgb(255,20,147)', 'rgb(0,100,0)')
    # p <- plot_ly(data,x = ~Percentage, y = ~MEDIA_TYPE_NAME , type = 'bar', orientation = 'h',height = 300)%>%
    #   layout(xaxis = x, yaxis = y)
    p <- data %>% plot_ly(labels = ~MEDIA_TYPE, values = ~Percentage,height = 300,marker = list(colors = colors,
                                                                                                line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout( autosize = T,
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    
  })
  
  output$plotoutputs <- renderUI({
    tabsetPanel(id="plotpanel",
                tabPanel("Overview",value = '1',withSpinner(plotlyOutput("investmentoverview",width="98%"),type=1),div(style="text-align: center;",downloadButton("Save_investmentoverview"),actionButton(inputId = "MarketingPage_switch",label = "Get Detailed Analysis",style = "color: white; background-color:#1c05ad;
                                  padding: 10px 15px; width: 170px; cursor: pointer; font-size: 16px; font-weight: 500;"),style="display:center-align;")),
                # tabPanel("Overview of Activity",value = '2',),
                tabPanel("Detailed Investment Analysis",value = '2',withSpinner(plotlyOutput("stackplot"),type = 1),div(style="text-align: center;",actionButton(inputId = "piepreview_switch",label = "Get Share wise details",style = "color: white; background-color:#1c05ad;
                                  padding: 10px 15px; width: 170px; cursor: pointer; font-size: 16px; font-weight: 500;"),shinyBS::bsModal("modalExample", 
                                                                                                                                  "Channel wise Investment share", 
                                                                                                                                  trigger = "piepreview_switch", # <----set the observer to the right button
                                                                                                                                  size = "large",
                                                                                                                                  withSpinner(plotlyOutput("plot_pie1"),type=1)),style="display:center-align;")),
                tabPanel("Detailed Activity Count",value='3',withSpinner(plotlyOutput("stackactivity_plot"),type = 1),div(style="text-align: center;",actionButton(inputId = "piepreview_activity_switch",label = "Get Share wise details",style = "color: white; background-color:#1c05ad;
                                  padding: 10px 15px; width: 170px; cursor: pointer; font-size: 16px; font-weight: 500;"),shinyBS::bsModal("modalExample1", 
                                                                                                                                           "Channel wise Activity share", 
                                                                                                                                           trigger = "piepreview_activity_switch", # <----set the observer to the right button
                                                                                                                                           size = "large",
                                                                                                                                           withSpinner(plotlyOutput("plot_pie2"),type=1)),style="display:center-align;"))
    )
    
  })
  
  
  output$Save_investmentoverview <- downloadHandler(
    filename = function(){
      if(input$Brand_P1=="MAVYRET")
        brand_name <- "Brand A"
      else if(input$Brand_P1=="HUMIRA")
        brand_name <- "Brand B"
      else if(input$Brand_P1=="LUPRON DEPOT")
        brand_name <- "Brand C"
      else if(input$Brand_P1=="CREON")
        brand_name <- "Brand D"
      paste0(brand_name,"_investmentVSactivity.csv")
    },
    content = function(file){
      path <- paste0("www/investmentVSactivity.csv")
      file.copy(path,file)
    }
  )
output$plot_pie1 <- renderPlotly({
  
  conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
  spendamount <- data.frame()
  spendamount <- dbGetQuery(conn, "select * from PROMO_DISPLAY")
  spendamount$DATE <- as.Date(as.numeric(spendamount$DATE), origin = "1899-12-30")
  spendamount <- spendamount[spendamount$BRAND_NAME == input$Brand_P1,]
  
  if(input$DateFilter_P1[1] <= min(as.Date(spendamount$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount$DATE)))
  {
    spendamount <- subset(spendamount, DATE >= min(as.Date(spendamount$DATE)) & DATE <= max(as.Date(spendamount$DATE))) 
  }
  else
  {
    spendamount <- subset(spendamount, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2]) 
  }
  spendamount <- subset(spendamount,select = c(DATE,SPEND_AMT))
  spendamount <- na.omit(spendamount)
  spendamount_temp <-spendamount%>%dplyr::arrange(DATE)
  
  
  DF2<-spendamount_temp %>%
    mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
    group_by(month, year) %>%
    summarise(total = sum(SPEND_AMT))
  
  
  DF2$DATE<-dmy(paste("01",DF2$month,DF2$year, sep="-"))
  
  output_data<-DF2 %>%
    group_by(DATE) %>%
    summarise(Total = sum(total))
  
  
  spendamount1 <- dbGetQuery(conn, "select * from PROMO_SEARCH")
  spendamount1$DATE <- as.Date(as.numeric(spendamount1$DATE), origin = "1899-12-30")
  spendamount1 <- spendamount1[spendamount1$BRAND == input$Brand_P1,]
  
  if(input$DateFilter_P1[1] <= min(as.Date(spendamount1$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount1$DATE)))
  {
    spendamount1 <- subset(spendamount1, DATE >= min(as.Date(spendamount1$DATE)) & DATE <= max(as.Date(spendamount1$DATE))) 
  }
  else
  {
    spendamount1 <- subset(spendamount1, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2]) 
  }
  spendamount1 <- subset(spendamount1,select = c(DATE,SPEND_AMT))
  spendamount1 <- na.omit(spendamount1)
  spendamount_temp1 <-spendamount1%>%dplyr::arrange(DATE)
  
  
  DF3<-spendamount_temp1 %>%
    mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
    group_by(month, year) %>%
    summarise(total = sum(SPEND_AMT))
  
  
  DF3$DATE<-dmy(paste("01",DF3$month,DF3$year, sep="-"))
  
  output_data1<-DF3 %>%
    group_by(DATE) %>%
    summarise(Total = sum(total))
  
  
  spendamount2 <- dbGetQuery(conn, "select * from EMAIL")
  spendamount2$DATE <- as.Date(as.numeric(spendamount2$DATE), origin = "1899-12-30")
  spendamount2 <- spendamount2[spendamount2$DISPLAY_BRAND_NAME == input$Brand_P1,]
  
  if(input$DateFilter_P1[1] <= min(as.Date(spendamount2$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount2$DATE)))
  {
    spendamount2 <- subset(spendamount2, DATE >= min(as.Date(spendamount2$DATE)) & DATE <= max(as.Date(spendamount2$DATE))) 
  }
  else
  {
    spendamount2 <- subset(spendamount2, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2]) 
  }
  spendamount2 <- subset(spendamount2,select = c(DATE,EMAIL_COUNT))
  spendamount2 <- na.omit(spendamount2)
  spendamount_temp2 <-spendamount2%>%dplyr::arrange(DATE)
  
  
  DF4<-spendamount_temp2 %>%
    mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
    group_by(month, year) %>%
    summarise(total = sum(EMAIL_COUNT))
  
  
  DF4$DATE<-dmy(paste("01",DF4$month,DF4$year, sep="-"))
  
  output_data2<-DF4 %>%
    group_by(DATE) %>%
    summarise(Total = sum(total))
  
  output_data2$Total <- output_data2$Total*0.123
  
  spendamount3 <- dbGetQuery(conn, "select * from CALL_ACTIVITY")
  spendamount3$DATE <- as.Date(as.numeric(spendamount3$DATE), origin = "1899-12-30")
  spendamount3$SOURCE_BRAND_NAME <- toupper(spendamount3$SOURCE_BRAND_NAME)
  spendamount3 <- spendamount3[spendamount3$SOURCE_BRAND_NAME == input$Brand_P1,]
  
  
  if(input$DateFilter_P1[1] <= min(as.Date(spendamount3$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount3$DATE)))
  {
    spendamount3 <- subset(spendamount3, DATE >= min(as.Date(spendamount3$DATE)) & DATE <= max(as.Date(spendamount3$DATE)))
    
  }
  else
  {
    spendamount3 <- subset(spendamount3, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2])
  }
  spendamount3<- subset(spendamount3,select = c(DATE,CALL_DUR))
  spendamount3 <- na.omit(spendamount3)
  spendamount_temp3 <-spendamount3%>%dplyr::arrange(DATE)
  
  dbDisconnect(conn)
  
  DF5<-spendamount_temp3 %>%
    mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
    group_by(month, year) %>%
    summarise(total = sum(CALL_DUR))
  
  
  DF5$DATE<-dmy(paste("01",DF5$month,DF5$year, sep="-"))
  
  output_data3<-DF5 %>%
    group_by(DATE) %>%
    summarise(Total = sum(total))
  
  output_data3$Total <- output_data3$Total*0.0608
  
  Total_investment <- sum(sum(output_data$Total),sum(output_data1$Total),sum(output_data2$Total),sum(output_data3$Total))
  investment_channels_spend <- data.frame(Total=c(sum(output_data$Total),sum(output_data1$Total),sum(output_data2$Total),sum(output_data3$Total)))
  investment_channels_spend <- investment_channels_spend/Total_investment*100
  
  col_names <- c("Display","Search","Email","Call")
  investment_channels_spend <- cbind(col_names,investment_channels_spend)
  colnames(investment_channels_spend) <- c("name","value")
  names(investment_channels_spend)[1] <- "name"
  names(investment_channels_spend)[2] <- "value"
  
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  fig <- plot_ly(investment_channels_spend, labels = ~name, values = ~value, type = 'pie',
                 marker = list(colors = colors))
  fig <- fig %>% layout(autosize = T,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
  
})
  
  
  
  output$plot_pie2 <- renderPlotly({
  conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
  activitycount <- data.frame()
  activitycount <- dbGetQuery(conn, "select * from PROMO_DISPLAY")
  activitycount$DATE <- as.Date(as.numeric(activitycount$DATE), origin = "1899-12-30")
  activitycount <- activitycount[activitycount$BRAND_NAME == input$Brand_P1,]
  if(input$DateFilter_P1[1] <= min(as.Date(activitycount$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount$DATE)))
  {
    activitycount <- subset(activitycount, activitycount$DATE >= min(as.Date(activitycount$DATE)) & activitycount$DATE <= max(as.Date(activitycount$DATE)))
  }
  else
  {
    activitycount <- subset(activitycount, activitycount$DATE >= input$DateFilter_P1[1] & activitycount$DATE <= input$DateFilter_P1[2])
  }
  activitycount <- subset(activitycount,select = c(DATE,ACTIVITY_COUNT))
  
  activitycount <- na.omit(activitycount)
  activitycount_temp1 <-activitycount%>%dplyr::arrange(DATE)
  
  
  DF1<-activitycount_temp1 %>%
    mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
    group_by(month, year) %>%
    summarise(total = sum(ACTIVITY_COUNT))
  
  
  DF1$DATE<-dmy(paste("01",DF1$month,DF1$year, sep="-"))
  
  output_data1<-DF1 %>%
    group_by(DATE) %>%
    summarise(Total = sum(total))
  
  activitycount1 <- dbGetQuery(conn, "select * from PROMO_SEARCH")
  activitycount1$DATE <- as.Date(as.numeric(activitycount1$DATE), origin = "1899-12-30")
  activitycount1 <- activitycount1[activitycount1$BRAND == input$Brand_P1,]
  if(input$DateFilter_P1[1] <= min(as.Date(activitycount1$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount1$DATE)))
  {
    activitycount1 <- subset(activitycount1, activitycount1$DATE >= min(as.Date(activitycount1$DATE)) & activitycount1$DATE <= max(as.Date(activitycount1$DATE)))
  }
  else
  {
    activitycount1 <- subset(activitycount1, activitycount1$DATE >= input$DateFilter_P1[1] & activitycount1$DATE <= input$DateFilter_P1[2])
  }
  activitycount1 <- subset(activitycount1,select = c(DATE,ACTIVITY_COUNT))
  
  activitycount1 <- na.omit(activitycount1)
  activitycount_temp2 <-activitycount1%>%dplyr::arrange(DATE)
  
  
  DF2<-activitycount_temp2 %>%
    mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
    group_by(month, year) %>%
    summarise(total = sum(ACTIVITY_COUNT))
  
  
  DF2$DATE<-dmy(paste("01",DF2$month,DF2$year, sep="-"))
  
  output_data2<-DF2 %>%
    group_by(DATE) %>%
    summarise(Total = sum(total))
  
  activitycount2 <- dbGetQuery(conn, "select * from EMAIL")
  activitycount2$DATE <- as.Date(as.numeric(activitycount2$DATE), origin = "1899-12-30")
  activitycount2 <- activitycount2[activitycount2$DISPLAY_BRAND_NAME == input$Brand_P1,]    
  if(input$DateFilter_P1[1] <= min(as.Date(activitycount2$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount2$DATE)))
  {
    activitycount2 <- subset(activitycount2, activitycount2$DATE >= min(as.Date(activitycount2$DATE)) & activitycount2$DATE <= max(as.Date(activitycount2$DATE)))
  }
  else
  {
    activitycount2 <- subset(activitycount2, activitycount2$DATE >= input$DateFilter_P1[1] & activitycount2$DATE <= input$DateFilter_P1[2])
  }
  activitycount2 <- subset(activitycount2,select = c(DATE,EMAIL_COUNT))
  activitycount2 <- na.omit(activitycount2)
  activitycount_temp3 <-activitycount2%>%dplyr::arrange(DATE)
  
  
  DF4<-activitycount_temp3 %>%
    mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
    group_by(month, year) %>%
    summarise(total = sum(EMAIL_COUNT))
  
  
  DF4$DATE<-dmy(paste("01",DF4$month,DF4$year, sep="-"))
  
  output_data3<-DF4 %>%
    group_by(DATE) %>%
    summarise(Total = sum(total))
  
  activitycount3 <- dbGetQuery(conn, "select * from CALL_ACTIVITY")
  activitycount3$DATE <- as.Date(as.numeric(activitycount3$DATE), origin = "1899-12-30")
  activitycount3$SOURCE_BRAND_NAME <- toupper(activitycount3$SOURCE_BRAND_NAME)
  activitycount3 <- activitycount3[activitycount3$SOURCE_BRAND_NAME == input$Brand_P1,]    
  if(input$DateFilter_P1[1] <= min(as.Date(activitycount3$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount3$DATE)))
  {
    activitycount3 <- subset(activitycount3, activitycount3$DATE >= min(as.Date(activitycount3$DATE)) & activitycount3$DATE <= max(as.Date(activitycount3$DATE)))
  }
  else
  {
    activitycount3 <- subset(activitycount3, activitycount3$DATE >= input$DateFilter_P1[1] & activitycount3$DATE <= input$DateFilter_P1[2])
  }
  activitycount3<- subset(activitycount3,select = c(DATE,CALL_DUR))
  activitycount3 <- na.omit(activitycount3)
  activitycount_temp4 <-activitycount3%>%dplyr::arrange(DATE)
  dbDisconnect(conn)
  
  DF5<-activitycount_temp4 %>%
    mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
    group_by(month, year) %>%
    summarise(total = sum(CALL_DUR))
  
  
  DF5$DATE<-dmy(paste("01",DF5$month,DF5$year, sep="-"))
  
  output_data4<-DF5 %>%
    group_by(DATE) %>%
    summarise(Total = sum(total))
  
  Total_investment <- sum(sum(output_data1$Total),sum(output_data2$Total),sum(output_data3$Total),sum(output_data4$Total))
  investment_channels_spend <- data.frame(Total=c(sum(output_data1$Total),sum(output_data2$Total),sum(output_data3$Total),sum(output_data4$Total)))
  investment_channels_spend <- investment_channels_spend/Total_investment*100
  
  col_names <- c("Display","Search","Email","Call")
  investment_channels_spend <- cbind(col_names,investment_channels_spend)
  colnames(investment_channels_spend) <- c("name","value")
  names(investment_channels_spend)[1] <- "name"
  names(investment_channels_spend)[2] <- "value"
  
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  fig <- plot_ly(investment_channels_spend, labels = ~name, values = ~value, type = 'pie',
                 marker = list(colors = colors))
  fig <- fig %>% layout(autosize = T,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
  })
  
  output$stackplot <- renderPlotly({
    
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    spendamount <- data.frame()
    spendamount <- dbGetQuery(conn, "select * from PROMO_DISPLAY")
    spendamount$DATE <- as.Date(as.numeric(spendamount$DATE), origin = "1899-12-30")
    spendamount <- spendamount[spendamount$BRAND_NAME == input$Brand_P1,]
    
    if(input$DateFilter_P1[1] <= min(as.Date(spendamount$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount$DATE)))
    {
      spendamount <- subset(spendamount, DATE >= min(as.Date(spendamount$DATE)) & DATE <= max(as.Date(spendamount$DATE))) 
    }
    else
    {
      spendamount <- subset(spendamount, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2]) 
    }
    spendamount <- subset(spendamount,select = c(DATE,SPEND_AMT))
    spendamount <- na.omit(spendamount)
    spendamount_temp <-spendamount%>%dplyr::arrange(DATE)
    
    
    DF2<-spendamount_temp %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(SPEND_AMT))
    
    
    DF2$DATE<-dmy(paste("01",DF2$month,DF2$year, sep="-"))
    
    output_data<-DF2 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    
    spendamount1 <- dbGetQuery(conn, "select * from PROMO_SEARCH")
    spendamount1$DATE <- as.Date(as.numeric(spendamount1$DATE), origin = "1899-12-30")
    spendamount1 <- spendamount1[spendamount1$BRAND == input$Brand_P1,]
    
    if(input$DateFilter_P1[1] <= min(as.Date(spendamount1$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount1$DATE)))
    {
      spendamount1 <- subset(spendamount1, DATE >= min(as.Date(spendamount1$DATE)) & DATE <= max(as.Date(spendamount1$DATE))) 
    }
    else
    {
      spendamount1 <- subset(spendamount1, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2]) 
    }
    spendamount1 <- subset(spendamount1,select = c(DATE,SPEND_AMT))
    spendamount1 <- na.omit(spendamount1)
    spendamount_temp1 <-spendamount1%>%dplyr::arrange(DATE)
    
    
    DF3<-spendamount_temp1 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(SPEND_AMT))
    
    
    DF3$DATE<-dmy(paste("01",DF3$month,DF3$year, sep="-"))
    
    output_data1<-DF3 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    
    spendamount2 <- dbGetQuery(conn, "select * from EMAIL")
    spendamount2$DATE <- as.Date(as.numeric(spendamount2$DATE), origin = "1899-12-30")
    spendamount2 <- spendamount2[spendamount2$DISPLAY_BRAND_NAME == input$Brand_P1,]
    
    if(input$DateFilter_P1[1] <= min(as.Date(spendamount2$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount2$DATE)))
    {
      spendamount2 <- subset(spendamount2, DATE >= min(as.Date(spendamount2$DATE)) & DATE <= max(as.Date(spendamount2$DATE))) 
    }
    else
    {
      spendamount2 <- subset(spendamount2, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2]) 
    }
    spendamount2 <- subset(spendamount2,select = c(DATE,EMAIL_COUNT))
    spendamount2 <- na.omit(spendamount2)
    spendamount_temp2 <-spendamount2%>%dplyr::arrange(DATE)
    
    
    DF4<-spendamount_temp2 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(EMAIL_COUNT))
    
    
    DF4$DATE<-dmy(paste("01",DF4$month,DF4$year, sep="-"))
    
    output_data2<-DF4 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    output_data2$Total <- output_data2$Total*0.123
    
    spendamount3 <- dbGetQuery(conn, "select * from CALL_ACTIVITY")
    spendamount3$DATE <- as.Date(as.numeric(spendamount3$DATE), origin = "1899-12-30")
    spendamount3$SOURCE_BRAND_NAME <- toupper(spendamount3$SOURCE_BRAND_NAME)
    spendamount3 <- spendamount3[spendamount3$SOURCE_BRAND_NAME == input$Brand_P1,]
    
    
    if(input$DateFilter_P1[1] <= min(as.Date(spendamount3$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount3$DATE)))
    {
      spendamount3 <- subset(spendamount3, DATE >= min(as.Date(spendamount3$DATE)) & DATE <= max(as.Date(spendamount3$DATE)))
      
    }
    else
    {
      spendamount3 <- subset(spendamount3, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2])
    }
    spendamount3<- subset(spendamount3,select = c(DATE,CALL_DUR))
    spendamount3 <- na.omit(spendamount3)
    spendamount_temp3 <-spendamount3%>%dplyr::arrange(DATE)
    
    dbDisconnect(conn)
    
    DF5<-spendamount_temp3 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(CALL_DUR))
    
    
    DF5$DATE<-dmy(paste("01",DF5$month,DF5$year, sep="-"))
    
    output_data3<-DF5 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    output_data3$Total <- output_data3$Total*0.0608
    a <- list(
      title = "Display"
    )
    
    b <- list(
      title = "Search"
    )
    
    c <- list(
      title = "Email"
    )
    
    d<- list(
      title = "Call"
    )
    
    
    p1 <- plot_ly(output_data, x = ~DATE, y = ~Total, type = 'scatter', name = 'Display' , mode = 'lines+markers')%>%
      layout(height = 400,showgrid = FALSE,yaxis = a)
    p2 <- plot_ly(output_data1, x = ~DATE, y = ~Total, type = 'scatter', name = 'Search' , mode = 'lines+markers')%>%
      layout(height = 400,showgrid = FALSE,yaxis = b)
    p3 <- plot_ly(output_data2, x = ~DATE, y = ~Total, type = 'scatter', name = 'Email' , mode = 'lines+markers')%>%
      layout(height = 400,showgrid = FALSE,yaxis = c)
    p4 <- plot_ly(output_data3, x = ~DATE, y = ~Total, type = 'scatter', name = 'Call' , mode = 'lines+markers')%>%
      layout(height = 400,showgrid = FALSE,yaxis = d)
    
    subplot(p1, p2 , p3 , p4,nrows = 2,margin = 0.06, titleY = TRUE)
  })
  
  stack_activitycount <- function(){
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    activitycount <- data.frame()
    activitycount <- dbGetQuery(conn, "select * from PROMO_DISPLAY")
    activitycount$DATE <- as.Date(as.numeric(activitycount$DATE), origin = "1899-12-30")
    dbDisconnect(conn)
    activitycount <- activitycount[activitycount$BRAND_NAME == input$Brand_P1,]
    # updateSliderInput(session,"DateFilter_P1",min = min(as.Date(activitycount$DATE)),
    #                   max = max(as.Date(activitycount$DATE)),
    #                   value=c(min(as.Date(activitycount$DATE)),max(as.Date(activitycount$DATE))),timeFormat="%b %Y")
    activitycount <- subset(activitycount, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2])
    activitycount <- subset(activitycount,select = c(DATE,ACTIVITY_COUNT))
    Brand <- activitycount[1,1]
    sum1 <- sum(activitycount[, 'ACTIVITY_COUNT'], na.rm = TRUE)
    activitycount1 <- dbGetQuery(conn, "select * from PROMO_SEARCH")
    activitycount1$DATE <- as.Date(as.numeric(activitycount1$DATE), origin = "1899-12-30")
    activitycount1 <- activitycount1[activitycount1$BRAND == input$Brand_P1,]
    
    activitycount1 <- subset(activitycount1, DATE >= input$DateFilter_P1[1] & DATE <= input$DateFilter_P1[2])
    activitycount1 <- subset(activitycount1,select = c(DATE,ACTIVITY_COUNT))
    plotdata <- data.frame(Brand,sum1,sum2)
    
    plotdata
    
  }
  
  output$stackactivity_plot <- renderPlotly({
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    activitycount <- data.frame()
    activitycount <- dbGetQuery(conn, "select * from PROMO_DISPLAY")
    activitycount$DATE <- as.Date(as.numeric(activitycount$DATE), origin = "1899-12-30")
    activitycount <- activitycount[activitycount$BRAND_NAME == input$Brand_P1,]
    if(input$DateFilter_P1[1] <= min(as.Date(activitycount$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount$DATE)))
    {
      activitycount <- subset(activitycount, activitycount$DATE >= min(as.Date(activitycount$DATE)) & activitycount$DATE <= max(as.Date(activitycount$DATE)))
    }
    else
    {
      activitycount <- subset(activitycount, activitycount$DATE >= input$DateFilter_P1[1] & activitycount$DATE <= input$DateFilter_P1[2])
    }
    activitycount <- subset(activitycount,select = c(DATE,ACTIVITY_COUNT))
    
    activitycount <- na.omit(activitycount)
    activitycount_temp1 <-activitycount%>%dplyr::arrange(DATE)
    
    
    DF1<-activitycount_temp1 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(ACTIVITY_COUNT))
    
    
    DF1$DATE<-dmy(paste("01",DF1$month,DF1$year, sep="-"))
    
    output_data1<-DF1 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    activitycount1 <- dbGetQuery(conn, "select * from PROMO_SEARCH")
    activitycount1$DATE <- as.Date(as.numeric(activitycount1$DATE), origin = "1899-12-30")
    activitycount1 <- activitycount1[activitycount1$BRAND == input$Brand_P1,]
    if(input$DateFilter_P1[1] <= min(as.Date(activitycount1$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount1$DATE)))
    {
      activitycount1 <- subset(activitycount1, activitycount1$DATE >= min(as.Date(activitycount1$DATE)) & activitycount1$DATE <= max(as.Date(activitycount1$DATE)))
    }
    else
    {
      activitycount1 <- subset(activitycount1, activitycount1$DATE >= input$DateFilter_P1[1] & activitycount1$DATE <= input$DateFilter_P1[2])
    }
    activitycount1 <- subset(activitycount1,select = c(DATE,ACTIVITY_COUNT))
    
    activitycount1 <- na.omit(activitycount1)
    activitycount_temp2 <-activitycount1%>%dplyr::arrange(DATE)
    
    
    DF2<-activitycount_temp2 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(ACTIVITY_COUNT))
    
    
    DF2$DATE<-dmy(paste("01",DF2$month,DF2$year, sep="-"))
    
    output_data2<-DF2 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    activitycount2 <- dbGetQuery(conn, "select * from EMAIL")
    activitycount2$DATE <- as.Date(as.numeric(activitycount2$DATE), origin = "1899-12-30")
    activitycount2 <- activitycount2[activitycount2$DISPLAY_BRAND_NAME == input$Brand_P1,]    
    if(input$DateFilter_P1[1] <= min(as.Date(activitycount2$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount2$DATE)))
    {
      activitycount2 <- subset(activitycount2, activitycount2$DATE >= min(as.Date(activitycount2$DATE)) & activitycount2$DATE <= max(as.Date(activitycount2$DATE)))
    }
    else
    {
      activitycount2 <- subset(activitycount2, activitycount2$DATE >= input$DateFilter_P1[1] & activitycount2$DATE <= input$DateFilter_P1[2])
    }
    activitycount2 <- subset(activitycount2,select = c(DATE,EMAIL_COUNT))
    activitycount2 <- na.omit(activitycount2)
    activitycount_temp3 <-activitycount2%>%dplyr::arrange(DATE)
    
    
    DF4<-activitycount_temp3 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(EMAIL_COUNT))
    
    
    DF4$DATE<-dmy(paste("01",DF4$month,DF4$year, sep="-"))
    
    output_data3<-DF4 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    activitycount3 <- dbGetQuery(conn, "select * from CALL_ACTIVITY")
    activitycount3$DATE <- as.Date(as.numeric(activitycount3$DATE), origin = "1899-12-30")
    activitycount3$SOURCE_BRAND_NAME <- toupper(activitycount3$SOURCE_BRAND_NAME)
    activitycount3 <- activitycount3[activitycount3$SOURCE_BRAND_NAME == input$Brand_P1,]    
    if(input$DateFilter_P1[1] <= min(as.Date(activitycount3$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount3$DATE)))
    {
      activitycount3 <- subset(activitycount3, activitycount3$DATE >= min(as.Date(activitycount3$DATE)) & activitycount3$DATE <= max(as.Date(activitycount3$DATE)))
    }
    else
    {
      activitycount3 <- subset(activitycount3, activitycount3$DATE >= input$DateFilter_P1[1] & activitycount3$DATE <= input$DateFilter_P1[2])
    }
    activitycount3<- subset(activitycount3,select = c(DATE,CALL_DUR))
    activitycount3 <- na.omit(activitycount3)
    activitycount_temp4 <-activitycount3%>%dplyr::arrange(DATE)
    dbDisconnect(conn)
    
    DF5<-activitycount_temp4 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(CALL_DUR))
    
    
    DF5$DATE<-dmy(paste("01",DF5$month,DF5$year, sep="-"))
    
    output_data4<-DF5 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    a <- list(
      title = "Display"
    )
    
    b <- list(
      title = "Search"
    )
    
    c <- list(
      title = "Email"
    )
    
    d<- list(
      title = "Call"
    )
    p1 <- plot_ly(output_data1, x = ~DATE, y = ~Total, type = 'scatter', name = 'Display' , mode = 'lines+markers')%>%
      layout(height = 400,showgrid = FALSE,yaxis = a)
    p2 <- plot_ly(output_data2, x = ~DATE, y = ~Total, type = 'scatter', name = 'Search' , mode = 'lines+markers')%>%
      layout(height = 400,showgrid = FALSE,yaxis = b)
    p3 <- plot_ly(output_data3, x = ~DATE, y = ~Total, type = 'scatter', name = 'Email' , mode = 'lines+markers')%>%
      layout(height = 400,showgrid = FALSE,yaxis = c)
    p4 <- plot_ly(output_data4, x = ~DATE, y = ~Total, type = 'scatter', name = 'Call' , mode = 'lines+markers')%>%
      layout(height = 400,showgrid = FALSE,yaxis = d)
    
    subplot(p1, p2,p3,p4 ,nrows = 2, margin = 0.06,titleY = TRUE)
    
    # p <- plot_ly(output_data, x = ~Brand, y = ~sum1, type = 'bar', name = 'Promo Display') %>%
    #   add_trace(y = ~sum2, name = 'Promo Search') %>%
    #   layout(yaxis = list(title = 'Sum'), barmode = 'stack',height = 325 ,width =200)
  })
  
  scatter_callactivity <- function(){
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    call_activity <- data.frame()
    call_activity <- dbGetQuery(conn, "select * from CALL_ACTIVITY")
    call_activity$DATE <- as.Date(as.numeric(call_activity$DATE), origin = "1899-12-30")
    call_activity$SOURCE_BRAND_NAME <- toupper(call_activity$SOURCE_BRAND_NAME)
    call_activity <- call_activity[call_activity$SOURCE_BRAND_NAME == input$Brand_P1,]
    if(input$DateFilter_P1[1] <= min(as.Date(call_activity$DATE))|| input$DateFilter_P1[2] <= max(as.Date(call_activity$DATE)))
    {
      call_activity <- subset(call_activity, call_activity$DATE >= min(as.Date(call_activity$DATE)) & call_activity$DATE <= max(as.Date(call_activity$DATE)))
    }
    else
    {
      call_activity <- subset(call_activity, call_activity$DATE >= input$DateFilter_P1[1] & call_activity$DATE <= input$DateFilter_P1[2])
    }
    call_activity <- subset(call_activity,select = c(CALL_DUR,DATE))
    
    dbDisconnect(conn)
    call_activity
    
  }
  
  output$callactivityplot <- renderPlotly({
    
    
    output_data <- scatter_callactivity()
    
    output_data_temp <-output_data%>%dplyr::arrange(DATE)
    
    DF2<-output_data_temp %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(CALL_DUR))
    
    
    DF2$DATE<-dmy(paste("01",DF2$month,DF2$year, sep="-"))
    
    output_data<-DF2 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    
    plot_ly(output_data, x = ~DATE, 
            y = ~Total, 
            name = 'trace 0', 
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'rgb(205, 12, 24)'))%>%
      plotly::layout( height = 325)
  })
  
  investment_overview <- reactive({
    
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    spendamount <- data.frame()
    spendamount <- dbGetQuery(conn, "select * from PROMO_DISPLAY")
    spendamount$DATE <- as.Date(as.numeric(spendamount$DATE), origin = "1899-12-30")
    spendamount <- spendamount[spendamount$BRAND_NAME == input$Brand_P1,] 
    if(input$DateFilter_P1[1] <= min(as.Date(spendamount$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount$DATE)))
    {
      spendamount <- subset(spendamount, spendamount$DATE >= min(as.Date(spendamount$DATE)) & spendamount$DATE <= max(as.Date(spendamount$DATE)))
    }
    else
    {
      spendamount <- subset(spendamount, spendamount$DATE >= input$DateFilter_P1[1] & spendamount$DATE <= input$DateFilter_P1[2])
    }
    spendamount <- subset(spendamount,select = c(DATE,SPEND_AMT))
    spendamount <- na.omit(spendamount)
    spendamount_temp <-spendamount%>%dplyr::arrange(DATE)
    
    
    DF2<-spendamount_temp %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(SPEND_AMT))
    
    
    DF2$DATE<-dmy(paste("01",DF2$month,DF2$year, sep="-"))
    
    output_data<-DF2 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    
    spendamount1 <- dbGetQuery(conn, "select * from PROMO_SEARCH")
    spendamount1$DATE <- as.Date(as.numeric(spendamount1$DATE), origin = "1899-12-30")
    spendamount1 <- spendamount1[spendamount1$BRAND == input$Brand_P1,] 
    if(input$DateFilter_P1[1] <= min(as.Date(spendamount1$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount1$DATE)))
    {
      spendamount1 <- subset(spendamount1, spendamount1$DATE >= min(as.Date(spendamount1$DATE)) & spendamount1$DATE <= max(as.Date(spendamount1$DATE)))
    }
    else
    {
      spendamount1 <- subset(spendamount1, spendamount1$DATE >= input$DateFilter_P1[1] & spendamount1$DATE <= input$DateFilter_P1[2])
    }
    spendamount1 <- subset(spendamount1,select = c(DATE,SPEND_AMT))
    spendamount1 <- na.omit(spendamount1)
    spendamount_temp1 <-spendamount1%>%dplyr::arrange(DATE)
    
    
    DF3<-spendamount_temp1 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(SPEND_AMT))
    
    
    DF3$DATE<-dmy(paste("01",DF3$month,DF3$year, sep="-"))
    
    output_data1<-DF3 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    
    spendamount2 <- dbGetQuery(conn, "select * from EMAIL")
    spendamount2$DATE <- as.Date(as.numeric(spendamount2$DATE), origin = "1899-12-30")
    spendamount2 <- spendamount2[spendamount2$DISPLAY_BRAND_NAME == input$Brand_P1,] 
    if(input$DateFilter_P1[1] <= min(as.Date(spendamount2$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount2$DATE)))
    {
      spendamount2 <- subset(spendamount2, spendamount2$DATE >= min(as.Date(spendamount2$DATE)) & spendamount2$DATE <= max(as.Date(spendamount2$DATE)))
    }
    else
    {
      spendamount2 <- subset(spendamount2, spendamount2$DATE >= input$DateFilter_P1[1] & spendamount2$DATE <= input$DateFilter_P1[2])
    }
    spendamount2 <- subset(spendamount2,select = c(DATE,EMAIL_COUNT))
    spendamount2 <- na.omit(spendamount2)
    spendamount_temp2 <-spendamount2%>%dplyr::arrange(DATE)
    
    
    DF4<-spendamount_temp2 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(EMAIL_COUNT))
    
    
    DF4$DATE<-dmy(paste("01",DF4$month,DF4$year, sep="-"))
    
    output_data2<-DF4 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    output_data2$Total <- output_data2$Total*0.123
    
    spendamount3 <- dbGetQuery(conn, "select * from CALL_ACTIVITY")
    spendamount3$DATE <- as.Date(as.numeric(spendamount3$DATE), origin = "1899-12-30")
    spendamount3$SOURCE_BRAND_NAME <- toupper(spendamount3$SOURCE_BRAND_NAME)
    spendamount3 <- spendamount3[spendamount3$SOURCE_BRAND_NAME == input$Brand_P1,] 
    if(input$DateFilter_P1[1] <= min(as.Date(spendamount3$DATE))|| input$DateFilter_P1[2] <= max(as.Date(spendamount3$DATE)))
    {
      spendamount3 <- subset(spendamount3, spendamount3$DATE >= min(as.Date(spendamount3$DATE)) & spendamount3$DATE <= max(as.Date(spendamount3$DATE)))
    }
    else
    {
      spendamount3 <- subset(spendamount3, spendamount3$DATE >= input$DateFilter_P1[1] & spendamount3$DATE <= input$DateFilter_P1[2])
    }
    spendamount3<- subset(spendamount3,select = c(DATE,CALL_DUR))
    spendamount3 <- na.omit(spendamount3)
    spendamount_temp3 <-spendamount3%>%dplyr::arrange(DATE)
    dbDisconnect(conn)
    
    DF5<-spendamount_temp3 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(CALL_DUR))
    
    
    DF5$DATE<-dmy(paste("01",DF5$month,DF5$year, sep="-"))
    
    output_data3<-DF5 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    output_data3$Total <- output_data3$Total*0.0608
    
    result <- rbind(output_data,output_data1)
    
    result2<- rbind(output_data2,output_data3)
    
    result3 <- rbind(result,result2,fill = NA)
    
    data<-result3 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(Total))
    
    
    data$DATE<-dmy(paste("01",data$month,data$year, sep="-"))
    
    result_data<-data %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    result_data
  })
  
  output$investmentoverview <- renderPlotly({
    
    result_data <- investment_overview()
    
    result_data1 <- activity_overview()
    
    result_data_temp <- result_data
    
    result_data1_temp <- result_data1
    
    colnames(result_data_temp) <- c("Date","Total")
    colnames(result_data1_temp) <- c("Date","Total")
    
    
    final_data <- cbind(result_data_temp,result_data1_temp[,2])
    colnames(final_data) <- c("Date","Total Investment","Total Activity count")
    write.csv(final_data,"www/investmentVSactivity.csv",row.names = F)
    
    ay2 <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "Total Activity Count"
    )
    
    fig <- plot_ly()
    fig <- fig %>% add_lines(x = result_data$DATE, y = result_data$Total, name = "Total Investment",title = "Total Investment")
    fig <- fig %>% add_lines(x = result_data1$DATE, y = result_data1$Total, name = "Total Activity Count", yaxis = "y2")
    fig <- fig %>% layout(
      title = "Overview of Investment and Activity", yaxis2 = ay2,
      xaxis = list(title="DATE")
    )
    
    # plot_ly(result_data, x = ~DATE, 
    #         y = ~Total, 
    #         name = 'trace 0', 
    #         type = 'scatter', 
    #         mode = 'lines')%>%
    #   plotly::layout(title = "Overview of Investment", height = 400)
    
  })
  
  activity_overview <- reactive({
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    activitycount <- data.frame()
    activitycount <- dbGetQuery(conn, "select * from PROMO_DISPLAY")
    activitycount$DATE <- as.Date(as.numeric(activitycount$DATE), origin = "1899-12-30")
    activitycount <- activitycount[activitycount$BRAND_NAME == input$Brand_P1,]
    if(input$DateFilter_P1[1] <= min(as.Date(activitycount$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount$DATE)))
    {
      activitycount <- subset(activitycount, activitycount$DATE >= min(as.Date(activitycount$DATE)) & activitycount$DATE <= max(as.Date(activitycount$DATE)))
    }
    else
    {
      activitycount <- subset(activitycount, activitycount$DATE >= input$DateFilter_P1[1] & activitycount$DATE <= input$DateFilter_P1[2])
    }
    activitycount <- subset(activitycount,select = c(DATE,ACTIVITY_COUNT))
    
    activitycount <- na.omit(activitycount)
    activitycount_temp1 <-activitycount%>%dplyr::arrange(DATE)
    
    
    DF1<-activitycount_temp1 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(ACTIVITY_COUNT))
    
    
    DF1$DATE<-dmy(paste("01",DF1$month,DF1$year, sep="-"))
    
    output_data1<-DF1 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    activitycount1 <- dbGetQuery(conn, "select * from PROMO_SEARCH")
    activitycount1$DATE <- as.Date(as.numeric(activitycount1$DATE), origin = "1899-12-30")
    activitycount1 <- activitycount1[activitycount1$BRAND == input$Brand_P1,]
    if(input$DateFilter_P1[1] <= min(as.Date(activitycount1$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount1$DATE)))
    {
      activitycount1 <- subset(activitycount1, activitycount1$DATE >= min(as.Date(activitycount1$DATE)) & activitycount1$DATE <= max(as.Date(activitycount1$DATE)))
    }
    else
    {
      activitycount1 <- subset(activitycount1, activitycount1$DATE >= input$DateFilter_P1[1] & activitycount1$DATE <= input$DateFilter_P1[2])
    }
    activitycount1 <- subset(activitycount1,select = c(DATE,ACTIVITY_COUNT))
    
    activitycount1 <- na.omit(activitycount1)
    activitycount_temp2 <-activitycount1%>%dplyr::arrange(DATE)
    
    
    DF2<-activitycount_temp2 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(ACTIVITY_COUNT))
    
    
    DF2$DATE<-dmy(paste("01",DF2$month,DF2$year, sep="-"))
    
    output_data2<-DF2 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    activitycount2 <- dbGetQuery(conn, "select * from EMAIL")
    activitycount2$DATE <- as.Date(as.numeric(activitycount2$DATE), origin = "1899-12-30")
    activitycount2 <- activitycount2[activitycount2$DISPLAY_BRAND_NAME == input$Brand_P1,]
    if(input$DateFilter_P1[1] <= min(as.Date(activitycount2$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount2$DATE)))
    {
      activitycount2 <- subset(activitycount2, activitycount2$DATE >= min(as.Date(activitycount2$DATE)) & activitycount2$DATE <= max(as.Date(activitycount2$DATE)))
    }
    else
    {
      activitycount2 <- subset(activitycount2, activitycount2$DATE >= input$DateFilter_P1[1] & activitycount2$DATE <= input$DateFilter_P1[2])
    }
    activitycount2 <- subset(activitycount2,select = c(DATE,EMAIL_COUNT))
    activitycount2 <- na.omit(activitycount2)
    activitycount_temp3 <-activitycount2%>%dplyr::arrange(DATE)
    
    
    DF4<-activitycount_temp3 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(EMAIL_COUNT))
    
    
    DF4$DATE<-dmy(paste("01",DF4$month,DF4$year, sep="-"))
    
    output_data3<-DF4 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    activitycount3 <- dbGetQuery(conn, "select * from CALL_ACTIVITY")
    activitycount3$DATE <- as.Date(as.numeric(activitycount3$DATE), origin = "1899-12-30")
    activitycount3$SOURCE_BRAND_NAME <- toupper(activitycount3$SOURCE_BRAND_NAME)
    activitycount3 <- activitycount3[activitycount3$SOURCE_BRAND_NAME == input$Brand_P1,]
    if(input$DateFilter_P1[1] <= min(as.Date(activitycount3$DATE))|| input$DateFilter_P1[2] <= max(as.Date(activitycount3$DATE)))
    {
      activitycount3 <- subset(activitycount3, activitycount3$DATE >= min(as.Date(activitycount3$DATE)) & activitycount3$DATE <= max(as.Date(activitycount3$DATE)))
    }
    else
    {
      activitycount3 <- subset(activitycount3, activitycount3$DATE >= input$DateFilter_P1[1] & activitycount3$DATE <= input$DateFilter_P1[2])
    }
    activitycount3<- subset(activitycount3,select = c(DATE,CALL_DUR))
    activitycount3 <- na.omit(activitycount3)
    activitycount_temp4 <-activitycount3%>%dplyr::arrange(DATE)
    dbDisconnect(conn)
    
    DF5<-activitycount_temp4 %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(CALL_DUR))
    
    
    DF5$DATE<-dmy(paste("01",DF5$month,DF5$year, sep="-"))
    
    output_data4<-DF5 %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    
    
    
    result_temp <- rbind(output_data1,output_data2)
    
    result_temp2<- rbind(output_data3,output_data4)
    
    result <- rbind(result_temp,result_temp2,fill = NA)
    
    data_temp<-result %>%
      mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(Total))
    
    
    data_temp$DATE<-dmy(paste("01",data_temp$month,data_temp$year, sep="-"))
    
    result_data1<-data_temp %>%
      group_by(DATE) %>%
      summarise(Total = sum(total))
    
    result_data1
    
  })
  
  output$activityoverview <- renderPlotly({
    
    result_data1 <- activity_overview()
    
    plot_ly(result_data1, x = ~DATE, 
            y = ~Total, 
            name = 'trace 0', 
            type = 'scatter', 
            mode = 'lines')%>%
      plotly::layout(title = "Overview of Activity",height = 400)
    
    
    
  })
  
  #########################################
  # ROI Analysis
  #########################################
  
  ROI_data_func<-function(brand){
    conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")
    ROI_data <- data.frame()
    ROI_data <- dbGetQuery(conn, paste0("select * from ROI_DF where brand in('",brand,"')"))
    dbDisconnect(conn)

    return(ROI_data)
  }
  
  output$callROI <-renderPlotly({
    data <- ROI_data_func("HUMIRA")
    x <- list(
      title = "Date",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    y <- list(
      title = "ROI for Call Activity",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    # p <- plot_ly(data,x = ~Percentage, y = ~CUSTOMER_TYPE_DESCRIPTION , type = 'bar', orientation = 'h',height = 300)%>%
    #   layout(xaxis = x, yaxis = y)

    p <- data %>% plot_ly(x = ~as.character(DATE_ID), y = ~ROI_CALL,mode = 'lines+markers', type = 'scatter', height = 300, 
                         line=list( color = "rgb(0,0,128)"),
                         marker=list( color = "rgb(0,0,128)"))%>%
    layout(xaxis = x, yaxis = y)


  }) 
  
  output$emailROI <-renderPlotly({
    data <- ROI_data_func(input$Brand_P1)
    x <- list(
      title = "Date",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    y <- list(
      title = "ROI for Email Activity",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    # p <- plot_ly(data,x = ~Percentage, y = ~CUSTOMER_TYPE_DESCRIPTION , type = 'bar', orientation = 'h',height = 300)%>%
    #   layout(xaxis = x, yaxis = y)
    
    p <- data %>% plot_ly(x = ~as.character(DATE_ID), y = ~ROI_EMAIL,mode = 'lines+markers', type = 'scatter', height = 300, 
                          line=list( color = "rgb(128,0,0)"),
                          marker=list( color = "rgb(128,0,0)"))%>%
                         layout(xaxis = x, yaxis = y)
    
    
  }) 
  
  output$displayROI <-renderPlotly({
    data <- ROI_data_func(input$Brand_P1)
    x <- list(
      title = "Date",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    y <- list(
      title = "ROI for Display Activity",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    # p <- plot_ly(data,x = ~Percentage, y = ~CUSTOMER_TYPE_DESCRIPTION , type = 'bar', orientation = 'h',height = 300)%>%
    #   layout(xaxis = x, yaxis = y)
    
    p <- data %>% plot_ly(x = ~as.character(DATE_ID), y = ~ROI_DISPLAY,mode = 'lines+markers', type = 'scatter', height = 300, 
                          line=list( color = "rgb(34,139,34)"),
                          marker=list( color = "rgb(34,139,34)"))%>%
                          layout(xaxis = x, yaxis = y)
    
    
  }) 

  
  output$searchROI <-renderPlotly({
    data <- ROI_data_func(input$Brand_P1)
    x <- list(
      title = "Date",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    y <- list(
      title = "ROI for Search Activity",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    # p <- plot_ly(data,x = ~Percentage, y = ~CUSTOMER_TYPE_DESCRIPTION , type = 'bar', orientation = 'h',height = 300)%>%
    #   layout(xaxis = x, yaxis = y)
    
    p <- data %>% plot_ly(x = ~as.character(DATE_ID), y = ~ROI_SEARCH,mode = 'lines+markers', type = 'scatter', height = 300, 
                          line=list( color = "rgb(139,0,139)"),
                          marker=list( color = "rgb(139,0,139)"))%>%
      layout(xaxis = x, yaxis = y)
    
    
  })
  
  
  output$overallROI <-renderPlotly({
    data <- ROI_data_func(input$Brand_P1)
    x <- list(
      title = "Date",
        zeroline = TRUE,
        showline = TRUE,
        showticklabels = TRUE,
        showgrid = FALSE,
        linecolor = toRGB("black"),
        linewidth = 1
    )
    y <- list(
      title = "ROI for all Activities",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      linecolor = toRGB("black"),
      linewidth = 1
    )
    call = c(11,12,51,53,51,59,87,100,92,85,55,78,70,67,84,87,78)
    email = c(57,68,83,81,80,42,83,100,48,50,28,30,59,39,36,37,24)
    display = c(83,97,87,96,100,87,91,92,93,95,97,96,95,95,94,100,93)
    search = c(77,79,100,97,96,87,80,79,78,77,77,76,75,74,73,72,71)
    ROI_df = data.frame(call, email, display, search)
    # p <- plot_ly(data,x = ~Percentage, y = ~CUSTOMER_TYPE_DESCRIPTION , type = 'bar', orientation = 'h',height = 300)%>%
    #   layout(xaxis = x, yaxis = y)
    
    p <- plot_ly(data, x = ~as.character(DATE_ID), y = ~call, name = 'Call', mode = 'lines+markers', type = "scatter", height = 600)%>% add_trace(y = ~email, name = 'Email', mode = 'lines+markers')%>% add_trace(y = ~display, name = 'Display', mode = 'lines+markers')%>% add_trace(y = ~search, name = 'Search', mode = 'lines+markers')%>% 
      layout(xaxis = x, yaxis = y)
    #, height = 600)%>% 
     # layout(xaxis = x, yaxis = y)
    
    
  }) 
    
}