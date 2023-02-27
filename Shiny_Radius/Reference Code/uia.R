# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ECharts2Shiny)

options(warn=-1)


####################### For the Time Filter
TotsLactul        <- rep(ymd("2016-01-01"),10000)
randomMonths      <- round(runif(n = 10000,min = 0,max = 11),0) 
randomDays        <- round(runif(n = 10000,min = 0,max = 28),0)

# Increments days
month(TotsLactul) <- month(TotsLactul) + randomMonths  
day(TotsLactul)   <- day(TotsLactul)   + randomDays  

# Make it a DT
dateRangeInput        <- data.table::as.data.table(x=TotsLactul)

#############################




########## Dashboard header carrying the title of the dashboard

header <- dashboardHeader(title = "Radius 2.0",
                          
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Sales Dept",
                                         message = "Sales are steady this month."
                                       ),
                                       messageItem(
                                         from = "New User",
                                         message = "How do I register?",
                                         icon = icon("question"),
                                         time = "13:45"
                                       ),
                                       messageItem(
                                         from = "Support",
                                         message = "The new server is ready.",
                                         icon = icon("life-ring"),
                                         time = "2014-12-01"
                                       )),
                          
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "5 new users today",
                                         icon("users")
                                       ),
                                       notificationItem(
                                         text = "12 items delivered",
                                         icon("truck"),
                                         status = "success"
                                       ),
                                       notificationItem(
                                         text = "Server load at 86%",
                                         icon = icon("exclamation-triangle"),
                                         status = "warning"
                                       )),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 90, color = "green",
                                                "Documentation"
                                       ),
                                       taskItem(value = 17, color = "aqua",
                                                "Project X"
                                       ),
                                       taskItem(value = 75, color = "yellow",
                                                "Server deployment"
                                       ),
                                       taskItem(value = 80, color = "red",
                                                "Overall project"
                                       )
                          ))



#################################
#Sidebar content of the dashboard


sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
    menuItem("Home Page", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Main", tabName = "Main", icon = icon("dashboard")),
    menuItem("Marketing Analysis", tabName = "Marketing_Analysis", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.salesforce.com",
             badgeLabel = "new", badgeColor = "green")
  )
)




##################################
#Page layout of the home page


frow1 <- fluidRow(
  box(width = 4,height = 120,background = 'aqua',
      selectInput(inputId = "Brand_P1", label = h3("Select Brand"),
                  choices = list("MAVYRET" = "MAVYRET", "HUMIRA" = "HUMIRA",
                                 "LUPRON DEPOT" = "LUPRON DEPOT","CREON"="CREON"), selected = "HUMIRA"))
  ,box(width = 4,height = 120,background = 'navy',sliderInput("DateFilter_P1",
                                                              label= tags$b(h4("Date Filter")),
                                                              width='100%',
                                                              step=31,
                                                              min = as.Date("2016-01-01","%Y-%m-%d"),
                                                              max = as.Date("2016-12-01","%Y-%m-%d"),
                                                              value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                                                              timeFormat="%b %Y"))
  ,#valueBoxOutput("value3")
  box(width = 4,height = 120,background = 'orange',
      h3("YOY Analysis")
      #selectInput("select", label = h3("Select Channel"),
      #            choices = list("Traditional" = 1, "Email" = 2,
      #                           "Digital Advertisments" = 3), selected = 1)
      )
)

frow2 <- fluidRow( 
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

frow3 <- fluidRow( 
  box(
    h3("ROI Analysis"),
    width = 3,height = 400,background = 'purple'
  )
  ,box(
    h3("Marketing Mix"),
    width = 9,height = 400,background = 'orange'
  )
  

)

#red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
##################################
#Page layout of the second page
frow4 <- fluidRow(
  box(width = 4,height = 120,background = 'aqua',
      selectInput("select", label = h3("Select Brand"),
                  choices = list("MAVYRET" = 1, "HUMIRA" = 2,
                                 "ELAGOLIX" = 3,"CREON"=4), selected = 1))
  ,box(width = 4,height = 120,background = 'navy',sliderInput("DatesMerge",
                                                                 label= tags$b(h4("Date Filter")),
                                                                 width='100%',
                                                                 step=31,
                                                                 min = as.Date("2016-01-01","%Y-%m-%d"),
                                                                 max = as.Date("2016-12-01","%Y-%m-%d"),
                                                                 value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                                                                 timeFormat="%b %Y"))
  ,#valueBoxOutput("value3")
  box(width = 4,height = 120,background = 'orange',
      h3("YOY Analysis"),
      sliderInput("slider",
                  "Dates:",
                  min = as.Date("2016-01-01","%Y-%m-%d"),
                  max = as.Date("2016-12-01","%Y-%m-%d"),
                  value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                  timeFormat="%b %Y")
  )
)

frow5 <- fluidRow( 
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

frow6 <- fluidRow( 
  box(
    h3("TRADITIONAL MEDIA MARKETING"),
    h3("DIGITAL MEDIA"),
    width = 12,height = 50,background = 'yellow'
  )

  
  
)


frow7 <- fluidRow( 
  box(
    h3("Region wise Call Activity"),
    width = 3,height = 350,background = 'green'
  )
  ,box(
    h3("Ads showtime comparison on tv"),
    width = 3,height = 350,background = 'red'
  )  ,box(
    h3("salesforce incentives"),
    width = 3,height = 350,background = 'fuchsia'
  )
  ,box(
    h3("target achieved %"),
    width = 3,height = 350,background = 'black'
  )
  
)












##################################
# combine the two fluid rows to make the body

body <- dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
                      
  tabItems(
    tabItem(tabName = "dashboard",
            img(src='background2.PNG',style="
                                      width: 70%;
                                      display: block;
                                      margin-left: auto;
                                      margin-right: auto;"),
            tags$h1(class="HomepageText","Radius 2.0"),
            tags$h1(class="HomepageText","Next Gen Marketing Solution")
        #    column(
        #      12,
        #    tags$img(src = "background.jpg"))
    ),
    
    tabItem(tabName = "Main",
            frow1,frow2,frow3
    ),
    tabItem(tabName = "Marketing_Analysis",
            frow4,frow5,frow6,frow7
    )
  )
)



##################################
#completing the ui part with dashboardPage

ui <- dashboardPage( title = 'Radius',header =  header,sidebar =  sidebar,body =  body, skin='blue')

