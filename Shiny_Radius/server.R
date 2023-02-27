
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

Sales_Summarized<-read.csv("Datasets\\Sales_Summarized.csv")

Sales_Summarized$Date=ymd(Sales_Summarized$Date)

Customer_Count <- read.csv("Datasets\\Count_Customer_Type.csv")

Customer_Count$Date <- ymd(Customer_Count$Date)



shinyServer(function(input, output, session) {
  
  #Importing User Table from the database
  credentials  =  dbGetQuery(conn, "SELECT * FROM USER")
  
  #User Authentication
  login = FALSE
  USER <- reactiveValues(login = login)
  
  rv = reactiveValues()
  rv$Sales_Summarized=Sales_Summarized
  
  rv1 <- reactiveValues()
  rv1$Customer_Count <- Customer_Count
  
  
  
  
  
  #
  observe({
    
    rv$Sales_Summarized <- Sales_Summarized[Sales_Summarized$Brand == input$Brand_P1,]
    
    updateSliderInput(session,"DateFilter_P1",min = min(rv$Sales_Summarized$Date),
                      max = max(rv$Sales_Summarized$Date),
                      value=c(min(rv$Sales_Summarized$Date),max(rv$Sales_Summarized$Date)),timeFormat="%b %Y")
  })
  
  observe({
    
    rv1$Customer_Count <- Customer_Count[Customer_Count$DISPLAY_BRAND_NAME == input$Brand_P1,]
    
    updateSliderInput(session,"DateFilter_P1",min = min(rv1$Customer_Count$Date),
                      max = max(rv1$Customer_Count$Date),
                      value=c(min(rv1$Customer_Count$Date),max(rv1$Customer_Count$Date)),timeFormat="%b %Y")   
  })
  
  
  output$salesplot <- renderPlotly({
    
    DF3<-rv$Sales_Summarized%>%dplyr::filter(Date>=input$DateFilter_P1[1])%>%dplyr::filter(Date<=input$DateFilter_P1[2])%>%dplyr::arrange(Date)
    DF3$Monthly_Growth<-0
    for(i in seq(2,nrow(DF3))){
      DF3$Monthly_Growth<-((DF3$Monthly_Growth[i]-DF3$Monthly_Growth[i-1])/DF3$Monthly_Growth[i-1])*100
    }
    plotly::plot_ly(DF3, x = ~Date, 
                    y = ~total, 
                    name = 'trace 0', 
                    type = 'scatter', 
                    mode = 'lines')%>%
      plotly::layout( height = 325)
  })
  
  
  observeEvent(input$button, {
    updateTabItems(session, "tabs", "Marketing_Analysis")
    
  })
  
  output$countplot <- renderPlot({
    
    ggplot(data = Customer_Count$Count)
  })
  
  
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
  
  #Displaying Logged in User to Navbar
  #output$loggedinuser<-renderUI({ 
  #  
  #  req(USER$login)
  #  tags$h2(input$userName,
  #         class = "dropdown", 
  #          style = "background-color: #eee !important; border: 0;
  #          font-weight: bold; margin:5px; padding: 10px;")
  #  })
  
  #Logout Button
  output$logoutbtn <- renderUI({
    
    req(USER$login)
    tags$div(a(icon("fa fa-sign-out"), "Logout", 
               href="javascript:window.location.reload(true)"),
             class = "dropdown", 
             style = "background-color: #eee !important; border: 0;
             font-weight: bold; margin:5px; padding: 10px;")
  })
  
  #Sidebar Panel dynamic according to the logged in user
  output$sidebarpanel <- renderUI({
    
    if (USER$login == TRUE ){ 
      #If the logged in user is advanced then File upload option is also displayed
      if (credentials[,"user_type"][which(credentials$username==input$userName)]=="advanced") {
        sidebarMenu(id="tabs",
                    menuItem("File Upload", tabName = "FileUpload", icon = icon("th")),
                    menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
                    shinyjs::hidden(menuItem("Market Analysis", tabName = "Marketing_Analysis", icon = icon("dashboard"))),
                    menuItem("Brand Filter",selectInput(inputId = "Brand_P1", label = "Select Brand",
                                                        choices = list("MAVYRET" = "MAVYRET", "HUMIRA" = "HUMIRA",
                                                                       "LUPRON DEPOT" = "LUPRON DEPOT","CREON"="CREON"), selected = "HUMIRA")),
                    menuItem("Date Filter",sliderInput("DateFilter_P1",
                                                       label= tags$b(h4("Filter Date")),
                                                       width='100%',
                                                       step=31,
                                                       min = as.Date("2016-01-01","%Y-%m-%d"),
                                                       max = as.Date("2016-12-01","%Y-%m-%d"),
                                                       value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                                                       timeFormat="%b %Y"))
                    
        )
      }
      else{
        sidebarMenu(id="tabs",
                    menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
                    shinyjs::hidden(menuItem("Market Analysis", tabName = "Marketing_Analysis", icon = icon("dashboard"))),
                    menuItem("Brand Filter",selectInput(inputId = "Brand_P1", label = "Select Brand",
                                                        choices = list("MAVYRET" = "MAVYRET", "HUMIRA" = "HUMIRA",
                                                                       "LUPRON DEPOT" = "LUPRON DEPOT","CREON"="CREON"), selected = "HUMIRA")),
                    menuItem("Date Filter",sliderInput("DateFilter_P1",
                                                       label= tags$b(h4("Filter Date")),
                                                       width='100%',
                                                       step=31,
                                                       min = as.Date("2016-01-01","%Y-%m-%d"),
                                                       max = as.Date("2016-12-01","%Y-%m-%d"),
                                                       value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                                                       timeFormat="%b %Y"))
                    
        )
        
      }
    }
  })
  
  
  #Bodu Panel dynamic according to the logged in user
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      #If the logged in user is advanced then File upload option is also displayed
      if (credentials[,"user_type"][which(credentials$username==input$userName)]=="advanced") {
        
        tabItems(
          tabItem(
            tabName ="FileUpload", class = "active",
            h2("This is file Upload Page")
          ),
          tabItem(
            tabName ="dashboard",
            MainPageRow1,MainPageRow2),
          
          
          
          tabItem(tabName = "Marketing_Analysis",
                  MarketPageRow1,MarketPageRow2,MarketPageRow3
          )
        )
      } 
      else {
        
        tabItems(
          
          tabItem(
            
            tabName ="dashboard", class = "active",
            MainPageRow1,MainPageRow2),
          
          tabItem(tabName = "Marketing_Analysis",
                  MarketPageRow1,MarketPageRow2,MarketPageRow3
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
  
  })

# dbDisconnect(conn)
