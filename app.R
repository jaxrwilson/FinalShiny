library(shiny)
library(fpp3)
library(ggplot2)
library(shinyWidgets)
library(quantmod)
library(plotly)
library(dplyr)
library(shinydashboard)
library(tidyquant)
library(flexdashboard)
library(feasts)
library(ggeasy)
library(ggthemes)
library(seasonal)
library(seasonalview)


# Path where data is
file_path <- "multiTimeline.csv"
g_trends <- read.csv(file_path, skip = 2)

# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)


ui <-
  dashboardPage( skin = "blue",
                 dashboardHeader(title = "Interest in \"College Football\"", titleWidth = 500),
                 dashboardSidebar( width = 200,
                                   sidebarMenu(
                                     menuItem("Introduction", tabName = "intro", 
                                              icon = icon("dashboard")),
                                     menuItem("Full-Time Series", tabName = "graph1", 
                                              icon = icon("chart-line")),
                                     menuItem("Plot Choice", tabName = "graph2", 
                                              icon = icon("chart-line")),
                                     menuItem("Simple Models", tabName = "SimpleModel", 
                                              icon = icon("chart-line")),
                                     menuItem("Exponential Smoothing", tabName = "ETSmodel", 
                                              icon = icon("chart-line")),
                                     menuItem("ARIMA Model", tabName = "ArimaModel", 
                                              icon = icon("chart-line"))
                                   )
                 ),
                 
                 dashboardBody(
                   tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "sans", Arial, "Arial";
        font-size: 20px;
      }
    '))),
                 
                 tabItems(
                   # First tab content
                   tabItem(tabName = "intro",
                           h1("Introduction"), 
                           
                           hr(),
                           
                           tags$div(
                             tags$h3("This app analyzes the interest in 
                                      \"College Football\" from data collected by 
                                      GoogleTrends."),
                             
                             tags$head(tags$style('h3 {color:#000099;}')),
                             
                             tags$br(),
                             
                             tags$h3("The second tab displays the Full-Time Series graphic for the interest in \"College Football\" from January 2004 to March 2022."),
                             
                             tags$br(),
                             
                             tags$h3("The third tab displays the user choice in one of three types of graphics: seasonality, autocorrelation, and decomposition. "),
                             
                             tags$br(),
                             
                             tags$h3("The fourth tab displays the user choice in one of three types of forecasts: Mean, Naïve, Seasonal Naïve, and Drift"),
                             
                             tags$br(),
                             
                             tags$h3("The fifth tab displays the user choice of two types of the Exponential Smooth Forecasts: Holts or Holts/Winters"),
                             
                             tags$br(),
                             
                             tags$h3("The sixth tab displays the user choice in one of four types of forecasts: ARIMA (2,1,0), ARIMA (0,1,2)(0,1,1), ARIMA (2,1,0)(0,1,1), & Autoselected ARIMA"),
                             
                             tags$br(),
                           ),
                           
                           
                   ),
                                
                   # Second tab content
                   tabItem(tabName = "graph1",
                           h1("Full-Time Series Graph"),
                           
                           hr(),
                           
                           basicPage(
                             plotlyOutput("fulltimeseries")
                           ),
                           
                           hr(),
                           
                           h3("Interpretation"),
                           
                           h4("The full-time series shows that the trend has strong
                                seasonality throughtout the plot. This is 
                                likely due to football season.")
                           
                           
                   ), 
                   
                   # Third tab content
                   tabItem(tabName = "graph2",
                           h1("Which plot would you like to see?"), 
                           
                           hr(),
                           
                           radioButtons("plot_type", 
                                        label = h3("Choose which plot you'd like to View:"),
                                        choices = c("Seasonality", 
                                                    "Autocorrelation", 
                                                    "Decomposition")),
                           
                           hr(),
                           
                           plotOutput("myplot"),
                           
                           hr(),
                           
                           h3("Interpretation"),
                           
                           textOutput("myplotint")
                           
                   ),
                   
                   # Fourth tab content
                   tabItem(tabName = "SimpleModel",
                           h1("Which Simple Forecast option you'd like to see?"),
                           
                           hr(),
                           
                           radioButtons("forecast_type_simple", 
                                        label = h3("Choose which Forecast you'd like to view:"),
                                        choices = c("Mean", 
                                                    "Naïve", 
                                                    "Seasonal Naïve",
                                                    "Drift")),
                           
                           hr(),
                           
                           
                           plotOutput("simple_forecast")
                           
                           
                           
                   ),
                   #fifth tab content
                   tabItem(tabName = "ETSmodel",
                           h1("Which Exponential Smoothing Forecast would you like to see?"),
                           
                           hr(),
                           
                           radioButtons("forecast_type_ETS", 
                                        label = h3("Choose which Forecast you'd like to view:"),
                                        choices = c("Holts",
                                                    "Holts/Winters")),
                           
                           hr(),
                           
                           
                           plotOutput("ETS_forecast")  
                   ),
                   
                   #sixth tab content
                   tabItem(tabName = "ArimaModel",
                           h1("Which ARIMA model would you like to see?"),
                           
                           hr(),
                           
                           selectInput(inputId = "forecast_type_arima", 
                                        label = h3("Choose which one would you like to view:"),
                                        choices = list("Autoselected ARIMA",
                                                    "Manual ARIMA"),
                                       selected = 1
                                       ),
                           
                           selectInput(
                             inputId = "Parameter1",
                             label = "If manual, select parameter 1",
                             choices = list("0", "1", "2"),
                             selected = 1
                           ),
                           selectInput(
                             inputId = "Parameter2",
                             label = "If manual, select parameter 2",
                             choices = list("0", "1"),
                             selected = 1
                           ),
                           selectInput(
                             inputId = "Parameter3",
                             label = "If manual, select parameter 3",
                             choices = list("0", "1", "2"),
                             selected = 1
                           ),
                           
                           hr(),
                           
                          
                           plotOutput("arima_forecast")
                           
      
                 )
                   
                 )))



server <- function(input, output, session) {
  output$fulltimeseries <- renderPlotly({
    p <- ggplot(g_trends, aes(Month, Interest)) + 
      geom_line(color = "#22afff") + 
      theme_fivethirtyeight()+
      labs(title = "The Interest of \"College Football\"", y = "Interest") +
      ggeasy::easy_center_title() +
      ggeasy::easy_all_text_color(color = "#000099") +
      theme(plot.background = element_rect(fill = "white"), 
            panel.background = element_rect(fill = "white"))
    ggplotly(p)
  })
  
  output$myplot <- renderPlot({
    if (input$plot_type == "Seasonality") {
      g_trends %>% gg_season(Interest)+
        theme_fivethirtyeight()+
        labs(title = "The Interest of \"College Football\"", y = "Interest") +
        ggeasy::easy_center_title() +
        ggeasy::easy_all_text_color(color = "#000099") +
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    } 
    else if (input$plot_type == "Autocorrelation") {
      g_trends %>% ACF() %>% 
        autoplot()+
        labs(title = "Interest of College Football")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "#000099")+
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    }
    else if (input$plot_type == "Decomposition") {
      x11_dcmp <- g_trends %>%
        model(x11 = X_13ARIMA_SEATS(Interest ~ x11())) %>%
        components()
      autoplot(x11_dcmp) +
        labs(title = "Decomposition of Interest of \" College Football\" using X-11.")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "#000099")+
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    }
  })
  
  output$myplotint <- renderText({
    if (input$plot_type == "Seasonality") {
      noquote(paste(c("The seasonality plot shows that the interest 
      in \"College Football\" peaks from September until around January, which makes sense 
      considering this coincides with football season.", 
                      collapse = " ")))
    } 
    else if (input$plot_type == "Autocorrelation") {
      noquote(paste(c("The autocorrelation plot shows that the 
      interest in \"College Football\" is seasonal.", collapse = " ")))
    }
    else if (input$plot_type == "Decomposition") {
      noquote(paste(c("The X11 decomposition plot shows that the trend
      peaked in about January 2020. The plot also shows a consistent amount
      of seasonality.", collapse = " ")))
    }
    
    
  })
  
  output$simple_forecast <- renderPlot({
    if(input$forecast_type_simple == "Mean") {
      interest_fit <- g_trends %>%
        model(
          Mean = MEAN(Interest)
        )
      
      interest_fc <- interest_fit %>% forecast(h = 24)
      
      interest_fc %>%
        autoplot(g_trends, level = NULL) +
        autolayer(
          filter_index(g_trends, "2004 Jan" ~.),
          colour = "#000099"
        ) +
        labs( y = "Interest",
              title = "Mean Forecast for interest in \"College Football\""
        ) + 
        guides(colour = guide_legend(title = "Forecast"))+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_color(color = "#22afff")
    }
    else if(input$forecast_type_simple == "Naïve") {
      interest_fit <- g_trends %>%
        model(`Naïve` = NAIVE(Interest)
        )
      
      interest_fc <- interest_fit %>% forecast(h = 24)
      
      interest_fc %>%
        autoplot(g_trends, level = NULL) +
        autolayer(
          filter_index(g_trends, "2004 Jan" ~ .),
          colour = "#000099"
        ) +
        labs( y = "Interest",
              title = "Naïve Forecast for interest in \"College Football\""
        ) + 
        guides(colour = guide_legend(title = "Forecast"))+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_color(color = "#22afff")
    }
    
    else if(input$forecast_type_simple == "Seasonal Naïve") {
      interest_fit <- g_trends %>%
        model(`Seasonal naïve` = SNAIVE(Interest)
        )
      
      interest_fc <- interest_fit %>% forecast(h = 24)
      
      interest_fc %>%
        autoplot(g_trends, level = NULL) +
        autolayer(
          filter_index(g_trends, "2004 Jan" ~ .),
          colour = "#000099"
        ) +
        labs( y = "Interest",
              title = "Seasonal Naïve Forecast for interest in \"College Football\""
        ) + 
        guides(colour = guide_legend(title = "Forecast"))+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_color(color = "#22afff")
    }
    else if (input$forecast_type_simple == "Drift") {
      interest_fit <- g_trends %>%
        model('Drift' = RW(Interest ~ drift())
        )
      interest_fc <- interest_fit %>% forecast(h = 24)
      
      interest_fc %>% 
        autoplot(g_trends, level = NULL) +
        autolayer(
          filter_index(g_trends, "2004 Jan" ~ .),
          colour = "#000099"
        ) +
        labs( y = "Interest",
              title = "Drift Forecast for interest in \"College Football\""
        ) + 
        guides(colour = guide_legend(title = "Forecast"))+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_color(color = "#22afff")
    }
    
  })
  
  output$ETS_forecast <- renderPlot({
    if(input$forecast_type_ETS == "Holts") {
      interest_fit <- g_trends %>%
        model(
          Holts = ETS(Interest ~ error("A") + trend("A") + season("N"))
        )
      
      interest_fc <- interest_fit %>% forecast(h = 24)
      
      interest_fc %>% 
        autoplot(g_trends, level = NULL) +
        autolayer(
          filter_index(g_trends, "2004 Jan" ~ .),
          colour = "#000099"
        ) +
        labs( y = "Interest",
              title = "Holts Forecast for interest in \"College Football\""
        ) + 
        guides(colour = guide_legend(title = "Forecast"))+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_color(color = "#22afff")
    }
    else if(input$forecast_type_ETS == "Holts/Winters") {
      interest_fit <- g_trends %>%
        model(`Holts/Winters` = ETS(Interest ~ error("A") + trend("A") 
                                    + season("A")))
      
      interest_fc <- interest_fit %>% forecast(h = 24)
      
      interest_fc %>% 
        autoplot(g_trends, level = NULL) +
        autolayer(
          filter_index(g_trends, "2004 Jan" ~ .),
          colour = "#000099"
        ) +
        labs( y = "Interest",
              title = "Holts/Winters Forecast for interest in \"College Football\""
        ) + 
        guides(colour = guide_legend(title = "Forecast"))+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_color(color = "#22afff")
    }
    
    
  })
  output$arima_forecast <- renderPlot({
    interest <- g_trends %>%
      filter_index("2004 Jan" ~ .) %>%
      select(g_trends)
    
    if (input$forecast_type_arima == "Autoselected ARIMA") {
      interest_fit <- interest %>%
        model(
          Step = ARIMA(Interest, stepwise=TRUE)
        )
      
      interest_fc <- interest_fit %>% forecast(h = 24)
      
      interest_fc %>%
        autoplot(interest, level = NULL) +
        autolayer(
          filter_index(Interest, "2004 Jan"~.),
          colour = "#000099"
        ) +
        labs(
          y = "Interest",
          title = "Autoselected Arima Forecast for Interest in College Football"
        ) +
        guides(colour = guide_legend(title = "Forecast"))
    } else {
      interest_fit <- interest %>%
        model(
          ManualArima = ARIMA(Interest ~ pdq(as.integer(input$Parameter1),as.integer(input$Parameter2),as.integer(input$Parameter3)))
        )
      
      interest_fc <- interest_fit %>% forecast(h = 24)
      
      interest_fc %>%
        autoplot(interest, level = NULL) +
        autolayer(
          filter_index(Interest, "2004 Jan" ~ .),
          colour = "#000099"
        ) +
        labs(
          y = "Interest",
          title = "Manual ARIMA forecast for Interest in College Football"
        ) +
        guides(colour = guide_legend(title = "Forecast"))
    }
  }) 
  

  
}

shinyApp(ui, server)