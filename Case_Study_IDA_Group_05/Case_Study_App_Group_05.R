if (!require(shiny)) {
  install.packages("shiny")
}

if (!require(DT)) {
  install.packages("DT")
}

if (!require(plotly)) {
  install.packages("plotly")
}

if (!require(readr)) {
  install.packages("readr")
}

if (!require(stringr)) {
  install.packages("stringr")
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
}

if (!require(dplyr)) {
  install.packages("dplyr")
}

if (!require(shinyWidgets)) {
  install.packages("shinyWidgets")
}

if (!require(R6)) {
  install.packages("R6")
}

if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!require(scales)) {
  install.packages("scales")
}
library(scales)

if (!require(forecast)) {
  install.packages("forecast")
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}
if (!require(R6)) {
  install.packages("R6")
}
if (!require(RCurl)) {
  install.packages("RCurl")
}
if (!require(RJSONIO)) {
  install.packages("RJSONIO")
}
if (!require(plyr)) {
  install.packages("plyr")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
}
if (!require(lubridate)) {
  install.packages("lubridate")
}
if (!require(readr)) {
  install.packages("readr")
}
if (!require(forecast)) {
  install.packages("forecast")
}
if (!require(padr)) {
  install.packages("padr")
}
if (!require(tidyquant)) {
  install.packages("tidyquant")
}
if (!require(timetk)) {
  install.packages("timetk")
}
if (!require(data.table)) {
  install.packages("data.table")
  require(data.table)
}


# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }
# 
# # usage
# packages <- c("shiny","shinyWidgets","shinydashboard","R6","RCurl","rmdformats","prettydoc","kableExtra","install.load","tidyverse","thematic","knitr","reader","ggplot2","DT", "plyr", "reshape2", "RColorBrewer", "scales", "grid","plotly","stringr","RJSONIO","dplyr","leaflet","leaflet.extras","lubridate","forecast","padr","tidyquant","scales","timetk","data.table")
# ipak(packages)

# Load data ----------------------------------------------------------------------------------------------

Final_Daten <- read.csv2("Final_Dataset_Group_05.csv")

# Here the important data is extracted from the final generated csv file in preparation for the visualization later

data <- Final_Daten %>%
  filter(Fehlerhaft >= 1) %>%
  select(c("Typ_Einzelteil", "ID_Einzelteil", "Gemeinde", "Fehlerhaft", "Fehlerhaft_Datum", "Laengengrad", "Breitengrad")) %>%
  mutate(datum = as.Date(Fehlerhaft_Datum, format = "%Y-%m-%d"))


# UI Design ----------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  # Define the theme color of the app
  # skin = "lightblue",
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro&display=swap');
      body {
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 500;
      }
      /* Change font of header text */
      h2 {
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 500;
        color:lightblue
      }
      .butt2{
        font-family: Source Sans Pro, sans-serif;
        margin-top: 25px; margin-left: 5px;
        background-color:lightblue
        
      }
      /* Make text visible on inputs */
      .shiny-input-container {
        color: lightblue;
      }")),
  ),
  
  # Define the title and logo of the app
  fluidRow(
    # includeCSS("www/styles.css"),
    column(
      width = 6,
      titlePanel(
        div(width = 5, h1("Case_Study_APP_05"))
        # , style = 'font: bold;font-family:"Source Sans Pro"; color: lightblue;'
      )
    ),
    column(width = 1, tags$img(src = "img_IDA.jpg", align = "center"))
  ),
  
  # Define the three sections of the app in order to show the number of part failures, predictions and tables respectively
  fluidRow(
    # includeCSS("www/styles.css"),
    tabBox(
      width = 12,
      tabPanel(
        div(h5("Number of Failures"), style = 'font: Bold 700; font-family:Source Sans Pro; color: lightblue;'),
        # title = "Number of Failures",
        fluidRow(
          column(
            width = 4,
            style = 'font: Bold 700; font-family:"Source Sans Pro"; color: lightblue;',
            selectizeInput(
              inputId = "city",
              label = "Select a city or cities",
              choices = c(""),
              selected = c(""),
              multiple = TRUE,
              width = "100%"
            )
          ),
          column(
            width = 4,
            style = 'font: Bold 700; font-family:"Source Sans Pro"; color: lightblue;',
            selectizeInput(
              inputId = "einzelteil",
              label = "Select a part or parts",
              choices = c(""),
              selected = c(""),
              multiple = TRUE,
              width = "100%"
            )
          ),
          column(
            width = 3,
            actionButton(
              inputId = "add_histogram",
              label = "Histogram",
              width = "100%",
              class = "butt2"
            )
          )
        ),
        wellPanel(plotlyOutput("plot_reg_hist"))
      ),
      tabPanel(
        # title = "Predict and Map",
        div(h5("Predict and Map"), style = 'font: Bold 700; font-family:Source Sans Pro; color: lightblue;'),
        wellPanel(plotlyOutput("total_outage")),
        wellPanel(plotlyOutput("plot_total_forecast")),
        fluidRow(
          column(
            width = 4,
            style = 'font: Bold 700; font-family:"Source Sans Pro"; color: lightblue;',
            selectizeInput(
              inputId = "part_predict",
              label = "Select a part",
              choices = c(""),
              selected = c(""),
              width = "100%"
            )
          ),
          column(
            width = 4,
            style = 'font: Bold 700; font-family:"Source Sans Pro"; color: lightblue;',
            selectizeInput(
              inputId = "gemeinde",
              label = "Select a city",
              choices = c(""),
              selected = c(""),
              width = "100%"
            )
          ),
          column(
            width = 3,
            actionButton(
              inputId = "add_predict",
              label = "Predict",
              width = "100%",
              class = "butt2"
            )
          ),
        ),
        wellPanel(plotlyOutput("plot_forecast")),
        wellPanel(plotlyOutput("plot_part_forecast")),
        wellPanel(leafletOutput("map"))
      ),
      tabPanel(
        # title = "Table",
        div(h5("Table"), style = 'font: Bold 700; font-family:"Source Sans Pro"; color: lightblue;'),
        DT::dataTableOutput("data_table"),
      )
    )
  )
)

# Server ----------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
##------------------------------------------------------------------------------------------------------
  
  # 4.a A bar chart stacking the number of failures for each part for each month. It should be possible to filter by location.

  # Real-time update of selected cities and parts
  updateSelectizeInput(session, inputId = "city", choices = unique(data$Gemeinde), selected = c(""), server = TRUE)
  updateSelectizeInput(session, inputId = "einzelteil", choices = unique(data$Typ_Einzelteil), selected = c(""), server = TRUE)

  # Datenverarbeitung, um die Anzahl der Ausfälle pro Teil und Stadt in den letzten drei Jahren zu ermitteln
  Anzahl_Fehlerhaft_Einzelteil_in_Monate <- reactive({
    data %>%
      filter(Gemeinde %in% input$city) %>%
      filter(Typ_Einzelteil %in% input$einzelteil) %>%
      mutate(Month = month(Fehlerhaft_Datum), Year = year(Fehlerhaft_Datum)) %>%
      mutate(Monat = as.Date(format.Date(Fehlerhaft_Datum, "%Y-%m-1"), "%Y-%m-%d")) %>%
      group_by(Gemeinde, Month, Monat, Typ_Einzelteil) %>%
      dplyr::summarise(Anzahl = n(), .groups = "keep")
  })

  # Die Visualisierung wird über eine Schaltfläche gesteuert, hier unter Verwendung von geom_bar, 
  # um die Anzahl der Ausfälle pro Teil pro Stadt für die letzten drei Jahre anzuzeigen
  observeEvent(input$add_histogram, {
    output$plot_reg_hist <- plotly::renderPlotly({
      p <- ggplot(Anzahl_Fehlerhaft_Einzelteil_in_Monate(), aes(x = Monat, y = Anzahl, fill = Gemeinde, group = Typ_Einzelteil)) +
        geom_bar(stat = "identity", position = "stack") +
        guides(fill = guide_legend(title = "Gemeinden")) +
        scale_fill_manual(values = c(
          "red", "blue", "green", "yellow", "goldenrod3",
          "yellowgreen", "rosybrown3", "magenta", "cyan", "cornsilk4"
        )) +
        labs(x = "Month", y = "The number of failures", title = "The number of failures for each part for each month") +
        scale_x_date(
          breaks = "1 month",
          labels = date_format(format = "%Y-%b", tz = "ECT"),
        ) +
        scale_y_continuous(
          breaks = function(x) {
            unique(floor(pretty(x)))
          }
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 0, size = 7),
          axis.text.y = element_text(size = 10), # die Schriftgröße der horizontalen und vertikalen Achsenüberschriften anpassen
          axis.title = element_text(size = 10),
          legend.position = "bottom", legend.box = "horizontal"
        )

      ggplotly(p)
    })
  })
  
##------------------------------------------------------------------------------------------------------
  
  # b. The outage history and a forecast for the outage of a selectable part for the 1st quarter of 2017.
  # Both the total outage, and the outage per city mentioned above, should be apparent from the visualization.

  # Die Gesamtzahl der Ausfälle pro Teil für die letzten drei Jahre ist hier dargestellt
  total_outage_data <- Final_Daten %>%
    filter(Fehlerhaft >= 1) %>%
    select(c("Typ_Einzelteil", "ID_Einzelteil", "Gemeinde", "Fehlerhaft", "Fehlerhaft_Datum", "Laengengrad", "Breitengrad")) %>%
    mutate(Monat = as.Date(format.Date(Fehlerhaft_Datum, "%Y-%m-1"), "%Y-%m-%d")) %>%
    group_by(Typ_Einzelteil) %>%
    dplyr::summarise(Anzahl = n(), .groups = "keep")

  output$total_outage <- plotly::renderPlotly({
    p <- total_outage_data %>% ggplot(aes(x = Typ_Einzelteil, y = Anzahl, color = Typ_Einzelteil, fill = Typ_Einzelteil)) +
      geom_bar(stat = "identity") +
      labs(x = "Typ_Einzelteil", y = "The number of failures", title = "The total number of failures for each part in ten cities") +
      scale_y_continuous(
        breaks = function(x) {
          unique(floor(pretty(x)))
        }
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 0, size = 7),
        axis.text.y = element_text(size = 10), # die Schriftgröße der horizontalen und vertikalen Achsenüberschriften anpassen
        axis.title = element_text(size = 10),
        legend.position = "bottom", legend.box = "horizontal"
      )
    ggplotly(p)
  })
  
##------------------------------------------------------------------------------------------------------
  # Hier eine Prognose des Gesamtbetrags, 
  # der für die einzelnen Teile im ersten Quartal 2017 vorbereitet werden muss
  forecast_data_total <- Final_Daten %>%
    select(c("Typ_Einzelteil", "Gemeinde", "Fehlerhaft", "Fehlerhaft_Datum")) %>%
    filter(Fehlerhaft >= 1) %>%
    mutate(Month = month(Fehlerhaft_Datum), Year = year(Fehlerhaft_Datum)) %>%
    mutate(Quarter = quarters(as.POSIXlt(Fehlerhaft_Datum))) %>%
    group_by(Typ_Einzelteil,Gemeinde,Year, Quarter ) %>%
    dplyr::summarise(Anzahl = n(), .groups = "keep") %>%
    spread(Gemeinde, Anzahl, fill = 0)
  
  
  output$plot_total_forecast <- plotly::renderPlotly({
    
    f <- function(i,j) {
      
      Quarter <- c(1,2,3,4,5,6,7,8,9,10,11,12)
      
      forecast_data_total_1 <- forecast_data_total[(j*12-11):(j*12),]
      
      data <- data.frame(Quarter, forecast_data_total_1[,i])
      
      #fiting the linear model
      liner_model <- lm(formula = as.formula(paste(colnames(data)[2], "~ .")), data)
      
      Q1_2017 <- data.frame(Quarter = 13)
      #predicts the future values
      
      q1 <- ceiling(predict(liner_model,newdata = Q1_2017))
      return(q1)
    }
    
    f.total <- function(i){
      
      predict_Q1 <- data.frame(Gemeinde = c("ASCHAFFENBURG","AUGSBURG","BAMBERG","BAYREUTH","ERLANGEN","INGOLSTADT","LANDSHUT","REGENSBURG","ROSENHEIM","WUERZBURG"), Prediction = c(f(4,i),f(5,i),f(6,i),f(7,i),f(8,i),f(9,i),f(10,i),f(11,i),f(12,i), f(13,i)))
      
      predict_t <- data.frame(Prediction <- sum(predict_Q1$Prediction))
      return(predict_t)
    }
    
    Prediction <- list(f.total(1),f.total(2),f.total(3),f.total(4),f.total(5),f.total(6),f.total(7),f.total(8),f.total(9),f.total(10),f.total(11),f.total(12),f.total(13),f.total(14),f.total(15),f.total(16),f.total(17),f.total(18),
                       f.total(19),f.total(20),f.total(21),f.total(22),f.total(23),f.total(24),f.total(25),f.total(26),f.total(27),f.total(28),f.total(29),f.total(30),f.total(31),f.total(32),f.total(33),f.total(34),f.total(35),f.total(36),f.total(37),f.total(38))
    
    data_p <- rbindlist(Prediction)
    
    colnames(data_p) <- c("Prediction")
    
    predict_total <- data.frame(Typ_Einzelteil = c("ID_T01","ID_T10","ID_T11","ID_T12","ID_T13","ID_T14","ID_T15","ID_T16","ID_T17","ID_T18","ID_T19","ID_T02","ID_T20","ID_T21","ID_T22","ID_T23","ID_T24","ID_T25","ID_T26","ID_T27","ID_T03","ID_T30","ID_T31","ID_T32","ID_T33","ID_T34","ID_T35","ID_T36","ID_T37","ID_T38","ID_T39","ID_T04","ID_T40","ID_T05","ID_T06","ID_T07","ID_T08","ID_T09"),
                                Prediction = data_p)
    
    
    g <- predict_total %>% ggplot(aes(x = Typ_Einzelteil, y = Prediction, fill = Typ_Einzelteil, color = Typ_Einzelteil)) +
      geom_bar(stat = "identity") +
      labs(x = "Typ_Einzelteil", y = "Total predicted quantity per part", title = "The total forecast quantities for each part for Q1 2017") +
      scale_y_continuous(
        breaks = function(x) {
          unique(floor(pretty(x)))
        }
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 0, size = 7),
        axis.text.y = element_text(size = 10), # die Schriftgröße der horizontalen und vertikalen Achsenüberschriften anpassen
        axis.title = element_text(size = 10),
        legend.position = "bottom", legend.box = "horizontal"
      )
    
    ggplotly(g)
  })

  # predict
  updateSelectizeInput(session, inputId = "part_predict", choices = unique(data$Typ_Einzelteil), selected = c(""), server = TRUE)
  updateSelectizeInput(session, inputId = "gemeinde", choices = unique(data$Gemeinde), selected = c(""), server = TRUE)


  # Linear model data
  linear_model_Data <- reactive({
    d <- Final_Daten %>%
      filter(Fehlerhaft >= 1) %>%
      select(c("Typ_Einzelteil", "Gemeinde", "Fehlerhaft", "Fehlerhaft_Datum")) %>%
      filter(Typ_Einzelteil %in% input$part_predict) %>%
      filter(Gemeinde %in% input$gemeinde) %>%
      mutate(Month = month(Fehlerhaft_Datum), Year = year(Fehlerhaft_Datum)) %>%
      mutate(Monat = as.Date(format.Date(Fehlerhaft_Datum, "%Y-%m-1"), "%Y-%m-%d")) %>%
      mutate(Quarter = quarters(as.POSIXlt(Fehlerhaft_Datum))) %>%
      group_by(Year, Monat, Typ_Einzelteil, Gemeinde) %>%
      dplyr::summarise(Anzahl = n(), .groups = "keep")

    g1 <- tibble(d) %>%
      pad_by_time(Monat,
        .by = "months", .start_date = "2014-01-01",
        .end_date = "2016-12-31", .pad_value = 0
      ) %>%
      select(c("Monat", "Anzahl"))


    g2 <- data.frame(Anzahl = c(
      sum(g1$Anzahl[1:3]), sum(g1$Anzahl[4:6]), sum(g1$Anzahl[7:9]),
      sum(g1$Anzahl[10:12]), sum(g1$Anzahl[13:15]), sum(g1$Anzahl[16:18]),
      sum(g1$Anzahl[19:21]), sum(g1$Anzahl[22:24]), sum(g1$Anzahl[25:27]),
      sum(g1$Anzahl[28:30]), sum(g1$Anzahl[31:33]), sum(g1$Anzahl[34:36])
    ))
  })


  # Forecast data per part per city in 2017 - liner_model
  predict_data <- reactive({
    Final_Daten %>%
      select(c("Typ_Einzelteil", "Gemeinde", "Fehlerhaft", "Fehlerhaft_Datum")) %>%
      filter(Fehlerhaft >= 1) %>%
      filter(Typ_Einzelteil %in% input$part_predict) %>%
      mutate(Month = month(Fehlerhaft_Datum), Year = year(Fehlerhaft_Datum)) %>%
      mutate(Quarter = quarters(as.POSIXlt(Fehlerhaft_Datum))) %>%
      group_by(Year, Quarter, Gemeinde, Typ_Einzelteil) %>%
      dplyr::summarise(Anzahl = n(), .groups = "keep")
  })

  
  
##------------------------------------------------------------------------------------------------------
  
  # predict map
  part_data <- reactive({
    Final_Daten %>%
      filter(Fehlerhaft >= 1) %>%
      select(c("Gemeinde", "Typ_Einzelteil")) %>%
      filter(Typ_Einzelteil %in% input$part_predict) %>%
      group_by(Gemeinde)
  })

  m_data <- Final_Daten %>%
    filter(Fehlerhaft >= 1) %>%
    select(c("Typ_Einzelteil", "ID_Einzelteil", "Gemeinde", "Fehlerhaft", "Fehlerhaft_Datum", "Laengengrad", "Breitengrad")) %>%
    mutate(Monat = as.Date(format.Date(Fehlerhaft_Datum, "%Y-%m-1"), "%Y-%m-%d")) %>%
    group_by(Typ_Einzelteil, Laengengrad, Gemeinde, Breitengrad) %>%
    dplyr::summarise(Anzahl = n(), .groups = "keep")

  shared_Data <- reactiveValues()

  observeEvent(input$add_predict, {
    output$plot_forecast <- plotly::renderPlotly({
      Quarter <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
      model_data <- data.frame(Quarter, Anzahl = linear_model_Data()$Anzahl)

      liner_model <- lm(formula = Anzahl ~ Quarter, model_data)

      intercept <- liner_model[[1]][[1]]
      slope <- liner_model[[1]][[2]]

      Q1_2017 <- data.frame(Quarter = 13)

      ggplot(model_data, aes(x = Quarter, y = Anzahl)) +
        geom_point() +
        geom_abline(intercept = intercept, slope = slope, linetype = 2) +
        geom_point(aes(x = Q1_2017$Quarter, y = predict(liner_model, newdata = Q1_2017)), colour = "red", size = 3) +
        labs(x = "Quarter", y = "The number of failures", title = "The linear model of failures for each part") +
        scale_x_continuous(
          breaks = function(x) {
            unique(floor(pretty(x)))
          }
        )
    })

    output$plot_part_forecast <- plotly::renderPlotly({
      part_data <- spread(predict_data(), Gemeinde, Anzahl)

      part_data[is.na(part_data)] <- 0

      Quarter <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

      f <- function(i) {
        predict_part_data <- data.frame(Quarter, part_data[, i])

        # fiting the linear model
        liner_model <- lm(formula = as.formula(paste(colnames(predict_part_data)[2], "~ .")), predict_part_data)

        Q1_2017 <- data.frame(Quarter = 13)
        # predicts the future values
        q1 <- ceiling(predict(liner_model, newdata = Q1_2017))

        return(q1)
      }

      predict_Q1 <- data.frame(
        Prediction = c(f(4), f(5), f(6), f(7), f(8), f(9), f(10), f(11), f(12), f(13)),
        Gemeinde = c("ASCHAFFENBURG", "AUGSBURG", "BAMBERG", "BAYREUTH", "ERLANGEN", "INGOLSTADT", "LANDSHUT", "REGENSBURG", "ROSENHEIM", "WUERZBURG")
      )
    
      shared_Data$map_Gemeinde <- isolate(predict_Q1$Gemeinde)
      shared_Data$map_Prediction <- isolate(predict_Q1$Prediction)

      ggplot(predict_Q1, aes(x = Gemeinde, y = Prediction, fill = Gemeinde, color = Gemeinde)) +
        geom_bar(stat = "identity") +
        labs(x = "Gemeinde", y = "The forecast quantities", title = "The forecast quantities for each part for Q1 2017 in ten cities")
        
    })
    
##------------------------------------------------------------------------------------------------------
    # Karte hier definieren
    output$map <- leaflet::renderLeaflet({
      part_Typ <- part_data()[!duplicated(part_data()$Gemeinde), ]

      map_Gemeinde_data <- shared_Data$map_Gemeinde
      map_Prediction_data <- shared_Data$map_Prediction
      
      

      map_data <- tibble(Gemeinde = map_Gemeinde_data, Prediction = map_Prediction_data, Typ_Einzelteil = part_Typ$Typ_Einzelteil)

      standort <- Final_Daten %>%
        filter(Fehlerhaft >= 1) %>%
        select(c("Gemeinde", "Laengengrad", "Breitengrad", "Postleitzahl"))

      standort_data <- standort[!duplicated(standort$Gemeinde), ]

      standort_map_data <- left_join(map_data, standort_data, by = "Gemeinde")
      
      standort_map_data$percent <- percent(standort_map_data$Prediction/sum(standort_map_data$Prediction), accuracy = 0.01)
      

      m_data %>%
        leaflet() %>%
        setView(
          lng = 10,
          lat = 51,
          zoom = 6
        ) %>% # centered to Germany map
        addTiles() %>%
        addHeatmap(
          lng = ~ as.numeric(Laengengrad),
          lat = ~ as.numeric(Breitengrad),
          intensity = ~Anzahl,
          blur = 35,
          max = 3500,
          radius = 30
        ) %>%
        addCircleMarkers(
          data = standort_map_data,
          lng = ~ as.numeric(Laengengrad),
          lat = ~ as.numeric(Breitengrad),
          popup = ~ paste(
            "<center><h5>Einzelteil</h5></center>",
            "Typ_Einzelteil: ",
            Typ_Einzelteil,
            "<br/>",
            "Number of recommendations: ",
            Prediction,
            "<br/>",
            "Percent: ",
            percent,
            "<br/>",
            "in: ",
            Postleitzahl, Gemeinde
          ),
          radius = 5,
          fillOpacity = 1
        )
    })
  })

  # Tabelle hier definieren
  output$data_table <- DT::renderDataTable({
    da_table <- Final_Daten %>%
      select(c("Typ_Einzelteil", "Fehlerhaft","Fehlerhaft_Datum","Gemeinde","Postleitzahl","Laengengrad","Breitengrad")) %>%
      datatable(
        filter = list(position = "top", clear = TRUE),
      )
    # datatable(
    #   Final_Daten[, c(
    #     "Typ_Einzelteil",
    #     "Fehlerhaft",
    #     "Fehlerhaft_Datum",
    #     "Gemeinde",
    #     "Postleitzahl",
    #     "Laengengrad",
    #     "Breitengrad"
    #   )],
    #   filter = list(position = "top", clear = TRUE),
    # )
  })
}

shinyApp(ui = ui, server = server)
