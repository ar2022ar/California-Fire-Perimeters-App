# Title: California Fire Perimeters
# Description: a shiny app to visualize California Fire Perimeters 
# Author: Anushka R

# =======================================================
# Packages 
# =======================================================
library(shiny)
library(tidyverse)    # data wrangling and graphics
library(lubridate)    # for working with dates
library(sf)           # for working with geospatial vector-data
library(leaflet)      # web interactive maps
library(plotly)       # web interactive graphics


# =======================================================
# Import data
# =======================================================
cal_perims = st_read(dsn = "California_Fire_Perimeters_(1950+)") |>
  st_transform(crs = 4326)

cal_perims = cal_perims |>
  mutate(MONTH = month(ALARM_DATE),
         SEASON = case_when(
           MONTH %in% c(1, 2, 12) ~ "Winter",
           MONTH %in% 3:5 ~ "Spring", 
           MONTH %in% 6:8 ~ "Summer",
           MONTH %in% 9:11 ~ "Fall"
         ),
         COLOR = case_when(
           SEASON == "Winter" ~ "#4C69C5",
           SEASON == "Spring" ~ "#3ABEA8",
           SEASON == "Summer" ~ "#FF964E",
           SEASON == "Fall" ~ "#FFC84E"
         ),
         LABEL = paste0(FIRE_NAME, ", ", ALARM_DATE))

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("California Fire Perimeters"),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # input widgets of first tab
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Year Analysis"),
        # replace with your widgets
        numericInput(inputId = "widget1",
                    label = "Year (1950+)",
                    value = 2020),
        selectInput(
          "widget2",
          "Labels, Seasons, Markers?",
          choices = list("Label w/ Fire Name" = 1, "Label w/ Fire Name & Date" = 2,"Season" = 3, "Markers" = 4),
          selected = 1),
        selectInput(inputId = "tiles",
                    label = "Choose a tile",
                    choices = c("CartoDB.Positron",
                                "OpenStreetMap",
                                "Esri.WorldTerrain",
                                "Esri.WorldGrayCanvas",
                                "OpenTopoMap"),
                    selected = "CartoDB.Positron"),
        selectInput(inputId = "size",
                    label = "Only show big fires?",
                    choices = c("True", "False"),
                    selected = "True")
        
      ), # closes 1st panel
      
      # ---------------------------------------------
      # input widgets of second tab
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Table in selected year"),
        numericInput(inputId = "widget3",
                     label = "Year (1950+)",
                     value = 2020),
        numericInput(inputId = "widget4",
                     label = "1 is a filtered, desc arranged table",
                     value = 1),
      ), # closes 2nd panel
      
      # ---------------------------------------------
      # input widgets of third tab
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==3",
        h4("Summary of all fires"),
        selectInput(
          "widget5",
          "Choose a certain summary statistic?",
          choices = list("Total Number of Fires per Year" = 1, "Total Area Burned per Year" = 2, "Q1 of Area Burned per Year" = 3, "Median Area Burned per Year" = 4, "Mean Area Burned per Year" = 5, "Q3 of Area Burned per Year" = 6),
          selected = 1),
        textInput(inputId = "widget6",
                     label = "Color of Barplot",
                     value = "skyblue"),
      ) # closes 3rd panel
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 3 tabsets: 
    # tab1: map of fires in selected year
    # tab2: table of fires in selected year
    # tab3: summary statistics of all fires
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # first tab (map)
        tabPanel(title = "Map",
                 value = 1,
                 leafletOutput("map", height = 600)),
        # second tab (table)
        tabPanel(title = "Table",
                 value = 2,
                 dataTableOutput(outputId = "table")),
        # third tab (summary plot)
        tabPanel(title = "Summary",
                 value = 3,
                 plotlyOutput(outputId = "plot")),
        id = "tabselected"
        
      ) # closes tabsetPanel
    ) # closes mainPanel
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # -----------------------------------------------
  # Output for first TAB (i.e. map)
  # -----------------------------------------------
  output$map <- renderLeaflet({
    big_fires_selected_year = cal_perims |>
      filter(YEAR_ == input$widget1)
    if(input$size=="True"){
      big_fires_selected_year = big_fires_selected_year %>% filter(GIS_ACRES>5000)
    }
    if(input$widget2==1){
    leaflet(data = big_fires_selected_year) |>
      setView(lng = -119, lat = 37, zoom = 6) |>
      addTiles() |>
      addProviderTiles(providers[[input$tiles]]) |>
      addPolygons(color = "tomato",
                  opacity = 0.9,
                  weight = 1,
                  label = ~FIRE_NAME) 
    }
    else if (input$widget2==2){
    big_fires_selected_year_table = big_fires_selected_year
    big_fires_selected_year_table$geometry = NULL
    leaflet(data = big_fires_selected_year) |>
      setView(lng = -119, lat = 37, zoom = 6) |>
      addProviderTiles(providers[[input$tiles]]) |>
      addPolygons(color = "tomato",
                  opacity = 0.9,
                  weight = 1,
                  label = ~LABEL)
    }
    else if (input$widget2==3){
      big_fires_selected_year |>
        leaflet() |>
        setView(lng = -119, lat = 37, zoom = 6) |>
        addProviderTiles(providers[[input$tiles]]) |>
        addPolygons(color = ~COLOR,
                    opacity = 0.9,
                    weight = 1,
                    label = ~LABEL) |>
        addLegend(position = "bottomleft",
                  colors = c("#4C69C5",  "#3ABEA8", "#FF964E", "#FFC84E"),
                  labels = c("Winter", "Spring", "Summer", "Fall"),
                  title = "Season",
                  opacity = 1)
    }
    else if(input$widget2==4){
      big_fires_centroids = big_fires_selected_year |>
        st_transform(crs = 3857) |>
        st_centroid() |>
        st_transform(crs = 4326)
      leaflet(data = big_fires_selected_year) |>
        setView(lng = -119, lat = 37, zoom = 6) |>
        addProviderTiles(providers[[input$tiles]]) |>
        addPolygons(color = "tomato", opacity = 0.8) |>
        addMarkers(data = big_fires_centroids,
                   popup = ~LABEL)
    }
  })
  
  
  # -----------------------------------------------
  # Output for second TAB (i.e. table of fires)
  # -----------------------------------------------
  output$table <- renderDataTable({
    big_fires_selected_year = cal_perims |>
      filter(YEAR_ == input$widget3)
    big_fires_selected_year_table = big_fires_selected_year 
    big_fires_selected_year_table$geometry = NULL
    for (i in 1:length(big_fires_selected_year_table$CAUSE)) {
      if(big_fires_selected_year_table$CAUSE[i]==1){
        big_fires_selected_year_table$CAUSE[i]="Lightning"
      }
      if(big_fires_selected_year_table$CAUSE[i]==2){
        big_fires_selected_year_table$CAUSE[i]="Equipment Use"
      }
      if(big_fires_selected_year_table$CAUSE[i]==3){
        big_fires_selected_year_table$CAUSE[i]="Smoking"
      }
      if(big_fires_selected_year_table$CAUSE[i]==4){
        big_fires_selected_year_table$CAUSE[i]="Campfire"
      }
      if(big_fires_selected_year_table$CAUSE[i]==5){
        big_fires_selected_year_table$CAUSE[i]="Debris"
      }
      if(big_fires_selected_year_table$CAUSE[i]==6){
        big_fires_selected_year_table$CAUSE[i]="Railroad"
      }
      if(big_fires_selected_year_table$CAUSE[i]==7){
        big_fires_selected_year_table$CAUSE[i]="Arson"
      }
      if(big_fires_selected_year_table$CAUSE[i]==8){
        big_fires_selected_year_table$CAUSE[i]="Playing with fire"
      }
      if(big_fires_selected_year_table$CAUSE[i]==9){
        big_fires_selected_year_table$CAUSE[i]="Miscellaneous"
      }
      if(big_fires_selected_year_table$CAUSE[i]==10){
        big_fires_selected_year_table$CAUSE[i]="Vehicle"
      }
      if(big_fires_selected_year_table$CAUSE[i]==11){
        big_fires_selected_year_table$CAUSE[i]="Powerline"
      }
      if(big_fires_selected_year_table$CAUSE[i]==12){
        big_fires_selected_year_table$CAUSE[i]="Firefighter Training"
      }
      if(big_fires_selected_year_table$CAUSE[i]==13){
        big_fires_selected_year_table$CAUSE[i]="Non-Firefighter Training"
      }
      if(big_fires_selected_year_table$CAUSE[i]==14){
        big_fires_selected_year_table$CAUSE[i]="Unknown/Unidentified"
      }
      if(big_fires_selected_year_table$CAUSE[i]==15){
        big_fires_selected_year_table$CAUSE[i]="Structure"
      }
      if(big_fires_selected_year_table$CAUSE[i]==16){
        big_fires_selected_year_table$CAUSE[i]="Aircraft"
      }
      if(big_fires_selected_year_table$CAUSE[i]==17){
        big_fires_selected_year_table$CAUSE[i]="Volcanic"
      }
      if(big_fires_selected_year_table$CAUSE[i]==18){
        big_fires_selected_year_table$CAUSE[i]="Escaped Prescribed Burn"
      }
      if(big_fires_selected_year_table$CAUSE[i]==19){
        big_fires_selected_year_table$CAUSE[i]="Illegal Alien Campfire"
      }
    }
    if(input$widget4==1){
      big_fires_selected_year_table = big_fires_selected_year_table %>% select(FIRE_NAME,GIS_ACRES,ALARM_DATE,CONT_DATE,CAUSE)
      big_fires_selected_year_table <- big_fires_selected_year_table %>% arrange(desc(GIS_ACRES))
    }
    else{ #arranged in ascending order, and displays ALL data. 
      big_fires_selected_year_table = big_fires_selected_year_table
      big_fires_selected_year_table <- big_fires_selected_year_table  %>% arrange(GIS_ACRES)
    }
    

    big_fires_selected_year_table
  })
  
  
  # ------------------------------------------------
  # Output for third TAB (i.e. summary plot)
  # ------------------------------------------------
  output$plot <- renderPlotly({
    cal_perims_summary = cal_perims
    cal_perims_summary$geometry = NULL
    if (input$widget5 ==1) {
      c = cal_perims_summary %>% select(YEAR_) %>% group_by(YEAR_) %>%count(YEAR_)
      c |>
        ggplot() +
        geom_col(aes(x = YEAR_, y = n), fill = input$widget6) +
        labs(title = "Total Number of Fires per Year",
             y = "Total Number of Fires", x="Year") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))
    }
    else if (input$widget5 ==2){
      d = cal_perims_summary %>% select(YEAR_, GIS_ACRES) %>% group_by(YEAR_) %>%summarize(sum = sum(GIS_ACRES, na.rm=TRUE))
      d |>
        ggplot() +
        geom_col(aes(x = YEAR_, y = sum), fill = input$widget6) +
        labs(title = "Total Area Burned per Year",
             y = "Total Area Burned", x="Year") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))
    }
    else if (input$widget5 ==3){ 
      f = cal_perims_summary %>% select(YEAR_, GIS_ACRES) %>% group_by(YEAR_) %>%summarize(q = quantile(GIS_ACRES,prob=c(.25), type=1, na.rm=TRUE))
      f |>
        ggplot() +
        geom_col(aes(x = YEAR_, y = q), fill = input$widget6) +
        labs(title = "First Quartile (Q1) of Area Burned per Year", y = "First Quartile (Q1) of Area Burned", x="Year") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))
    }
    else if (input$widget5 ==4){
      e = cal_perims_summary %>% select(YEAR_, GIS_ACRES) %>% group_by(YEAR_) %>%summarize(median = median(GIS_ACRES, na.rm=TRUE))
      e |>
        ggplot() +
        geom_col(aes(x = YEAR_, y = median), fill = input$widget6) +
        labs(title = "Median Area Burned per Year",
             y = "Median Area Burned", x="Year") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))
    }
    else if (input$widget5 ==5){
      f = cal_perims_summary %>% select(YEAR_, GIS_ACRES) %>% group_by(YEAR_) %>%summarize(mean = mean(GIS_ACRES, na.rm=TRUE))
      f |>
        ggplot() +
        geom_col(aes(x = YEAR_, y = mean), fill = input$widget6) +
        labs(title = "Mean Area Burned per Year",
             y = "Mean Area Burned", x="Year") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))
    }
    else if (input$widget5 ==6) { 
      f = cal_perims_summary %>% select(YEAR_, GIS_ACRES) %>% group_by(YEAR_) %>%summarize(q = quantile(GIS_ACRES,prob=c(.75), type=1, na.rm=TRUE))
      f |>
        ggplot() +
        geom_col(aes(x = YEAR_, y = q), fill = input$widget6) +
         labs(title = "Third Quartile (Q3) of Area Burned per Year", y = "Third Quartile (Q3) of Area Burned", x="Year") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))
    }
    
  })
  
} # closes server


# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
