# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>% 
    select(CITY_ASCII, 
           LNG, 
           LAT, 
           BIKE_PREDICTION,
           BIKE_PREDICTION_LEVEL,
           LABEL,
           DETAILED_LABEL,
           FORECASTDATETIME) %>%
    group_by(CITY_ASCII) %>% mutate(MAX_BIKE_PREDICTION = max(BIKE_PREDICTION))
  
  
  
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
      #Render the city overview map
      leafletProxy("city_bike_map") %>% clearMarkers()
      index = which(cities_max_bike$CITY_ASCII == input$city_dropdown)
      leafletProxy("city_bike_map") %>% addCircles(lng = cities_max_bike$LNG[index], 
                                                   lat = cities_max_bike$LAT[index], 
                                                   popup = cities_max_bike$DETAILED_LABEL[index])
      
      output$temp_line <- renderPlot({
        ggplot(subset(city_weather_bike_df, CITY_ASCII == input$city_dropdown),
               aes(x = FORECASTDATETIME, 
                   y = TEMPERATURE, 
                   label = TEMPERATURE,
                   group = 1)) + 
          geom_line(color = "yellow", size = 1.5) + 
          geom_point() + 
          geom_text() +
          theme(axis.text.x=element_blank()
                )
      })
      
      output$bike_line <- renderPlot({
        ggplot(subset(city_weather_bike_df, CITY_ASCII == input$city_dropdown),
               aes(x = FORECASTDATETIME, 
                   y = BIKE_PREDICTION, 
                   label = TEMPERATURE,
                   group = 1)) + 
          geom_line(color = "light blue", 
                    size = 1.5, 
                    linetype = "dashed") + 
          geom_point() + 
          geom_text() +
          theme(axis.text.x=element_blank())
      })
      
      output$bike_date_output <- renderText({
        paste("Time = ", input$plot_click$x, "\nBikeCountPred = ", input$plot_click$y)
      })
      
      output$humidity_pred_chart <- renderPlot({
        ggplot(subset(city_weather_bike_df, CITY_ASCII == input$city_dropdown),
               aes(x = HUMIDITY, 
                   y = BIKE_PREDICTION)) + 
          geom_point() + 
          geom_smooth(method = lm, 
                      formula = y ~ poly(x, 4))
        
      })
      
      
    }
    else {
      #Render the specific city map
      leafletProxy("city_bike_map")
    } 
  })
  
  
  
  
  # Then render output plots with an id defined in ui.R
  output$city_bike_map <- renderLeaflet({
    # Complete this function to render a leaflet map
    leaflet(data=cities_max_bike) %>%
      addTiles() %>%
      addCircleMarkers(data = cities_max_bike, 
                       lng = ~LNG, 
                       lat = ~LAT, 
                       popup = ~LABEL, 
                       color = color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL), 
                       radius = if(cities_max_bike$BIKE_PREDICTION_LEVEL == "small") {
                         6} else {
                           if(cities_max_bike$BIKE_PREDICTION_LEVEL == "medium") {
                             10} else {12
                             }
                         }
      )
    
  })
  
  
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  
})
