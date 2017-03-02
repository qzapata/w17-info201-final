# import libraries
library('shiny')
library('dplyr')
library('ggplot2')
library('maps')

# store data set
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv', stringsAsFactors = FALSE)

# removes 2014 and convert value col to numeric
persons.data <- filter(persons.data, Year != '2014')
persons.data$Value <- sapply(persons.data$Value, as.numeric)

shinyServer(function(input, output) {
  # filters data for map to use
  map.filter <- reactive({
    data <- persons.data %>% 
      filter(Year == input$year.choice) %>% 
      filter(Population.type %in% input$type.of.displacement) %>% 
      mutate(ISO3 = iso.alpha(.[[input$purpose.choice]], n=3))
    View(data)
    return(data)
  })
  
  # generates map based on user input
  output$map.plot <- renderPlot({
    world.map <- map_data('world') %>% 
      mutate(ISO3 = iso.alpha(.$region, n = 3)) %>% 
      left_join(map.filter())
    
    # create map
    map <- ggplot(data=world.map) +
      geom_polygon(mapping=aes(x=long, y=lat, group=group, fill=cut(Value, 9)),
                   color='black',
                   size=0.25) +
      coord_quickmap() +
      labs(title=paste('Displacement of Persons in', input$year.choice), 
           fill='Displaced Population', 
           y='Latitude',
           x='Longitude') +
      scale_fill_brewer(type = 'seq', palette = 2, direction = 1)
    
    return(map)
  })
})