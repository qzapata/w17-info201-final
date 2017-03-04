# import libraries
library('shiny')
library('dplyr')
library('ggplot2')
library('maps')

# test libraries-------
library('plotly')
library('maptools')

# store data set
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv', stringsAsFactors = FALSE)
data("wrld_simpl")
world <- data.frame(wrld_simpl)

# removes 2014 and convert value col to numeric
persons.data <- filter(persons.data, Year != '2014')
persons.data$Value <- sapply(persons.data$Value, as.numeric)

shinyServer(function(input, output) {
  # renders the plot the user requests
  output$map.plot <- renderPlotly({
    print(input$type)
    if(input$type == 'color.map.plot') {
      validate(need(input$type.of.displacement, 'Please choose at least one kind of displacement'))
      return(color.map.plot())
    }
    return(NULL)
  })
  
  # filters data for map to use
  map.filter <- reactive({
    data <- persons.data %>% 
      filter(Year == input$year.choice) %>% 
      filter(Population.type %in% input$type.of.displacement) %>% 
      mutate(ISO3 = iso.alpha(.$Country...territory.of.asylum.residence, n=3))
    return(data)
  })
  
  # generates color map
  color.map.plot <- reactive({
    world.map <- map.filter() %>%
      group_by(ISO3) %>% 
      summarise(total=sum(Value), origin.all=paste0(Origin, ': ', Value, collapse="<br>")) %>%  
      left_join(world)
    
    # add hover data
    world.map$hover <- with(world.map, paste(NAME, '<br>', 'Total displaced people in residence:', total))
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
      
    # specify map projection/options
    g <- list(showframe = TRUE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator'))
      
    map <- plot_geo(world.map) %>% 
      config(displayModeBar=FALSE) %>% 
      add_trace(z=~total, color=~total, colors='Blues', text=~hover, locations=~ISO3, marker=list(line=1)) %>% 
      layout(title=paste('Displaced persons', input$year.choice), geo=g)
    return(map)
  })
})