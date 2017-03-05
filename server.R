# import libraries
library('shiny')
library('dplyr')
library('ggplot2')
library('maps')
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
    if(input$type == 'color.map.plot') {
      validate(need(input$type.of.displacement, 'Please choose at least one kind of displacement'))
      return(color.map.plot())
    }
    return(line.map.plot())
  })
  
  # filters data for map to use
  map.filter <- reactive({
    data <- persons.data %>% 
      filter(Year == input$year.choice) %>% 
      filter(Population.type %in% input$type.of.displacement) %>% 
      mutate(ISO3.residence = iso.alpha(.$Country...territory.of.asylum.residence, n=3),
             ISO3.origin = iso.alpha(.$Origin, n=3)) 
    
    if (input$type == 'line.map.plot' & input$country.choice != 'All') {
      if (input$direction.choice == 'ISO3.origin'){
        data <- filter(data, Country...territory.of.asylum.residence == input$country.choice)
      } else {
        data <- filter(data, Origin == input$country.choice)
      }
    }
    return(data)
  })

  # creates click event data text 
  output$click.text <- renderPrint({
    cat('Country data:', '\n')
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a country to view intake breakdown" else d
  })
  
  # creates color map view
  color.map.plot <- reactive({
    # filter data for graph
    world.map <- map.filter() %>%
      group_by(ISO3.residence) %>% 
      summarise(origin.all=paste0(Origin, ': ', Value, collapse="\n"), total=sum(Value)) %>%  
      left_join(world, by=c('ISO3.residence'='ISO3'))
      
    # specify map projection/options
    g <- list(showframe = TRUE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator'))
      
    map <- plot_geo(world.map) %>% 
      config(displayModeBar=FALSE) %>% 
      add_trace(z=~total, color=~total, colors='Blues', text=~total, locations=~ISO3.residence, marker=list(line=1)) %>% 
      colorbar(title='Displaced People') %>% 
      layout(title=paste('Displaced persons', input$year.choice), geo=g)
    
    return(map)
  })
  
  # creates line map view
  line.map.plot <- reactive({
    # modify data set
    world.map <- map.filter() %>% 
      group_by_(input$direction.choice) %>% 
      summarise(sum = sum(Value)) %>% 
      left_join(map.filter())
    
    # create two data frames to get both x and y coords
    world.map.residence <- world.map %>% 
      left_join(world, by=c('ISO3.residence'='ISO3'))
    
    world.map.origin <- map.filter() %>% 
      left_join(world, by=c('ISO3.origin'='ISO3')) %>% 
      select(ISO3.residence, ISO3.origin, NAME, LAT, LON)
    
    # final join and get data for only other countries
    world.map <- left_join(world.map.residence, world.map.origin, 
                           by=c('ISO3.residence'='ISO3.residence', 'ISO3.origin'='ISO3.origin')) %>% 
      filter(ISO3.origin != ISO3.residence) %>% 
      mutate(id = seq_len(nrow(.)))
    
    # specify map projection/options
    g <- list(showframe = TRUE,
              showcoastlines = TRUE,
              projection = list(type = 'Mercator'))
    
    map <- plot_geo() %>% 
      config(displayModeBar=FALSE) %>% 
      layout(title=paste('Displaced persons', input$year.choice), geo=g, showlegend=FALSE) %>% 
      add_segments(data=group_by(world.map, id), 
                   x=~LON.y, xend=~LON.x,
                   y=~LAT.y, yend=~LAT.x,
                   text=~Origin, type='scattermapbox',
                   size=I(1), hoverinfo='none', color=~Origin,
                   alpha=ifelse(input$direction.choice == 'ISO3.residence', 0.3, 1))
    
    if (input$direction.choice == 'ISO3.residence') {
      map <- map %>% 
        add_markers(data=world.map, x=~LON.x, y=~LAT.x, text=~NAME.x,
                    size=~Value, hoverinfo='text', alpha=0.5)
    }
    
    return(map)  
  })
})