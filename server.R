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
  #data frame to print country breakdown
  breakdown <- reactiveValues()
  breakdown$df <- ''
  
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
      if (input$direction.choice.line == 'ISO3.origin') {
        data <- filter(data, Country...territory.of.asylum.residence == input$country.choice)
      } else {
        data <- filter(data, Origin == input$country.choice)
      }
    }
    return(data)
  })

  # creates click event data text 
  output$click.text <- renderPrint({
    # data for clicked country
    d <- event_data("plotly_click")
    
    # prints usability or selected data
    if (is.null(d)) {
      cat("Click on a country to view country breakdown")
    } else {
      selected <- breakdown$df[d$pointNumber+1,]
      cat(ifelse(input$direction.choice.color == 'ISO3.residence','Fleeing  ', 'Residing '), 
          'data for ', levels(selected$NAME)[selected$NAME],':\n', selected$origin.all, sep="")
    }
  })
  
  # creates color map view
  color.map.plot <- reactive({
    # filter data for graph
    world.map <- map.filter() %>%
      group_by_(input$direction.choice.color) %>% 
      summarise(origin.all=ifelse(input$direction.choice.color == 'ISO3.residence',
                                  paste0(Origin, ': ', Value, collapse="\n"),
                                  paste0(Country...territory.of.asylum.residence, ': ', Value, collapse="\n")), 
                total=sum(Value)) 
    
    # renames columns for join
    colnames(world.map)[1] <- 'ISO3'
    
    # join and filter areas too small to plot 
    world.map <- left_join(world.map, world) %>% 
      filter(AREA > 100)
    
    # stores data frame to reactive var
    breakdown$df <- world.map
    
    # specify map projection/options
    g <- list(showframe = TRUE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator'))
      
    map <- plot_geo(world.map) %>% 
      config(displayModeBar=FALSE) %>% 
      add_trace(z=~total, color=~total, colors='Blues', text=~NAME, 
                locations=~ISO3, marker=list(line=1)) %>% 
      colorbar(title='Displaced People') %>% 
      layout(title=paste('Displaced persons', input$year.choice), geo=g)
    
    return(map)
  })
  
  # creates line map view
  line.map.plot <- reactive({
    # modify data set
    world.map <- map.filter() %>% 
      group_by_(input$direction.choice.line) %>% 
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
    
    # stores world.data in reactive variable
    breakdown$df <- world.map
    
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
                   text=~Origin, split=~id,
                   size=I(1), hoverinfo='text', color=~Origin,
                   alpha=ifelse(input$direction.choice.line == 'ISO3.residence', 0.3, 1))
    
    if (input$direction.choice.line == 'ISO3.residence') {
      map <- map %>% 
        add_markers(data=world.map, x=~LON.x, y=~LAT.x, text=~NAME.x,
                    size=~Value, hoverinfo='text', alpha=0.5)
    }

    return(map)  
  })
  
  output$map.description <- renderPrint({
    # var to see what type of direction should choose from
    type <- ifelse(input$type=='color.map.plot', TRUE, FALSE)
    direc <- ifelse(type,
           ifelse(input$direction.choice.color=='ISO3.residence', 
                  TRUE, FALSE),
           ifelse(input$direction.choice.line=='ISO3.residence', 
                  FALSE, TRUE))
    
    # stat variables for data based on line or color view
    View(breakdown$df)
    if (type) {
      country.max.var <- breakdown$df[which(breakdown$df$total == max(breakdown$df$total)),]$NAME
      country.max <- levels(country.max.var)[country.max.var]
      country.min.var <- breakdown$df[which(breakdown$df$total == min(breakdown$df$total)),]$NAME
      country.min <- levels(country.min.var)[country.min.var]
    } else {
      country.max.var <- breakdown$df[which(breakdown$df$Value == max(breakdown$df$Value)),]
      country.max <- ifelse(direc,
                            levels(country.max.var$NAME.y)[country.max.var$NAME.y],
                            levels(country.max.var$NAME.x)[country.max.var$NAME.x])
      country.min.var <- breakdown$df[which(breakdown$df$Value == min(breakdown$df$Value)),]
      country.min <- ifelse(direc,
                            levels(country.min.var$NAME.y)[country.min.var$NAME.y],
                            levels(country.min.var$NAME.x)[country.min.var$NAME.x])
      
      country.max.status <- tolower(country.max.var$Population.type)
      country.min.status <- tolower(country.min.var$Population.type)
    }
    
    # print statement
    cat('The map above is a ', ifelse(type, 'choropleth ', 'line '), 'map depicting data on displaced
        people, and what country they ', ifelse(direc, 'reside in', 'fled from'), ' for the year ', input$year.choice,
        '. ',
        ifelse(!type, paste('The map shows data for people ', ifelse(direc, 'from ', 'in '), input$country.choice,
                            '. '), ''),
        'Most displaced people ', ifelse(direc, 'flee from ', 'flee to '), country.max, 
        ifelse(type, '', paste0(' with the status ', country.max.status)), ', and the least amount of people ', 
        ifelse(direc, 'flee from ', 'flee to '), country.min, 
        ifelse(type, '', paste0(' with the the status ', country.min.status)), '.', sep="")
  })
})