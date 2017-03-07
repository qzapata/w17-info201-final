# import libraries
library('shiny')
library('dplyr')
library('ggplot2')
library('maps')
library('plotly')
library('dygraphs')

# store data set
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv', stringsAsFactors = FALSE)

# removes 2014 and convert value col to numeric
persons.data <- filter(persons.data, Year != '2014')
persons.data$Value <- sapply(persons.data$Value, as.numeric)


shinyServer(function(input, output) {
  
  # create filtered data for line graph
  bar.filter <- reactive({
    data <- persons.data %>% 
      filter(Population.type %in% input$type.of.displacement) %>% 
      mutate(ISO3.residence = iso.alpha(.$Country...territory.of.asylum.residence, n=3),
             ISO3.origin = iso.alpha(.$Origin, n=3)) 
    
      if (input$direction.choice == 'ISO3.origin'){
        data <- filter(data, Country...territory.of.asylum.residence == input$country.choice)
        data <- select(data, Year, Value)
        } else {
        data <- filter(data, Origin == input$country.choice)
        data <- select(data, Year, Value)
      }

    return(data)
  })
  
  # Generate Dygraph for selected country
  output$dygraph <- renderDygraph({
        dygraph(bar.filter(), main = paste("Displacement Graph for", input$country.choice)) %>%
          dySeries("Value", label = "Population") %>%
          dyRangeSelector() %>%
          dyOptions(stepPlot = TRUE)
  })
    
})
    
  

