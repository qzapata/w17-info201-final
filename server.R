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
  
  # Creates filtered data frame as reactive variable
  filtered.data <- reactive ({
    persons.filtered <- 
      if (input$table.type == "Residence") {
        filter(persons.data, Country...territory.of.asylum.residence == input$country.choice)
      } else {
        filter(persons.data, Origin == input$country.choice) 
      }
    if (input$year.choice != "All") {
      persons.filtered <- filter(persons.filtered, Year == input$year.choice)
    }
    persons.filtered <- filter(persons.filtered, Population.type %in% input$type.of.displacement) %>%
                        arrange(desc(Value))
    if(nrow(persons.filtered) > input$row.num) {
      persons.filtered <- persons.filtered[0:input$row.num,]
    }
    return(persons.filtered)
  })
  
  # Creates table of the top countries where people are either going to or coming from.
  output$table <- renderTable({
    if(nrow(filtered.data()) == 0) {
      return()
    } else {
      table.data <- filtered.data()
      table.data$Value <- format(table.data$Value,big.mark=",",scientific=FALSE)
      colnames(table.data) <- c("Year", "Country of Residence", "Origin","Type of Displacement", "Number of People")
    return(table.data)
    }
  })
  
  # Message letitng the user know that there are no values found.
  output$text <- renderText({
    if(nrow(filtered.data()) == 0) {
      return("No data found for the active filters.")
    }
  })
})
