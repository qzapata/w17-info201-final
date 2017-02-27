# import libraries
library('shiny')
library('dplyr')
library('ggplot2')
library('maps')

# store data set
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv', stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  
})