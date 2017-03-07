# import libraries
library('shiny')
library('plotly')

# import data
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv')

# Variables for tab
pop.types <- levels(persons.data$Population.type)
country.names <- levels(persons.data$Country...territory.of.asylum.residence)

# main ui
shinyUI(fluidPage(
  # creates title and page title
  titlePanel(strong('Team Kiwi'), windowTitle='Team Kiwi'),
  
  # creates multi-column layout
  sidebarLayout(
    sidebarPanel(
      # universal panels
 #     selectInput('year.choice', label='Year', choice=rev(years)),
      checkboxGroupInput('type.of.displacement', label='Type of Displacement', choice=pop.types,
                         selected=pop.types[1]),
      selectInput('direction.choice', label='Purpose', 
                  choice=list('Fleeing'='ISO3.residence', 'Accepting'='ISO3.origin')),
      selectInput('country.choice', label='Country', 
                  choice=c('All', country.names), selected=country.names[1]),
      hr(),
      helpText("Data from UNHCR")
    ),
    
    # creates main panel for data
    mainPanel(
      tabsetPanel(type='tabs',
                  tabPanel('Line Graph', hr(), dygraphOutput("dygraph"))
    )    
    )
 
  )))