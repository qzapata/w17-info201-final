# import libraries
library('shiny')

# imports data set
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv')

# varaibles to speed up load time
years <- 1951:2013
pop.types <- levels(persons.data$Population.type)
country.names <- levels(persons.data$Country...territory.of.asylum.residence)

# main ui
shinyUI(fluidPage(
  # creates title and page title
  titlePanel(strong('Team Kiwi'), windowTitle='Team Kiwi'),
  
  # creates multi-column layout
  sidebarLayout(
    sidebarPanel(
      selectInput('year.choice', label='Year', choice=rev(years)),
      selectInput('purpose.choice', label='Relation', choice=list('Taking in'='Country...territory.of.asylum.residence',
                                                                  'Fleeing'='Origin')),
      checkboxGroupInput('type.of.displacement', label='Type of Displacement', choice=pop.types, 
                         selected=pop.types),
      selectInput('map.type', label='Map Type', choice=list('Color'='color.map.plot', 
                                                            'Line'='line.map.plot')),       
      selectInput('country.choice', label='Country', choice=c('All', country.names))
    ),
    
    # creates main panel for data
    mainPanel(
      tabsetPanel(type='tabs',
            tabPanel('Map',
                     plotOutput('map.plot'))
                     #textOutput('map.text'))      
      )
    )
  )
))