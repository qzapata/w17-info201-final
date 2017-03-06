# import libraries
library('shiny')
library('plotly')

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
      conditionalPanel(
        condition='input.tabPanel=="Map"',
        # universal panels
        selectInput('year.choice', label='Year', choice=rev(years)),
        checkboxGroupInput('type.of.displacement', label='Type of Displacement', choice=pop.types, 
                           selected=pop.types),
        conditionalPanel(
          condition='input.type == "color.map.plot"',
          selectInput('direction.choice.color', label='Purpose', 
                      choice=list('Fleeing'='ISO3.origin', 'Residing'='ISO3.residence'))),
                       
        # line plot panels
        conditionalPanel(
        condition='input.type == "line.map.plot"',
        selectInput('direction.choice.line', label='Purpose', 
                    choice=list('Fleeing'='ISO3.residence', 'Residing'='ISO3.origin')),
        selectInput('country.choice', label='Country', choice=c('All', country.names), selected=country.names[1])),
                       
        # filter plot
        selectInput('type', label='Map Type', choice=list('Color'='color.map.plot',
                                                          'Line'='line.map.plot'))
      )
    ),
    
    # creates main panel for data
    mainPanel(
      tabsetPanel(type='tabs', id='tabPanel',
            tabPanel('Map',
                     plotlyOutput('map.plot'),
                     textOutput('map.description'),
                     conditionalPanel(condition='input.type == "color.map.plot"',
                                      verbatimTextOutput('click.text')))
      )
    )
  )
))