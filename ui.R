# import libraries
library('shiny')


# import data set
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv')

# variables to use
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
        condition='input.tabPanel=="Table"',
        selectInput('country.choice', label='Country', choice=c(country.names), selected=country.names[1])),
        selectInput('year.choice', label='Year', choice=c("All",rev(years))),
        checkboxGroupInput('type.of.displacement', label='Type of Displacement', choice=pop.types, 
                         selected=pop.types),
        radioButtons("table.type", "Table Information:", c("Origin","Residence")),
        numericInput('row.num', label = 'Max Rows', value = 10, min = 0)
      
      )
    ,
    
    # creates main panel for data
  mainPanel(
      tabsetPanel(type='tabs', id = 'tabPanel',
                  tabPanel('Table',
                      p(" The following table includes information about where displaced persons are going and where they are coming from, arranged descendingly
                        according to the number of people displaced:"),
                      textOutput('text'),
                      tableOutput('table')
                  )
                  
      )
    )
  )
))