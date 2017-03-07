# import libraries
library('shiny')
library('plotly')
library('dygraphs')

# import data
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv')

# Variables for tab
pop.types <- levels(persons.data$Population.type)
country.names <- levels(persons.data$Country...territory.of.asylum.residence)


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
        condition='input.tabPanel == "Graph"',
        checkboxGroupInput('type.of.displacement', label='Type of Displacement', choice=pop.types,
                           selected=pop.types[1]),
        selectInput('direction.choice', label='Purpose', 
                    choice=list('Fleeing'='ISO3.residence', 'Accepting'='ISO3.origin')),
        selectInput('country.choice', label='Country', 
                    choice=c('All', country.names), selected=country.names[1]),
        hr(),
        helpText("Data from UNHCR")),
      conditionalPanel(
        condition='input.tabPanel=="Table"',
        selectInput('country.choice', label='Country', choice=c(country.names), selected=country.names[1]),
        selectInput('year.choice', label='Year', choice=c("All",rev(years))),
        checkboxGroupInput('type.of.displacement', label='Type of Displacement', choice=pop.types, 
                         selected=pop.types),
        radioButtons("table.type", "Table Information:", c("Origin","Residence")),
        numericInput('row.num', label = 'Max Rows', value = 10, min = 0)
      )
    ),
    
    # creates main panel for data
    mainPanel(
      tabsetPanel(type='tabs',id='tabPanel',
        tabPanel("Introduction", br("One of the largest policy issues in international politics today is immigration. Throughout history, people have been displaced across the globe, all for a variety of reasons. 
                                     To better understand this issue, one can look at the number of displaced people leaving countries all around the world, however, this data (which contains many levels of displacement information for 200 countries)
                                     can be quite difficult to observe and interpret. Our goal is to make this data more presentable and easy to digest."),
                                  br("This application contains three main ways to view the immigration data: a map to visually show displacement, a table to show the largest displacement events, and 
                                    a graph to track displacement over time. Through the use of these tools, users will be able to better understand several things about immigration. They will be able to find out where people are going around the world, where they are coming from, 
                                    immigration trends over time, and many other questions they might have while exploring the data. "),
                                  br("The data used in this application is from the United Nations Refugee Agency. UNHCR originally collected this data while helping on
                                     refugees, asylum-seekers, and people who are displaced since 1951. The data collects data based
                                     on about 200 countries both as destinations and origins for displaced people. ")
       ) ,
      tabPanel('Table',
               p(" The following table includes information about where displaced persons are going and where they are coming from, arranged descendingly
                 according to the number of people displaced:"),
               textOutput('text'),
               tableOutput('table')),
      tabPanel('Graph', 
               hr(), 
               dygraphOutput("dygraph"))
      )
    )
  )
))