# import libraries
library('shiny')
library('plotly')
library('dygraphs')
library('shinythemes')

# imports data set
persons.data <- read.csv('data/unhcr_popstats_export_time_series_all_data.csv')

# variables to use -- some years excluded because too little data available
years <- 1960:2013
pop.types <- levels(persons.data$Population.type)
country.names <- levels(persons.data$Country...territory.of.asylum.residence)

# predefines common filters
country.filter <- function(id) {selectInput(id, label='Country', 
                              choice=c(country.names), selected=country.names[1])
                  }
displacement.filter <- function(id) {checkboxGroupInput(id, label='Type of Displacement', 
                                          choice=pop.types, selected=pop.types)
                       }
direction.filter <-  function(id) {radioButtons(id, "Purpose", 
                                  choices=list('Fleeing'='ISO3.residence', 'Residing'='ISO3.origin'),
                                  selected='Fleeing')
                     }

# main ui
shinyUI(fluidPage(theme=shinytheme("flatly"),
  # creates title and page title
  titlePanel(strong('UNHCR Displaced Persons Data'), windowTitle='UNHCR Displaced Persons Data'),
  h4('Team Kiwi'),
  
  # creates multi-column layout
  sidebarLayout(
    sidebarPanel(
      # shows help information for introduction entry users
      conditionalPanel(
        condition='input.tabPanel=="Introduction"',
        helpText("Click on a tab to view specified visualization!")
      ),
      
      # shows interactive fitlers for map
      conditionalPanel(
        condition='input.tabPanel=="Map"',
        # universal panels
        selectInput('year.choice', label='Year', choice=rev(years)),
        displacement.filter("type.of.displacement"),
        conditionalPanel(
          condition='input.type == "color.map.plot"',
          radioButtons("direction.choice.color", "Purpose", 
                       choices=c('Fleeing'='ISO3.origin', 'Residing'='ISO3.residence'),
                       selected='Fleeing')),
                       
        # line plot panels
        conditionalPanel(
          condition='input.type == "line.map.plot"',
          direction.filter("direction.choice"),
          selectInput("country.choice", label='Country', choice=c('All', country.names), selected=country.names[1])
          ),
                       
        # filter plot
        selectInput('type', label='Map Type', choice=list('Color'='color.map.plot',
                                                          'Line'='line.map.plot')),
        
        # displays where information came from
        helpText("Data from UNHCR")
      ),
      
      # shows interactive fitlers for graph
      conditionalPanel(
        condition='input.tabPanel == "Graph"',
        displacement.filter("displacement.graph"),
        direction.filter("direction.graph"),
        country.filter("country.graph"),
        hr(),
        
        # displays where information came from
        helpText("Data from UNHCR")
      ),
      
      # shows interactive fitlers for table
      conditionalPanel(
        condition='input.tabPanel=="Table"',
        country.filter("country.table"),
        selectInput('year.table', label='Year', choice=c("All",rev(years))),
        displacement.filter("displacement.table"),
        radioButtons("table.type", "Table Information:", c("Origin","Residence")),
        numericInput('row.num', label = 'Max Rows', value = 10, min = 0),
        
        # displays where information came from
        helpText("Data from UNHCR")
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
                                     on about 200 countries both as destinations and origins for displaced people. "),
                                  br(strong("Types of Displacement Glossary")),
                                  p("Asylum-seekers: Someone whose request for sanctuary has yet to be processed."),
                                  p("Internally displaced persons: People who have not crossed a border to find safety."),
                                  p("Refugees (incl. refugee-like situations): People fleeing conflict or persecution."),
                                  p("Returned IDPs: Internally displaced persons who return to their areas of origin with the protection of UNHCR and assistance activities."),
                                  p("Returnees: Displaced people returning home after time in exile."),
                                  p("Stateless: People who are denied a nationality."),
                                  p("Others of concern: Individuals who do not fall under any of the other groups, fall under UNHCR protection.")
                ),
        tabPanel('Map',
                 plotlyOutput('map.plot'),
                 textOutput('map.description'),
                 conditionalPanel(condition='input.type == "color.map.plot"',
                                  verbatimTextOutput('click.text'))),
        tabPanel('Table',
                 p(" The following table includes information about where displaced persons are going and where they are coming from, arranged descendingly
                   according to the number of people displaced:"),
                 textOutput('text'),
                 tableOutput('table')),
        tabPanel('Graph', 
                 hr(),
                 p("The following line graph is about displacement population for type or types of displacement, 
                 purpose, specific country and time range."),
                 hr(), 
                 dygraphOutput("dygraph"))
      )
    )
  )
))
