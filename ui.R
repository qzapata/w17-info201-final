# import libraries
library('shiny')

# main ui
shinyUI(fluidPage(
  # creates title and page title
  titlePanel(strong('Team Kiwi'), windowTitle='Team Kiwi'),
  
  # creates multi-column layout
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    # creates main panel for data
    mainPanel(
      tabsetPanel(type='tabs',
        tabPanel("Introduction"), br("One of the largest policy issues in international politics today is immigration. Throughout history, people have been displaced across the globe, all for a variety of reasons. 
                                     To better understand this issue, one can look at the number of displaced people leaving countries all around the world, however, this data (which contains many levels of displacement information for 200 countries)
                                     can be quite difficult to observe and interpret. Our goal is to make this data more presentable and easy to digest."),
                                  br("This application contains three main ways to view the immigration data: a map to visually show displacement, a table to show the largest displacement events, and 
                                    a graph to track displacement over time. Through the use of these tools, users will be able to better understand several things about immigration. They will be able to find out where people are going around the world, where they are coming from, 
                                    immigration trends over time, and many other questions they might have while exploring the data. "),
                                  br("The data used in this application is from the United Nations Refugee Agency. UNHCR originally collected this data while helping on
                                     refugees, asylum-seekers, and people who are displaced since 1951. The data collects data based
                                     on about 200 countries both as destinations and origins for displaced people. ")
      )
    )
  )
))