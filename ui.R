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
                                  br("Using the FEATURES HERE, users will be able to better understand several things about immigration. They will be able to find out where people are going around the world, where they are coming from, 
                                    immigration trends over time, and many other questions they might have while exploring the data. ")
      )
    )
  )
))