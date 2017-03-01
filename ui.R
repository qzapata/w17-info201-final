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
                  
      )
    )
  )
))