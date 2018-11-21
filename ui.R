#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Highway Traffic Status"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("inTrafic","Select Highway",choices = plusTrafic$HighwayName)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("highwayTrafficStatus")
    )
  )
))
