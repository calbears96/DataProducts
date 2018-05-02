#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(scales)
library(quantmod)
library(dplyr)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
    # Application title
    titlePanel("Stock Price"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('Company', label=h4('Stock Symbol'),
                        choices = list('Apple' = 'AAPL', 'Google' = 'GOOG', 
                                       'Microsoft' = 'MSFT', 'Amazon' = 'AMZN',
                                       'Facebook' = 'FB')),

            dateRangeInput('DateRange', 'Date range input:',
                           min = '2007-01-03',
                           max = '2018-05-01',
                           start = '2007-01-03',
                           end = '2018-05-01'),
            checkboxInput('AddTrend', 'Add Trend Line'),
            checkboxInput('Returns', 'Add daily return graph'),
            checkboxInput('Volume', 'Add daily volume graph' )
                          
        
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("StockPlot"),
            h3('Smoothing model equation: '),
            h3(textOutput('model'))
        )
    )
))
