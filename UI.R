library(shiny)
library(datasets)

ui <- shinyUI(fluidPage(
  titlePanel("Sentiment Analysis DashBoard"),
    tabsetPanel(
    tabPanel("Sentiment Bar Graph",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv'))
                 
                 
                 
               ),
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    ),
    tabPanel("Word Cloud",
             
               mainPanel(
                 plotOutput('Word',width = "100%")
               )
          
    )
    
    )
)
)