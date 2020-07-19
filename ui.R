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
shinyUI(
    
    navbarPage(
        title = 'AFL Visualisations',
        
        tabPanel('Team', #titlePanel("Old Faithful Geyser Data"),
                 mainPanel(width=10,
                     plotOutput(outputId="distPlot",height="325px"),
                     plotOutput(outputId="distPlot2",height="325px")
                 ),
                 sidebarPanel(width=2,
                    fluidRow(
                         column(width=12,
                         checkboxInput("xAdjust", label = "TG Stat Adjustment", 
                                value = FALSE))),
                     fluidRow(
                         column(width=12,
                         radioButtons("TeamStatID", "Rank Statistic",
                                choices=lstStatID,
                                selected="DI",inline=FALSE))),
                     
                     fluidRow(
                         column(width=6,
                         selectInput("TeamIDA", "Team A",choices=lstTeamID,
                                 selected="CW",selectize = FALSE, 
                                 width = "100px", size = 18)),
                         column(width=6,
                         selectInput("TeamIDB", "Team B",choices=lstTeamID,
                                 selected="ES",selectize = FALSE,
                                 width = "100px", size = 18))),
                     hr(),
                     p("Data from 2019 Season"),
                     p("TG rank within own team"),
                     p("Statistic rank across all players"),
                     p(tags$a(href="https://medium.com/analytics-vidhya/afl-player-importance-and-impact-d20acf754f9d", 
                              "Methodology")))
                 ) #, end tabpanel team
                 
        
        #tabPanel('Tab2'),
        #tabPanel('Tab3')
    )
)

