#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(DT)
library(dplyr)
library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)

source("componentSCH.R")
source("componentSeats.R")
source("componentRevenue.R")
source("componentClasses.R")
source("componentREAL.R")
source("componentREALIndividual.R")
source("componentFaculty.R")
source("componentTeaching.R")

data <- pull_data()
load("REALdf.rda")
#curriculum <- load("curriculum.rda")
faculty <- get_faculty()

ui <- dashboardPage(
    
    skin="black",
    
    dashboardHeader(
        title = "ENVS Teaching"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName="dashboard",  icon = icon("dashboard")),
            
            menuItem("Classes", icon=icon("school"),
                     classesMenuItem,
                     seatMenuItem,
                     schMenuItem,
                     revenueMenuItem
                     ),
            menuItem("Students", icon=icon("user-astronaut"),
                     realIndMenuItem,
                     realMenuItem 
                     ),
            menuItem("Faculty", icon=icon("user"),
                     facultyMenuItem,
                     teachingMenuItem
                     )
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem( tabName = "dashboard",
                     fluidRow(
                         column( 
                             width=12,
                             tags$img(src="ces_logo.png", width="75%", align="center"),
                             tags$p(""),
                             tags$hr(),
                             tags$p("This dashboard provides up-to-date information on curricula and faculty in the Center for Environmental Studies at Virginia Commonwealth University.  ")
                             )
                     )
            ),
            classesBodyItem(data,faculty),
            seatsBodyItem(data),
            schBodyItem(data),
            revenueBodyItem(data),
            realBodyItem(data),
            realIndBodyItem(data),
            facultyBodyItem(data,faculty),
            teachingBodyItem(data,faculty)
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    output$classTable <- getClassOutput( input, data )
    output$seats <- getSeatOutput(input, data)
    output$sch <- getSCHOutput( input, data )
    output$revenue <- getRevenueOutput( input, data )
    
    output$realTable <- getREALOutput(input,data, REAL.df)
    output$realIndPlot <- getREALIndOutput(input, data, REAL.df)
    
    output$facultySummaryTable <- getFacultyTableOutput( input, data, faculty )
    output$teachingLoadPlot <- getTeachingLoadPlotOutput( input, data, faculty)
}

# Run the application 
shinyApp(ui = ui, server = server)
