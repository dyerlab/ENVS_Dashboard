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

source("pullData.R")

source("componentSCH.R")
source("componentSeats.R")
source("componentRevenue.R")
source("componentClasses.R")
source("componentCalendar.R")
source("componentFees.R")
source("componentREAL.R")
source("componentREALIndividual.R")
source("componentFaculty.R")
source("componentTeaching.R")
source("componentTeachingEvals.R")
source("componentLoad.R")
source("componentFees.R")

options(dplyr.summarise.inform=FALSE)

data <- pull_data()
load("REALdf.rda")
#curriculum <- load("curriculum.rda")
faculty <- get_faculty()
reviews <- get_reviews()
feeData <- get_fee_data()

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
                     calendarMenuItem,
                     seatMenuItem,
                     schMenuItem,
                     revenueMenuItem,
                     feesMenuItem
                     ),
            menuItem("Students", icon=icon("user-astronaut"),
                     realIndMenuItem,
                     realMenuItem 
                     ),
            menuItem("Faculty", icon=icon("user"),
                     facultyMenuItem,
                     teachingMenuItem,
                     loadMenuItem,
                     evalsMenuItem
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
            calendarBodyItem(data),
            seatsBodyItem(data),
            schBodyItem(data),
            revenueBodyItem(data),
            feesBodyItem( feeData ),
            realBodyItem(data),
            realIndBodyItem(data),
            facultyBodyItem(data,faculty),
            teachingBodyItem(data,faculty),
            loadBodyItem( faculty ),
            evalsBodyItem(reviews, faculty)
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    output$classTable <- getClassOutput( input, data )
    output$calendarTable <- getCalendarOutput(input)
    output$seats <- getSeatOutput(input, data)
    output$sch <- getSCHOutput( input, data )
    output$revenue <- getRevenueOutput( input, data )
    output$feesTable <- getFeesOutput( input, feeData)
    
    output$realTable <- getREALOutput(input,data, REAL.df)
    output$realIndPlot <- getREALIndOutput(input, data, REAL.df)
    
    output$facultySummaryTable <- getFacultyTableOutput( input, data, faculty )
    output$teachingLoadPlot <- getTeachingLoadPlotOutput( input, data, faculty)
    output$facultyLoadTable <- getFacultyTableOutput(input, data, faculty )
    output$evalsPlot <- getEvalsPlotOutput(input, reviews, faculty )
}

# Run the application 
shinyApp(ui = ui, server = server)
