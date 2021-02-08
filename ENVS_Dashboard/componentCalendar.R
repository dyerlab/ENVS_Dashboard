#' Makes a calendar from the selected semester
#' 

library( tidyverse )
library( knitr )
library( kableExtra )
source("makeGoogleCalendar.R")


calendarMenuItem <- menuItem("Calendar",
                             tabName = "calendar",
                             icon = icon("calendar"))



calendarBodyItem <- function( data ) {
  semester <- as.character(sort(unique(data$Term)))
  
  ret <- tabItem( tabName = "calendar",
                  fluidRow(
                    column( 
                      width=12,
                      h1("Environmental Studies Weekly Course Schedule"),
                      box( 
                        title="Semester",
                        column(
                          width=12,
                          selectInput("celendarSemester",
                                      "Semester",
                                      semester, 
                                      selected = "202120")
                        ))
                    ),
                    column(
                      width=12,
                      tableOutput("calendarTable")
                    )
                  ))
  return( ret )
}


getCalendarOutput <- function(input) {
  
  #ret <- renderTable( {
  #  makeClassTable( input$celendarSemester ) 
  #}, rownames = TRUE, sanitize.text.function = NULL)
  
  ret <- renderText( {
    makeClassTable( input$celendarSemester )  %>%
      kable("html", row.names = TRUE, escape = FALSE) %>%
      kable_styling("striped", full_width = FALSE)
  })
  
  
  return( ret )    

}

