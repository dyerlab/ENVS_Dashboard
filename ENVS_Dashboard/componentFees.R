#' This is the stuff for course fees
#' 

feesMenuItem <- menuItem( "Fees", 
                          tabName = "fees",
                          icon = icon("coins"))

feesBodyItem <- function( data ) {
  

  ret <- tabItem( tabName = "fees",
                  fluidRow(
                    column(
                      width=12,
                      h1("Course Fees"), 
                      valueBox( format( sum(data$Fees)  , 
                                        big.mark=",", 
                                        scientific=FALSE), 
                                "Total Fees Collected",
                                icon=icon("dollar")),
                      valueBox( length(unique(data$Course)),
                                "Courses With Fees",
                                icon=icon("th"), 
                                color="purple"),
                      valueBox( sum(data$Enrollment),
                                "Students Assessed Course Fees",
                                icon = icon("user-astronaut"),
                                color = "yellow"),
                      p("Course fees are assessed for each student who registers for a class.  These data are based upon the number of enrolled seats after add/drop for each semester."),
                      
                      box(
                        title = "Fees Assessed",
                        status = "primary",
                        width=12,
                        solidHeader = TRUE,
                        dataTableOutput("feesTable")
                        )
                      )
                    )
                  )
  return( ret )
}

getFeesOutput <- function( input, data ) {
  
  ret <- renderDataTable({
    
    data %>%
      DT::datatable( width="100%")
    
  })
}