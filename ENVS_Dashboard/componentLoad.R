library( tidyverse )



loadMenuItem <- menuItem("Teaching History",
                         tabName = "load",
                         icon = icon("balance-scale") )

loadBodyItem <- function( faculty ) {
  
  faculty %>%
    filter( Category != "Adjunct") %>%
    arrange( Name ) -> tmp
  
  
  
  ret <- tabItem( tabName = "load",
                  fluidRow(
                    column(
                      width=12,
                      h1("Teaching Loads By Faculty"),
                      
                      box(
                        title="Faculty",
                        width=6,
                        status="info",
                        solidHeader = FALSE,
                        column(
                          width=6,
                          selectInput("facultyLoadSelection",
                                      "Faculty:",
                                      tmp$Name )  
                        )
                      ),
                      box(
                        title="Grouping",
                        width=6,
                        status = "info",
                        solidHeader=FALSE,
                        column(
                          width=6,
                          selectInput("facultyLoadGrouping",
                                      "Grouping:",
                                      c("Annual",
                                        "Semester",
                                        "Semester & Class"))
                        )
                      ),
                      
                      box(
                        title="Teaching Load",
                        width = 12, 
                        solidHeader = TRUE,
                        status = "primary",
                        dataTableOutput("facultyLoadTable") 
                      )
                    )  
                  )
  )
  return( ret )
}


getFacultyTableOutput <- function( input, data, faculty ) {
  
  ret <- renderDataTable( {
    
    data %>%
      filter( Instructor == input$facultyLoadSelection ) -> tmp
    
    if ( input$facultyLoadGrouping == "Semester"  ) {
      tmp %>%
        group_by( Term ) %>%
        summarize( SCH=sum(SCH) ) %>%
        arrange( desc( Term )  ) -> tmp
    } else if ( input$facultyLoadGrouping == "Annual" ) {
      tmp %>% 
        group_by( Year ) %>%
        summarize( SCH=sum(SCH) ) %>%
        arrange( desc( Year )  )  -> tmp
    } else {
      tmp %>% 
        group_by( Year, Term, Course  ) %>%
        summarize( SCH=sum(SCH) ) %>%
        arrange( desc( Year ), desc( Term )  )  -> tmp
    }
    
    tmp   %>%
      datatable( width="100%")
  })
  return( ret )
}
