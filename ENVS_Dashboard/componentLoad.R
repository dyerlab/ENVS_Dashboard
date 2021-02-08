library( tidyverse )



loadMenuItem <- menuItem("Load",
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
                                        "Semester"))
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
        mutate( Grouping = Term ) -> tmp
    } else {
      tmp %>% 
        mutate( Grouping = Year ) -> tmp
    }
    
    tmp %>%
      group_by( Grouping ) %>%
      summarize( SCH=sum(SCH) ) %>%
      arrange( desc( Grouping )  ) %>%
      datatable( width="100%")
  })
  return( ret )
}
