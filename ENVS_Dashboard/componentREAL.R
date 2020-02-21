#' This component is available to look at all the REAL designated classes
#' 


library(dplyr)
source("pullData.R")


realMenuItem <- menuItem( "REAL Designation",
                          tabName="real",
                          icon=icon("external-link-square-alt"))

# The UI
realBodyItem <- function( data ) {
  ret <- tabItem( tabName = "real",
                  fluidRow(
                    
                    column(
                      width=12,
                      h1("REAL Student Curriculum"), 
                      
                      valueBox( 57,
                                "Previuous Year Graduating Students Surveyed",
                                icon=icon(""), color="purple"),
                      
                      valueBox( 31.8, 
                                "Fraction of Seats in ENVS 'REAL' Classes",
                                icon=icon("chart-line"), color="yellow"),
                      
                      valueBox( 75.4, 
                               "Fraction of Students with REAL Experience",
                               icon=icon("chart-line"), color="yellow" ),
                      
                      box( 
                        title="Environmental Studies Undergraduate REAL Credit Loads",
                        width=12,
                        solidHeader=TRUE,
                        status="primary",
                        
                        column(
                          width=3,
                          selectInput("categoryREAL",
                                      "REAL Categorization",
                                      c("REAL Level",
                                        "REAL Type")
                          )
                        ),
                        
                        dataTableOutput("realTable")
                      )
                    )
                  )
  )
  
  return( ret)
}





getREALOutput <- function( input, data, REAL.df ) {
  
  
  ret <- renderDataTable({
    
    tbl <- data.frame() 
    
    if( input$categoryREAL == "REAL Level"){
      REAL.df %>%
        group_by(`Class Category` = REAL.Level) %>%
        summarize( `ENVS Class` = sum(Credits[ InMajor == TRUE]),
                   `Other Class` = sum(Credits[ InMajor == FALSE]),
                   `Total` = sum(Credits, na.rm=TRUE)) -> tbl
    }
    else {
      REAL.df %>%
        group_by(`Class Category` = REAL.Type) %>%
        summarize( `ENVS Class` = sum(Credits[ InMajor == TRUE]),
                   `Other Class` = sum(Credits[ InMajor == FALSE]),
                   `Total` = sum(Credits)) -> tbl
    }
    
    
    if( nrow(tbl) > 0 ) {
      DT::datatable(tbl, width="100%",options = list( pageLength=15) )
    } else {
      print("No data available")
    } 
    
  })
}







