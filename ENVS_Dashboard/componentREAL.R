#' This component is available to look at all the REAL designated classes
#' 


library(dplyr)
source("pullData.R")


realMenuItem <- menuItem( "REAL",
                          tabName="real",
                          icon=icon("external-link-square-alt"))

# The UI
realBodyItem <- function( data ) {
  ret <- tabItem( tabName = "real",
                  fluidRow(
                    
                    column(
                      width=12,
                      h1("REAL Student Curriculum"), 
                      
                      valueBox( format( 2019, big.mark="", scientific=FALSE), 
                                "Census Year",
                                icon=icon("calendar")),
                      
                      valueBox( 57,),
                                "Graduating Students Surveyed",
                                icon=icon(""), color="purple"),
                      
                      valueBox( trendCoefficient(data,"SCH"), "Annual Trendline",
                                icon=icon("chart-line"), color="yellow" ),
                      
                      
                      
                    )
                  ))
}