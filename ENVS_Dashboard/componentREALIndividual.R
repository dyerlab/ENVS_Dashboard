#' This component is available to look at all the REAL designated classes
#' 


library(dplyr)
source("pullData.R")


realIndMenuItem <- menuItem( "REAL Individual",
                             tabName="realInd",
                             icon=icon("external-link-alt"))

# The UI
realIndBodyItem <- function( data ) {
  ret <- tabItem( tabName = "realInd",
                  fluidRow(
                    
                    column(
                      width=12,
                      h1("REAL Individual Perspective"), 
                      
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
                          selectInput("categoryREALInd",
                                      "REAL Categorization",
                                      c("REAL Credit Depth",
                                        "REAL Designation")
                          )
                        ),
                        
                        plotOutput("realIndPlot")
                      )
                    )
                  )
  )
  
  return( ret)
}





getREALIndOutput <- function( input, data, REAL.df ) {
  
  print(summary(REAL.df))
  ret <- renderPlot({
    
    if( input$categoryREALInd == "REAL Designation") {
      REAL.df %>%
        group_by(VNumber, REAL.Level) %>%
        summarize( REAL_CREDITS = sum( Credits)) %>%
        filter( REAL.Level != "Class not REAL designated") %>%
        ggplot( aes(REAL_CREDITS) ) + 
        geom_histogram(binwidth = 1) +
        facet_grid( REAL.Level ~ . , scales="free_y" ) + 
        xlab("Credits within REAL designated courses") +
        ylab("Frequencie of student credit load")
    } else {
      REAL.df %>%
        group_by(VNumber) %>% 
        summarize( ENVS_Credits = sum( Credits[ DEPT == "ENVS" ]), 
                   Graduating_Credits = sum(Credits) ) %>%
        arrange( desc(Graduating_Credits ) ) -> credits
      
      d <- data.frame( Category = rep( c( "ENVS Classes",
                                          "Non-ENVS Classes"), each=nrow(credits)),
                       Credits = c(credits$ENVS_Credits,
                                   credits$Graduating_Credits - credits$ENVS_Credits
                       ) )
      
      ggplot( d, aes(Credits) ) + 
        geom_histogram( binwidth=5 ) + 
        facet_grid( Category ~ .) + 
        xlab("Student Credit Hours") + 
        ylab("Frequency in Graduating Cohort")
      
    }
    
    
    
  })
}







