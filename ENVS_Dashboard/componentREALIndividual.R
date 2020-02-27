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
                                "Fraction of Students with REAL Experience Across All Classes",
                                icon=icon("chart-line"), color="yellow" ),
                      
                  
                      box( 
                        title="REAL Categorization",
                        width=12,
                        solidHeader=TRUE,
                        status="info",
                        column(
                          width=6,
                          selectInput("categoryREALInd",
                                      "REAL Categorization",
                                      c("REAL Credit Depth",
                                        "REAL Designation")
                          )
                        )
                      ), 
                      
                      box( 
                        title="Environmental Studies Undergraduate REAL Credit Loads",
                        status="primary",
                        width=12,
                        solidHeader = TRUE,
                        plotOutput("realIndPlot")
                      )
                      
                    )
                  )
  )
  
  return( ret)
}





getREALIndOutput <- function( input, data, REAL.df ) {
  
  ret <- renderPlot({
    
    if( input$categoryREALInd == "REAL Designation") {
      REAL.df %>%
        group_by(VNumber, REAL.Level) %>%
        summarize( REAL_CREDITS = sum( Credits)) %>%
        filter( REAL.Level != "Class not REAL designated") %>%
        ggplot( aes(REAL_CREDITS, fill = REAL.Level) ) + 
        geom_density(binwidth = 1, alpha=0.6 ) +
        xlab("Credits within REAL designated courses") +
        ylab("Frequencie of student credit load") + 
        theme_bw( base_size = 20 ) + 
        scale_fill_brewer(palette = "Set1")
      
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
      
      ggplot( d, aes(Credits, fill=Category) ) + 
        geom_histogram( binwidth=5 , position='dodge2') + 
        xlab("Student Credit Hours") + 
        ylab("Frequency in Graduating Cohort") + 
        theme_bw( base_size = 20 ) + 
        scale_fill_brewer(palette = "Set1")
      
    }
    
    
    
  })
}







