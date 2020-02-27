library( dplyr )
library( ggplot2 )
library( ggrepel )

evalsMenuItem <- menuItem("Teaching Evaluations",
                          tabName = "evals",
                          icon = icon("user-check"))

evalsBodyItem <- function( reviews, faculty ) {
  
  
  ret  <- tabItem( tabName = "evals",
                   fluidRow(
                     column(
                       width=12,
                       h1("Teaching Evaluations"),
                       valueBox( 0.0, 
                                 "Classes Responding",
                                 icon = icon("arrow-alt-circle-up") 
                       ),
                       valueBox( 0.0, 
                                 "Classes Responding",
                                 icon = icon("arrow-alt-circle-up"),
                                 color="purple" ),
                       valueBox( 0.0, 
                                 "Classes Responding",
                                 icon = icon("arrow-alt-circle-up"),
                                 color = "yellow" ),
                       
                       p("The data below are derived from the questions delivered to all students at the end of the term.  From the larger survey, there are 20 questions that have relevance to individual teaching activities that facutly are evaluated upon."),
                       
                       box(  # the data controls part
                         title = "Teaching Evaluations",
                         width = 12, 
                         status = "info",
                         solidHeader = FALSE,
                         column(
                           width=6,
                           selectInput("evalsQuestion",
                                       "Survey Question",
                                       unique( reviews$Question ) )
                         ),
                         column(
                           width=6,
                           selectInput("evalsFaculty",
                                       "Faculty Selection:",
                                       c("All", faculty %>% 
                                           filter( Category != "Adjunct") %>%
                                           select( Name ) ) )
                         )
                         
                       ),
                       
                       box(  # the plotting part
                         title = "Teaching Reviews",
                         width = 12,
                         status = "primary",
                         solidHeader =  TRUE,
                         plotOutput( "evalsPlot" )
                       )
                     )
                   )
  )
  return( ret )
}



getEvalsPlotOutput <- function( input, reviews, faculty ) {
  
  ret <- renderPlot({
    
    reviews %>%
      filter( Question == input$evalsQuestion ) %>%
      droplevels() -> df
    
    
    if( input$evalsFaculty == "All") {
      
      # Make the plot
      df %>%
        ggplot() + 
        geom_density( aes(x=Score,fill = `Course Level`), alpha = 0.5 ) +
        xlab("Average Class Score") +
        ylab("Frequency") + 
        ggtitle( input$evalsQuestion ) + 
        theme_minimal() + 
        xlim( c(1,5) ) + 
        ylim( c(0,1) )
      
    } else {
      
      df %>%
        filter( Instructor == input$evalsFaculty ) %>%
        droplevels() %>%
        select( Class, Score, `Course Level` ) -> fac 
      
      if( nrow(fac) > 0 ) {
        df %>%
          ggplot() + 
          geom_density( aes( x=Score, fill = `Course Level`), alpha = 0.5 ) +
          xlab("Average Class Score") +
          ylab("Frequency") + 
          ggtitle( input$evalsQuestion ) + 
          theme_minimal() +
          xlim( c(1,5) ) + 
          ylim( c(0,1) ) +
          geom_linerange( aes( x=Score, ymin=0, ymax=0.5), data =  fac ) +
          geom_label_repel( aes( x=Score, y=0.5, label=Class, fill = `Course Level`), 
                            data =  fac,
                            nudge_y = 0.1 )     
        
      } else {
        data.frame( x = 0, y = 0, label = paste(input$evalsFaculty, 
                                                "did not yield any evaluations this past year")) %>%
          ggplot() + 
          geom_text( aes(x,y,label=label)) +
          theme_minimal()
      }
      
      
      
      
    }

    
    
  })
  
  return( ret )
  
  
  
  
}














