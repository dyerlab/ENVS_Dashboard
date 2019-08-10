#' This is all the content for the available seats components of the dashboard.
#' 

library(dplyr)


# Sidebar Menu Item
seatMenuItem <- menuItem( "Seats",
                          tabName="seats",        
                          icon = icon("couch") )

# The Seats UI
seatsBodyItem <- function( data ) { 
  ret <- tabItem( tabName = "seats",
                  fluidRow(
                    
                    column(
                      width= 12,
                      h1("Available Seats in ENVS Classes"),
                      
                      valueBox( format( sum(data$Seats), big.mark=",", scientific=FALSE), 
                                "Total Seats To Date",
                                icon=icon("couch")),
                      valueBox( round(sum(data$Seats)/length(unique(data$Year)),digits = 1),
                                "Average Annual Availability",
                                icon=icon("calendar"), color="purple"),
                      valueBox( trendCoefficient(data,"Seats"), "Annual Trendline",
                                icon=icon("chart-line"), color="yellow" ),
                      
                      box(
                        title="Seats Available",
                        status="primary",
                        width=12,
                        solidHeader = TRUE,
                        plotOutput("seats")
                      ),
                      
                      box(
                        title = "Temporal Range",
                        status = "info",
                        width = 6,
                        
                        selectInput( "period", "Time Period",
                                     c( "Academic Year", 
                                        "Fall Semester"="Fall", 
                                        "Spring Semester" = "Spring", 
                                        "Summer Term" = "Summer") ),
                        
                        sliderInput("yearSliderSeats", "Year Range:",
                                    min(data$Year), max(data$Year),
                                    c(min(data$Year), max(data$Year) ) )
                      ),
                      
                      box(
                        title="Course Specifics",
                        status = "info",
                        width = 6,
                        
                        selectInput( "population", "Student Population:",
                                     c("All Majors","Undergraduate","Graduate")),
                        
                        selectInput( "class", "ENVS Courses",
                                     c("All ENVS Classes" = "All", levels(data$Course ) ) )
                      )
                      
                    )
                    
                  )
  )
  return( ret)
  
}


# The Output Part(s)

getSeatOutput <- function( input, data ) {
  
  ret <- renderPlot({
    
    # filter on 
    if( input$population != "All Majors" ) {
      data %>%
        filter( Level == input$population) -> data
    }
    
    # filter on class
    if( input$class != "All" ) {
      data %>%
        filter( Course == input$class ) -> data
    }
    
    # filter on academic year 
    if( input$period != "Academic Year") {
      data %>%
        filter( Semester == input$period) -> data
    }   
    
    yr_range <- input$yearSliderSeats
    
    title <- paste( "Seats for ", 
                    input$population, 
                    " in ", 
                    ifelse(input$class=="All", "All Classes", input$class), 
                    " ( ", 
                    ifelse( input$period == "Academic Year", input$period, paste( input$period, "Semester")),
                    ": ", yr_range[1], "-", yr_range[2], ")", 
                    sep="")
    
    
    
    
    data %>%
      filter( Year >= yr_range[1], Year <= yr_range[2] ) %>%
      group_by( Year ) %>%
      summarize( Seats = sum(Seats), Status=unique(Status)) -> df
    
    ggplot(df, aes(Year, Seats, fill=Status) ) + 
      geom_bar(stat="identity") + 
      ylab("Seats Available") + 
      xlab("Period") + 
      scale_fill_brewer( palette="Set1") + 
      ggtitle(title) + 
      theme_bw(base_size=20)  + 
      theme( axis.text.x=element_text(angle=75,hjust=1) ) 
    
  }
  )
}



