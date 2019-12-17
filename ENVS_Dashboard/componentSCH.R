#' This is all the content for the available seats components of the dashboard.
#' 

library(dplyr)
source("pullData.R")

# Sidebar Menu Item
schMenuItem <- menuItem("Contact Hours", 
                        tabName = "sch",        
                        icon = icon("clock"))

# The UI
schBodyItem <- function( data )  {
  ret <- tabItem( tabName = "sch",
                          fluidRow(
                            
                            column(
                              width= 12,
                              h1("Student Contact Hours for ENVS Classes"),
                              
                              valueBox( format( sum(data$SCH), big.mark=",", scientific=FALSE), 
                                        "Total Contact Hours To Date",
                                        icon=icon("clock")),
                              valueBox( round(sum(data$SCH)/length(unique(data$Year)),digits = 1),
                                        "Mean Annual Student Contact Hours",
                                        icon=icon("calendar"), color="purple"),
                              valueBox( trendCoefficient(data,"SCH"), "Annual Trendline",
                                        icon=icon("chart-line"), color="yellow" ),
                              
                              box(
                                title = "Temporal Range",
                                status = "info",
                                width = 6,
                                
                                selectInput( "periodSCH", "Time Period",
                                             c( "Academic Year", 
                                                "Fall Semester"="Fall", 
                                                "Spring Semester" = "Spring", 
                                                "Summer Term" = "Summer") ),
                                
                                sliderInput("yearSliderSCH", "Year Range:",
                                            min(data$Year), max(data$Year),
                                            c(min(data$Year), max(data$Year) ) )
                              ),
                              
                              box(
                                title="Course Specifics",
                                status = "info",
                                width = 6,
                                
                                selectInput( "populationSCH", "Student Population:",
                                             c("All Majors","Undergraduate","Graduate")),
                                
                                selectInput( "classSCH", "ENVS Courses",
                                             c("All ENVS Classes" = "All", sort(unique(data$Course)) ) )
                              ),
                              
                              box(
                                title="Contact Hours",
                                status="primary",
                                width=12,
                                solidHeader = TRUE,
                                plotOutput("sch")
                              )
                            )
                          )
                        )
  
  return(ret)
}


# The Output 
getSCHOutput <- function( input , data) {
  
  ret <- renderPlot({
    
    # filter on 
    if( input$populationSCH != "All Majors" ) {
      data %>%
        filter( Level == input$populationSCH) -> data
    }
    
    # filter on class
    if( input$classSCH != "All" ) {
      data %>%
        filter( Course == input$classSCH ) -> data
    }
    
    # filter on academic year 
    if( input$periodSCH != "Academic Year") {
      data %>%
        filter( Semester == input$periodSCH) -> data
    }   
    
    yr_range <- input$yearSliderSCH
    
    title <- paste( "Contact Hours for ", 
                    input$populationSCH, 
                    " in ", 
                    ifelse(input$classSCH=="All", "All Classes", input$classSCH), 
                    " ( ", 
                    ifelse( input$periodSCH == "Academic Year", input$periodSCH, paste( input$periodSCH, "Semester")),
                    ": ", yr_range[1], "-", yr_range[2], ")", 
                    sep="")
    
    data %>%
      filter( Year >= yr_range[1], Year <= yr_range[2] ) %>%
      group_by( Year ) %>%
      summarize( SCH = sum(SCH) ) %>%
      ggplot( aes(Year, SCH ) ) + 
      geom_bar(stat="identity") + 
      ylab("Student Contact Hours") + 
      xlab("Period") + 
      scale_fill_brewer( palette="Set1") + 
      ggtitle(title) + 
      theme_bw(base_size=20)  + 
      theme( axis.text.x=element_text(angle=75,hjust=1) ) 
    
  }
  )
}



