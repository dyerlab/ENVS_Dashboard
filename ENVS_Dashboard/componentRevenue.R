#' This is all the content for the revenue
#' 

library(dplyr)
source("pullData.R")


# Sidebar Menu Item
revenueMenuItem <- menuItem("Revenue", 
                        tabName = "revenue",        
                        icon = icon("dollar"))

# The UI
revenueBodyItem <- function( data ) {
  ret <- tabItem( tabName = "revenue",
                          fluidRow(
                            column(
                              width= 12,
                              h1("Tuition Generation from ENVS Classes"),
                              
                              valueBox( format( sum(data$Revenue), big.mark=",", scientific=FALSE), 
                                        "Total Revenue To Date",
                                        icon=icon("piggy-bank")),
                              valueBox( format( sum(data$Revenue)/length(unique(data$Year)),digits = 2, big.mark=",", scientific=FALSE),
                                        "Mean Annual Tuition Revenue",
                                        icon=icon("calendar"), color="purple"),
                              valueBox( format( trendCoefficient(data,"Revenue"), digits=2, big.mark = ",", scientific=FALSE), 
                                        "Annual Trendline",
                                        icon=icon("chart-line"), color="yellow" ),
                              p("Revenue was estimated based upon cost per credit hour taking into account changes in tuition rates since 2008.  These values do not take into consideration the taxes levied on tuition by administration, which averages 37% of generated tuition funds returning to Life Sciences (n.b., CES does not operate under a revenue sharing model)."), 
                              box(
                                title="Tuition Related Revenue",
                                status="primary",
                                width=12,
                                solidHeader = TRUE,
                                plotOutput("revenue")
                              ),
                              box(
                                title = "Temporal Range",
                                status = "info",
                                width = 6,
                                
                                selectInput( "periodRevenue", "Time Period",
                                             c( "Academic Year", 
                                                "Fall Semester"="Fall", 
                                                "Spring Semester" = "Spring", 
                                                "Summer Term" = "Summer") ),
                                
                                sliderInput("yearSliderRevenue", "Year Range:",
                                            min(data$Year), max(data$Year),
                                            c(min(data$Year), max(data$Year) ) )
                              ),
                              box(
                                title="Course Specifics",
                                status = "info",
                                width = 6,
                                
                                selectInput( "populationRevenue", "Student Population:",
                                             c("All Majors","Undergraduate","Graduate")),
                                
                                selectInput( "classRevenue", "ENVS Courses",
                                             c("All ENVS Classes" = "All", levels(data$Course ) ) )
                              )
                            )
                          )
                        )
  return( ret ) 
}



# The Output 
getRevenueOutput <- function( input, data ) {
  
  ret <- renderPlot({
    
    # filter on 
    if( input$populationRevenue != "All Majors" ) {
      data %>%
        filter( Level == input$populationRevenue) -> data
    }
    
    # filter on class
    if( input$classRevenue != "All" ) {
      data %>%
        filter( Course == input$classRevenue ) -> data
    }
    
    # filter on academic year 
    if( input$periodRevenue != "Academic Year") {
      data %>%
        filter( Semester == input$periodRevenue) -> data
    }   
    
    yr_range <- input$yearSliderRevenue
    
    title <- paste( "Tuition Revenue from ", 
                    input$populationRevenue, 
                    " in ", 
                    ifelse(input$classRevenue=="All", "All Classes", input$classRevenue), 
                    " ( ", 
                    ifelse( input$periodRevenue == "Academic Year", input$periodRevenue, paste( input$periodRevenue, "Semester")),
                    ": ", yr_range[1], "-", yr_range[2], ")", 
                    sep="")
    
    data %>%
      filter( Year >= yr_range[1], Year <= yr_range[2] ) %>%
      group_by( Year ) %>%
      summarize( Revenue = sum(Revenue), Status=unique(Status)) %>%
      ggplot( aes(Year, Revenue, fill=Status) ) + 
      geom_bar(stat="identity") + 
      ylab("Tuition Revenue ($)") + 
      xlab("Period") + 
      scale_fill_brewer( palette="Set1") + 
      ggtitle(title) + 
      theme_bw(base_size=20)  + 
      theme( axis.text.x=element_text(angle=75,hjust=1) ) 
    
  }
  )
}



