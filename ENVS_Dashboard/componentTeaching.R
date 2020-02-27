library(dplyr)
library(ggplot2)


teachingMenuItem <- menuItem("Teaching Loads", tabName="teaching", icon=icon("weight-hanging") )


teachingBodyItem <- function( data, faculty ) {
  
  data %>% 
    filter( Term %in% c("201710","201720","201730", 
                        "201810","201820","201830", 
                        "201910","201920","201930")) %>%
    filter( Instructor != "Albrecht-Mallinger" ) %>%
    group_by(Instructor) %>%
    summarize( SCH=sum(SCH)/3) -> df
  
  merge( faculty, df, by.x="Name",by.y="Instructor") %>%
    filter( Category %in% c("Tenure Track", "Term") )  %>% 
    arrange( Name ) %>%
    mutate( stdSCH = SCH/Effort) %>%
    droplevels() -> df
  
  ret <- tabItem( tabName = "teaching",
                  fluidRow(
                    column(
                      width=12,
                      h1("Term & Tenure Track Teaching"),
                      
                      valueBox( format( max(df$SCH), big.mark=",", scientific=FALSE, digits=2), 
                                "Maximum Contact Hours",
                                icon=icon("arrow-alt-circle-up")),
                      valueBox( format( mean(df$SCH), big.mark=",", scientific=FALSE, digits = 2),
                                "Mean Contact Hours",
                                icon=icon("ellipsis-h"), color="purple"),
                      valueBox( format( mean(df$stdSCH), big.mark=",", scientific=FALSE, digits=2),
                                "Contact Hours / Teaching Effort",
                                icon=icon("bullseye"), color="yellow"),

                      p("Data reported for period of Current Director (2017-Present).  Faculty not represented across this entire time period removed to prevent bias.  These data are only for classes taught in ENVS, if a faculty member teaching a course under a different rubric, the contact hours will not be represented here."),
                      
                      box(
                        title="Total Teaching Effort",
                        width=12,
                        status="info",
                        solidHeader = FALSE,
                        column(
                          width=6,
                          selectInput("teachingPlotType",
                                      "Effort View:",
                                      c("Standardized Effort" = "hist",
                                        "SCH ~ f(Effort)" = "point") )  
                        ),
                        column(
                          width=6,
                          selectInput("teachingFacultyGroup",
                                      "Faculty Selection:",
                                      c("All", as.character(df$Name) ) )
                        )
                      ),
                      
                      box(
                        title="Teaching Loads for Faculty in Environmental Studies",
                        width=12,
                        status="primary",
                        solidHeader=TRUE,
                        plotOutput("teachingLoadPlot")
                      )
                    )
                  )
  )
  return(ret)
}




getTeachingLoadPlotOutput <- function( input, data, faculty ) {
  
  ret <- renderPlot({
    data %>% 
      filter( Term %in% c("201710","201720","201730", 
                          "201810","201820","201830", 
                          "201910","201920","201930")) %>%
      filter( Instructor != "Albrecht-Mallinger" ) %>%
      group_by(Instructor) %>%
      summarize( SCH=sum(SCH)/3) -> df
    
    merge( faculty, df, by.x="Name",by.y="Instructor") %>%
      filter( Category %in% c("Tenure Track", "Term") )  %>% 
      mutate( stdSCH = SCH/Effort) %>%
      droplevels() -> df
    
    if( input$teachingPlotType == "point") {

      if( input$teachingFacultyGroup == "All" ) {
        ggplot(df, aes(Effort,SCH,shape=Category)) + 
          geom_point(size=5, alpha=0.75) + 
          xlim(c(0,100)) + 
          theme_bw(base_size=20)  +
          xlab("Faculty Teaching Effort") + 
          ylab("Annual Student Contact Hours") 
      } else {
        df$Individual <- "Background"
        df$Individual[ df$Name == input$teachingFacultyGroup ] <- input$teachingFacultyGroup
        df$Individual <- factor( df$Individual, ordered=TRUE, levels=c("Background",input$teachingFacultyGroup) )
        ggplot(df, aes(Effort,SCH,shape=Category, color=Individual)) + 
          geom_point(size=5, alpha=0.8) + 
          xlim(c(0,100)) + 
          theme_bw(base_size=20)  +
          xlab("Faculty Teaching Effort") + ylab("Annual Student Contact Hours") + 
          scale_color_brewer( palette="Set1") 
      }
      
    } else  {
      
      if( input$teachingFacultyGroup == "All" ) {
        ggplot( df, aes(Category,stdSCH) ) + 
          geom_boxplot(notch=FALSE, fill="#56B4E9", outlier.size = 4) + 
          theme_bw(base_size=20)  +
          ylab("Standardized Teaching (SCH/Teaching)") + 
          xlab("Faculty Categories")
      } else {
        df$Cat <- as.character(df$Category)
        df$Cat[ df$Name == input$teachingFacultyGroup ] <- input$teachingFacultyGroup
        df$Cat <- factor( df$Cat, ordered=TRUE, levels=c(input$teachingFacultyGroup, "Term","Tenure Track"))
        
        ggplot( df, aes(Cat,stdSCH) ) + 
          geom_boxplot(notch=FALSE, fill="#56B4E9") + 
          theme_bw(base_size=20)  +
          ylab("Standardized Teaching (SCH/Teaching)") + 
          xlab("Individual and Faculty Categories")
      }
      
    }
  })
  
  return( ret )
}

