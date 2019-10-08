#' General Summary stuff for classes
#' 
#' 

library(DT)
source("pullData.R")

classesMenuItem <- menuItem("Summary",       
                            tabName = "classes",    
                            icon = icon("th")
)



classesBodyItem <- function( data, faculty ) {
  
  data %>%
    mutate( Course = factor( Course ) ) %>%
    group_by(Course) %>%
    summarize(Title = paste0(unique(Title), collapse=", "),
              Size = round(mean(Enrollment), digits=0),
              Type = unique(Type), 
              Level = unique(Level),
              Professors = paste0( sort(unique(Instructor)), collapse=", "),
              Offered = paste0(sort(unique(Semester)), collapse=", ")) -> Classes
  
  faculty <- c("All", sort(as.character(unique(data$Instructor))))
  faculty <- faculty[nchar(faculty)>0]
  
  
  ret <- tabItem( tabName = "classes",
                  fluidRow(
                    column(
                      width=12,
                      h1("Environmental Studies Class Summaries"),
                      
                      infoBox( "Number of Classes",       nrow(Classes),                       icon=icon("school")                   ),
                      infoBox( "Undergraduate Offerings", sum(Classes$Level=="Undergraduate"), icon=icon("calendar"), color="purple"   ),
                      infoBox( "Graduate Offerings",      sum(Classes$Level=="Graduate"),      icon=icon("chart-line"), color="yellow" ),
                      
                      box(
                        title="Class Summaries",
                        width=12,
                        solidHeader=TRUE,
                        status="primary",
                        
                        column(
                          width=3,
                          selectInput("levelClass",
                                      "Course Level:",
                                      c("All","Undergraduate","Graduate" ))
                        ),
                        
                        column(
                          width=3,
                          selectInput("typeClass", "Course Type:",
                                      c("All",
                                        "Lecture"="LEC",
                                        "Laboratory"="LAB",
                                        "Field Work"="FLD",
                                        "Research"="RES",
                                        "Independent Study"="IND",
                                        "Thesis"="MST") )
                        ),
                        
                        column(
                          width=3,
                          selectInput("offeredClass", "Semester Offered:",
                                      c("Any","Fall","Spring","Summer"))
                        ),
                        
                        column(
                          width=3,
                          selectInput("facultyClass", "Faculty",
                                      faculty )
                        ),
                        
                        dataTableOutput("classTable")
                        
                      )
                    )
                  )
  )
  return(ret)
}





getClassOutput <- function( input, data ) {
  
  data %>%
    group_by(Course) %>%
    summarize(Title = paste0(unique(Title), collapse=", "),
              Size = round(mean(Enrollment), digits=0),
              Type = unique(Type), 
              Level = unique(Level),
              Professors = paste0( sort(unique(Instructor)), collapse=", "),
              Offered = paste0(sort(unique(Semester)), collapse=", ")) -> Classes
  
  
  ret <- renderDataTable({
    
    if( input$offeredClass != "Any"){
      Classes %>% 
        filter( agrepl(input$offeredClass,Offered)) -> Classes
    }
    
    if( input$facultyClass != "All"){
      Classes %>%
        filter( agrepl(input$facultyClass, Professors)) -> Classes
    }
    
    if( input$typeClass != "All") {
      Classes %>%
        filter( Type == input$typeClass) -> Classes
    }
    
    if( input$levelClass != "All") {
      Classes %>%
        filter( Level == input$levelClass ) -> Classes
    }
    
    
    if( nrow(Classes) > 0 ) {
      Classes %>%
        select( Course, Title, Size, Professors) %>%
        DT::datatable( width="100%")
    } else {
      print("No data available")
    } 
    
  })
}
