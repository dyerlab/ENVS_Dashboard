

source("pullData.R")



facultyMenuItem <- menuItem("Summary", tabName="faculty", icon=icon("th") )


facultyBodyItem <- function( data, faculty) {
  ret <- tabItem( tabName = "faculty",
                  fluidRow(
                    column(
                      width=12,
                      h1("Environmental Studies Faculty"),
                      
                      infoBox("Current Tenure Track Faculty",
                              sum( faculty$Category == "Tenure Track"),
                              icon=icon("user") ),
                      
                      infoBox("Current Term Faculty",
                              sum( faculty$Category == "Term"),
                              icon=icon("user"), color="purple"),
                      
                      infoBox("Current Adjunct Faculty",
                              sum( faculty$Category == "Adjunct"),
                              icon=icon("user"), color="yellow"),
                      box(
                        title="Class Summaries",
                        width=12,
                        solidHeader=TRUE,
                        status="primary",
                        dataTableOutput("facultySummaryTable")
                    )
                    )
                    )
  )
  return( ret)
}


getFacultyTableOutput <- function( input, data, faculty ) {
  merge(faculty, data, by.x = "Name", by.y="Instructor") %>%
    filter( Category != "Adjunct")  %>%
    droplevels() %>%
    group_by(Name) %>% 
    summarize( Category=unique(Category), 
               Envrollment=format(mean(Enrollment), digits = 2), 
               Classes=paste0(sort(unique(Title)), collapse=", ")) -> df
  
  ret <- renderDataTable({
    datatable(df, width="100%")    
  })
  
  return( ret )

}

