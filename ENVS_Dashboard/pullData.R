library(dplyr)

pull_data <- function() {
  data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS-3wFAaGu0NpcB87o6LXvVAN85_6cPzyCPb_w-DqDu8l3rbvg7BISlIqo3fL8CRaSkWKe8IJvcLiKQ/pub?gid=0&single=true&output=csv",
                   header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
  
  data %>% 
    mutate( YEAR = floor( TERM/100 ) ) %>% 
    mutate( SEMESTER = factor( ifelse( substr(data$TERM, 5, 6) == "10", "Fall", 
                                       ifelse(substr(data$TERM,5,6)=="20", "Spring", "Summer")),
                               ordered=TRUE, levels=c("Fall","Spring","Summer")))  %>%
    mutate( LEVEL = factor( ifelse( as.numeric(substr(data$COURSE, 5, 8 )) < 500, 
                                    "Undergraduate","Graduate" ),
                            ordered=TRUE, levels=c("Undergraduate","Graduate")) ) %>%
    mutate( TUITION = ifelse( LEVEL=="Graduate", 
                              ifelse(TERM < 201900, 590, 674), 
                              ifelse(TERM < 201900, 378, 417) ) ) %>%
    mutate( SCH = ACTUAL.ENROLLMENT * MAX.CREDITS ) %>% 
    mutate( TERM = factor( TERM, ordered=TRUE) ) %>% 
    select( TERM, YEAR, SEMESTER, LEVEL, COURSE, TYPE, MAX.CREDITS, TITLE, 
            MAX.SIZE, ACTUAL.ENROLLMENT, SCH, TUITION, PRIMARY.INSTRUCTOR.LAST.NAME, SECONDARY.INSTRUCTOR.LAST.NAME )  %>%
    mutate( Revenue = SCH * TUITION) %>% 
    filter( ACTUAL.ENROLLMENT > 0, TITLE != "CANCELLED") ->  data
  
  names(data) <- c("Term","Year", "Semester", "Level", 
                   "Course", "Type", "Credits",
                   "Title","Seats", "Enrollment", 
                   "SCH", "Tuition", "Instructor","Instructor2","Revenue")
  
  today <- strsplit( as.character(Sys.Date()), "-", fixed=TRUE)[[1]]
  term <- paste( today[1],"20",sep="")
  month <- as.numeric( today[2] )
  if( month > 8 ) {
    term <- paste( as.numeric(today[1])+1, 10, sep="" )
  } else if( month > 5 ) {
    term <- paste( as.numeric(today[1]), 30, sep="" )
  }
  data$Title <- as.character( data$Title)
  data$Title[ data$Course %in% c("ENVS291","ENVS391","ENVS491","ENVS591","ENVS691")] <- "SPECIAL TOPICS" 
  data$Title <- factor( data$Title, ordered=FALSE)
  data$Status <- "Final Class Count"
  data$Status[ as.numeric( as.character(data$Term )) > term ] <- "Registered To Date"
  data$Status <- factor( data$Status, ordered = TRUE, levels = c("Registered To Date","Final Class Count") )
  
  
  # fix instructor2 column
  data$Instructor <- as.character( data$Instructor )
  data$Instructor[ nchar(data$Instructor) == 0 ] <- "TBA"
  data$Instructor2 <- as.character( data$Instructor2 )
  data$Instructor2[ nchar(data$Instructor2) == 0] <- NA
  
  ret <- data[ is.na(data$Instructor2),]
  
  dup <- data[ !is.na(data$Instructor2),]
  dup$Enrollment <- dup$Enrollment / 2.0 
  dup2 <- dup
  dup2$Instructor <- dup2$Instructor2 
  ret <- rbind( ret, dup, dup2 )
  ret %>%
    arrange( Term, Course ) %>% 
    select( setdiff( names(ret), "Instructor2") ) -> data
  
  return( data )
}



trendCoefficient <- function( data, response ) {
  data %>% 
    filter( Status=="Final Class Count") -> data
  df <- data[, c("Year",response)]
  names(df)[2] <- "Value"
  df %>% group_by(Year) %>% summarize( Values = sum(Value)) -> d
  fit <- lm(Values ~ Year, data=d)
  return( round(fit$coefficients[2], digits=2) )
}



get_faculty <- function( ) {
  facultyTT <- c("Agosta","Bukaveckas","Dyer","Fernandez","McGarvey","Vonesh" )
  facultyTerm <- c( "Albrecht-Mallinger","Bulluck","Ciminelli","Connors","Crawford","McIninch","Shuart","Sikder","Viverette")
  facultyAdjunct <- c("Bernier","Blankenship","Ford","Fox","Kelly","Oden","Parent","Watson")
  faculty <- data.frame( Name = c(facultyTT, facultyTerm, facultyAdjunct),
                         Category = c( rep("Tenure Track", length(facultyTT)),
                                       rep("Term", length(facultyTerm)),
                                       rep("Adjunct",length(facultyAdjunct))),
                         Effort = c(40,40,10,40,40,20,
                                    80,25,40,80,25,40,25,80,30,
                                    100,100,100,100,100,100,100,100)
                         
  )
  
  faculty$Category <- factor( faculty$Category, ordered=FALSE )
  return( faculty)
}










  
