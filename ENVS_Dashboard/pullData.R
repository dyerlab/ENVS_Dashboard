library(dplyr)

pull_data <- function( hideSpecialTopics=TRUE ) {
  
  #data <- read.csv("RawEnrollmentData.csv")
  data <- read.csv("RawEnrollmentData_202020.csv")
  #data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS-3wFAaGu0NpcB87o6LXvVAN85_6cPzyCPb_w-DqDu8l3rbvg7BISlIqo3fL8CRaSkWKe8IJvcLiKQ/pub?gid=0&single=true&output=csv",
  #                 header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
  
  data <- filter( data, TERM != "F")

  data %>% 
    mutate( YEAR = floor( as.numeric(TERM)/100 ) ) %>% 
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
    select( TERM, YEAR, SEMESTER, LEVEL, CRN, COURSE, SECT, TYPE, MAX.CREDITS, TITLE, 
            MAX.SIZE, ACTUAL.ENROLLMENT, SCH, TUITION, PRIMARY.INSTRUCTOR.LAST.NAME, SECONDARY.INSTRUCTOR.LAST.NAME )  %>%
    mutate( Revenue = SCH * TUITION) %>% 
    filter( ACTUAL.ENROLLMENT > 0, TITLE != "CANCELLED") ->  data
  
  

  
  
  
  
  
  names(data) <- c("Term","Year", "Semester", "Level", 
                   "CRN", "Course", "Section", "Type", "Credits",
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
  
  if( hideSpecialTopics ) {
    data$Title <- as.character( data$Title)
    data$Title[ data$Course %in% c("ENVS291","ENVS391","ENVS491","ENVS591","ENVS691")] <- "SPECIAL TOPICS" 
    data$Title <- factor( data$Title, ordered=FALSE)  
  }
  
  data$Status <- "Final Class Count"
  data$Status[ as.numeric( as.character(data$Term )) > term ] <- "Registered To Date"
  data$Status <- factor( data$Status, ordered = TRUE, levels = c("Final Class Count","Registered To Date") )
  
  # fix instructor2 column
  data$Instructor <- as.character( data$Instructor )
  data$Instructor[ nchar(data$Instructor) == 0 ] <- "TBA"
  data$Instructor2 <- as.character( data$Instructor2 )
  data$Instructor2[ nchar(data$Instructor2) == 0] <- NA

  
  # add faculty types
  data$FacultyType <- "Other"
  data$FacultyType[ data$Instructor %in% c("Agosta", "Bukaveckas", "Dyer", "Fernandez", "Jones", 
                                           "McGarvey", "Vonesh") ] <- "Tenure Track"
  
  data$FacultyType[ data$Instructor %in% c("Albrecht-Mallinger", "Bulluck", "Ciminelli", "Connors", 
                                           "McIninch", "Sikder", "Viverette") ] <- "Term"
  data$FacultyType[ data$Instructor %in% c("Crawford", "Parent", "Shuart") ] <- "Affiliate"
  data$FacultyType[ data$Instructor %in% c("Bernier", "Blankenship", "Fox", "Godfrey", "Kelly", 
                                           "Oden", "Watson", "Prevost-White", "Robertson", "Toibin", 
                                           "Wood", "Valdez") ] <- "Adjunct"
  
  data$FacultyType <- factor( data$FacultyType, 
                              ordered=TRUE,
                              levels = c("Tenure Track", "Term", "Adjunct","Affiliate","Other") )
  
  
  
  
  # Make a unique identifier to remove duplicates
  #data$ID <- paste( data$Term,data$CRN,data$Course,data$Section,data$Instructor, data$Title, sep=".")
    
  # fix part where two people are instructors so each gets half credit for SCH
  ret <- data[ is.na(data$Instructor2),]
  dup <- data[ !is.na(data$Instructor2),]
  dup$Enrollment <- dup$Enrollment / 2.0 
  dup2 <- dup
  dup2$Instructor <- dup2$Instructor2 
  ret <- rbind( ret, dup, dup2 )
  
  ret %>%
    arrange( Term, Course ) %>% 
    select( setdiff( names(ret), "Instructor2") ) %>%
    distinct() -> data
  
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
  facultyTT <- c( "Agosta", "Bukaveckas", "Dyer", "Fernandez", "McGarvey", "Vonesh", "Jones" )
  facultyTerm <- c( "Albrecht-Mallinger", "Bulluck", "Ciminelli", "Connors", "Crawford", "McIninch", "Shuart", "Sikder", "Viverette")
  facultyAdjunct <- c("Bernier", "Blankenship", "Ford", "Fox", "Kelly", "Oden", "Parent", "Watson")
  faculty <- data.frame( Name = c(facultyTT, facultyTerm, facultyAdjunct),
                         Category = c( rep("Tenure Track", length(facultyTT)),
                                       rep("Term", length(facultyTerm)),
                                       rep("Adjunct",length(facultyAdjunct))),
                         Effort = c(40,40,10,40,40,20,40,
                                    80,25,40,80,25,40,25,80,30,
                                    100,100,100,100,100,100,100,100)
                         
  )
  
  faculty$Category <- factor( faculty$Category, ordered=FALSE )
  return( faculty)
}



get_reviews <- function() {
  evals <- NULL
  load("evals2019.rda")
  return( evals )
}



get_fee_data <- function () {
  pull_data() %>%
    filter( Term %in% c("202010","202020") ,
            Course %in% c( "ENVS201","ENVZ335","ENVS101","ENVS102","ENVS260",
                           "ENVS360","ENVS361","ENVS461","ENVZ595" ) ) %>%
    group_by( Term, Course ) %>%
    summarize( Enrollment = sum( Enrollment ) )  %>%
    mutate( Fees = ifelse( Course %in% c("ENVS201","ENVZ335"), 
                           Enrollment * 65, 
                           Enrollment * 50) )  -> feeData
  
  return( feeData )
}



  
