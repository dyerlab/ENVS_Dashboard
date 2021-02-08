# Makes a Google Calendar Entry for the specified semester
library( tidyverse )


makeClassTable <- function( semester  ) {
  
  data <- read_csv("RawEnrollmentData_202020.csv")
  
  if( !(semester %in% data$TERM)) {
    print("nothing selected nothing returned.")
    return( "" )
  }
  

   data %>%
    filter( TERM == semester ) %>%
    mutate( MON = ifelse( is.na(`MON-IND`), "", "M"),
            TUE = ifelse( is.na(`TUE-IND`), "", "T"),
            WED = ifelse( is.na(`WED-IND`), "", "W"),
            THU = ifelse( is.na(`THU-IND`), "", "R"),
            FRI = ifelse( is.na(`FRI-IND`), "", "F") ) %>%
    mutate( DAYS = paste( MON,TUE,WED,THU,FRI, sep="" ) ) %>%
    mutate( LOCATION = paste( BUILDING, ROOM, sep="") )  %>%
    filter( nchar(DAYS) > 0 ) %>%
    select( COURSE, SECT, TITLE, 
            INSTRUCTOR = `PRIMARY INSTRUCTOR LAST NAME`,
            `START DATE`, `END DATE`, DAYS, 
            `BEGIN TIME`, `END TIME`, 
            LOCATION ) -> tmp
  
  
  if( nrow(tmp) > 0 ) {
    out <- matrix("", nrow = 15, ncol=5)
    row.names( out ) <- as.character( seq( 7, 21))
    colnames( out ) <- c("M","T","W","T","F")
    
    for( i in 1:nrow(tmp) ) {
      start <- floor( tmp$`BEGIN TIME`[i] / 100 ) 
      end <- floor( tmp$`END TIME`[i] / 100 )
      title <- paste(tmp$COURSE[i], tmp$SECT[i], sep=".")
      title <- paste( title, tmp$TITLE[i], sep=": ")
      days <- tmp$DAYS[i]
      for( hour in seq(start,end) ) {
        ridx <- which( hour == row.names(out))
        for( day in strsplit(days, split = "")[[1]] ){
          cidxs <- which( day == colnames(out) )
          for( cidx in cidxs ) {
            
            #print( paste( start, ridx, end, cidx, day) )
            
            
            if( length( cidx ) > 0 ) {
              if( nchar( out[ridx,cidx] ) > 1 ) {
                out[ridx,cidx] <- paste( out[ridx,cidx], title, sep = "<br/>")         
              } else { 
                out[ridx,cidx] <- title         
              }
            }
          }
        }
      }
    }
    
    colnames( out ) <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
    
    return( as.data.frame( out ) )
    
  } else {
    return( "No classes" )
  }
  
    
}


