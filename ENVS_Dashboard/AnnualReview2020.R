# Makes individual plots


rm(list=ls())
library( tidyverse )
#load("ENVS_Dashboard/data.rda")
source("pullData.R")
data <- pull_data()



# Faculty in last 3 years
data %>%
  filter( Year >= 2017 ) %>%
  group_by( Instructor ) %>%
  summarize( numClasses = length(Course) )



# Enrollment per semester over last 3 years
data %>%
  filter( Year >= 2017 ) %>%
  filter( Semester %in% c("Fall","Spring") ) %>%
  group_by( Course ) %>%
  summarize( Seats = mean( Enrollment, na.rm=T) ) -> tmp
View(tmp)


# Grab enrollment for students in Fall 2019 and Spring 2020

data %>%
  filter( Course %in% c( "ENVS201","ENVS260",
                         "ENVS360","ENVS361","ENVS461", "ENVZ595",
                         "ENVZ335") ) %>%
  group_by( Year ) %>%
  summarize( Total = sum(Enrollment))


data %>%
  filter( Year < 2020, Year > 2011) %>%
  group_by( Year ) %>%
  summarize( SCH = sum( SCH ) ) %>%
  mutate( Director = factor( ifelse( Year < 2016, "Previous", "Dyer"), ordered=TRUE ) ) %>%
  ggplot( aes(Year,SCH, fill = Director) ) + 
  geom_bar( stat="identity" ) + 
  xlab( "Calendar Year" ) +
  ylab( "Student Contact Hours" ) +
  xlim( c(2008,2019) ) + 
  scale_x_continuous( breaks = 2008:2019 ) + 
  theme_bw() + 
  scale_fill_manual( values = c("#8fa3d6","grey") ) +
  stat_smooth( method="lm", se=FALSE, linetype=1, size=1, aes(color=Director)) +
  scale_color_manual( values = c("#4466bb","#888888"))
  



data %>%
  filter( Term %in% c(201620, 201720, 201820, 201920, 202020) ) %>%
  group_by( Term ) %>%
  summarize( SCH = sum(SCH) ) %>%
  ggplot( aes(Term,SCH) ) + 
  geom_bar( stat="identity", fill="#8fa3d6" ) + 
  xlab( "Spring Semester Enrollment" ) +
  ylab( "Student Contact Hours" ) +
  theme_bw()


read_csv("ENS_Minors2020.csv") %>% 
  mutate( Term = `ACADEMIC PERIOD`,
          ID = ID, 
          College = factor( COLLEGE1 ),
          Major = factor( MAJOR1 ), 
          Class = factor( CLASS, ordered=TRUE, levels=c("FR","SO","JR","SR") ) ) %>%
  select( Term, ID, College, Major, Class ) -> data


year_from_term <- function( x ) {
  x <- as.character( x )
  
  yr <- as.numeric( str_sub( x, 1, 4) )
  term <- str_sub(x,5,6)
  if( term == "10" )
    return( yr - 1)
  else 
    return( yr )
}

data$Year <- as.numeric( str_sub( data$Term, 1, 4) )
data$Year <- ifelse( str_sub(data$Term,5,6) == "10", data$Year - 1, data$Year )

data %>%
  group_by(Year) %>%
  summarize( Minors = length(unique(ID))) %>%
  filter( Year < 2020 ) %>%
  ggplot( aes(Year,Minors) ) + geom_bar( stat="identity", fill="#8fa3d6") + 
  xlab( "Calendar Year" ) +
  ylab( "Number of Minors" ) +
  theme_bw()
  
  
data %>%
  filter( Term == "202010") %>%
  group_by(College) %>%
  summarize( Minors = length(unique(ID)))  %>%
  ggplot( aes(College,Minors) ) + geom_bar( stat="identity", fill="#8fa3d6") + 
  xlab( "Fall Semester Enrollment" ) +
  ylab( "Students In Minor" ) +
  theme_bw()


data %>%
  filter( Term == "202010") %>%
  group_by( Major ) %>%
  summarize( Minors = length(unique(ID))) %>%
  arrange( -Minors )




