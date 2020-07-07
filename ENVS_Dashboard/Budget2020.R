rm(list=ls())
library( tidyverse )
source("pullData.R")
data <- pull_data()





data %>%
  filter( Term %in% c("202010", "202020", "202030") ) %>%
  filter( FacultyType == "Adjunct") %>%
  group_by(Semester, FacultyType ) %>%
  summarize( Seats = sum(Enrollment),
             `Contact Hours` = sum(SCH),
             Revenue = sum(Revenue)) %>%
  knitr::kable( format = "pipe")
