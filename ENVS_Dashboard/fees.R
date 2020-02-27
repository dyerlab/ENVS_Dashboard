source("pullData.R")
pull_data() %>%
  filter( Term %in% c("202010","202020") ,
          Course %in% c( "ENVS201","ENVZ335","ENVS101","ENVS102","ENVS260",
                         "ENVS360","ENVS361","ENVS461","ENVZ595" ) ) %>%
  group_by( Term, Course ) %>%
  summarize( Enrollment = sum( Enrollment ) )  %>%
  mutate( Fees = ifelse( Course %in% c("ENVS201","ENVZ335"), 
                         Enrollment * 65, 
                         Enrollment * 50) )  -> feeData


total %>%
  group_by( Term ) %>%
  summarize( `Total` = sum(Fees))
