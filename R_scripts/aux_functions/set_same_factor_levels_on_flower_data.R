
set_same_factor_levels_on_flower_data <- function(pollinator_i_data_clogit){
  
  max_step_ID <- max(pollinator_i_data_clogit$step_ID)
  
  pollinator_i_data_clogit_mod <- pollinator_i_data_clogit
  
  pollinator_i_data_clogit_mod$Periodo_hora = factor(pollinator_i_data_clogit_mod$Periodo_hora, 
                                                     levels = c(1,2,3))
  pollinator_i_data_clogit_mod$Bosque = factor(pollinator_i_data_clogit_mod$Bosque, 
                                               levels = 1:5)
  pollinator_i_data_clogit_mod$time_of_day = factor(pollinator_i_data_clogit_mod$time_of_day, 
                                                    levels = c("10:00 - 12:39", "12:40 - 15:19", "15:20 - 18:05"))
  pollinator_i_data_clogit_mod$plot = factor(pollinator_i_data_clogit_mod$plot, 
                                             levels = c("Plot 1","Plot 2","Plot 3","Plot 4","Plot 5"))
  pollinator_i_data_clogit_mod$step_ID = factor(pollinator_i_data_clogit_mod$step_ID, 
                                                levels = 1:max_step_ID)
  
  pollinator_i_data_clogit_mod$Year = factor(pollinator_i_data_clogit_mod$Year, 
                                                levels = 2020:2021)
  
  return(pollinator_i_data_clogit_mod)
  
}

set_same_factor_levels_on_flower_data_donana <- function(pollinator_i_data_clogit){
  
  max_step_ID <- max(pollinator_i_data_clogit$step_ID)
  
  pollinator_i_data_clogit_mod <- pollinator_i_data_clogit
  
  pollinator_i_data_clogit_mod$Periodo_hora = factor(pollinator_i_data_clogit_mod$Periodo_hora, 
                                                     levels = c(1,2,3))
  pollinator_i_data_clogit_mod$time_of_day = factor(pollinator_i_data_clogit_mod$time_of_day, 
                                                    levels = c("10:00 - 11:59", "12:00 - 13:59", "14:00 - 16:05"))
  pollinator_i_data_clogit_mod$plot = factor(pollinator_i_data_clogit_mod$Bosque, 
                                             levels = c("Pinar Aznalcazar","Pinar Hinojos","Pinar Puebla",
                                                        "Pinar Villamanrique Este (Chaparral)","Pinar Villamanrique Sur"))
  pollinator_i_data_clogit_mod$step_ID = factor(pollinator_i_data_clogit_mod$step_ID, 
                                                levels = 1:max_step_ID)
  
  pollinator_i_data_clogit_mod$Year = factor(pollinator_i_data_clogit_mod$Year, 
                                             levels = 2021)
  
  return(pollinator_i_data_clogit_mod)
  
}
