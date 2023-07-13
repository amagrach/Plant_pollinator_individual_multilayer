

generate_rd_steps_XY3 <- function(steps_pollinator_i_year_plot_periodo_change_row,
                              number_random_steps,
                              steps_lenght_turning_table,
                              p_binomial){
  
  rd_steps_pollinator_i_year_plot_periodo_change_row <- 
    steps_pollinator_i_year_plot_periodo_change_row %>% 
    slice(rep(1:n(), each = number_random_steps)) %>% mutate(control = 0)
  
  
  samples_rd_steps <- sample(1:nrow(steps_lenght_turning_table),
                             number_random_steps,
                             replace = T,
                             prob = steps_lenght_turning_table$prob)
  
  rd_lengths <-
    steps_lenght_turning_table$length[samples_rd_steps]
  
  rd_angles_radians <- (steps_lenght_turning_table$length[samples_rd_steps])*pi/180
  
  cos_rd_angles <- cos(rd_angles_radians)
  sin_rd_angles <- sin(rd_angles_radians)
  
  rd_delta_x <- round(rd_lengths * cos_rd_angles, 0)
  rd_delta_y <- round(rd_lengths * sin_rd_angles, 0)
  
  rd_step_lengths <- sqrt(rd_delta_x^2 + rd_delta_y^2)
  
  rd_steps_pollinator_i_year_plot_periodo_change_row$step_length <-
    rd_step_lengths
  
  rd_steps_pollinator_i_year_plot_periodo_change_row$turning_angle <-
    NA
  
  rd_steps_pollinator_i_year_plot_periodo_change_row$change_plant_sp <-
    runif(number_random_steps) > p_binomial
  
  rd_steps_pollinator_i_year_plot_periodo_change_row$delta_X <-
    rd_delta_x
  
  rd_steps_pollinator_i_year_plot_periodo_change_row$delta_Y <-
    rd_delta_y
  
  rd_steps_pollinator_i_year_plot_periodo_change_row$X2 <-
    rd_steps_pollinator_i_year_plot_periodo_change_row$X1+
    rd_steps_pollinator_i_year_plot_periodo_change_row$delta_X
  
  rd_steps_pollinator_i_year_plot_periodo_change_row$Y2 <-
    rd_steps_pollinator_i_year_plot_periodo_change_row$Y1+
    rd_steps_pollinator_i_year_plot_periodo_change_row$delta_Y
  
  rd_steps_pollinator_i_year_plot_periodo_change_row <- 
    rd_steps_pollinator_i_year_plot_periodo_change_row %>% filter(X2>=0, 
                                                                  Y2>=0,
                                                                  X2 < 25,
                                                                  Y2 < 25)
  
  return(rd_steps_pollinator_i_year_plot_periodo_change_row)
  
}
