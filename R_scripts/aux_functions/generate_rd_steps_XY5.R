

generate_rd_steps_XY5 <- function(steps_pollinator_i_year_plot_periodo_change_row,
                              number_random_steps,
                              steps_length_data,
                              p_binomial){
  
  rd_steps_pollinator_i_year_plot_periodo_change_row <- 
    steps_pollinator_i_year_plot_periodo_change_row %>% 
    slice(rep(1:n(), each = number_random_steps)) %>% mutate(control = 0)
  
  different_step_lengths <- unique(steps_length_data)
  
  
  if(length(different_step_lengths)<2){
    rd_lengths <- rep(different_step_lengths,number_random_steps)
  }else{
   
    #fit our dataset to a gamma distribution using mle
    steps_length_data[steps_length_data==0] <- 1e-3
    fit <- fitdistrplus::fitdist(steps_length_data, distr = "gamma", method = "mle")
    
    
    rd_lengths <- rgamma(number_random_steps,
                         fit[["estimate"]][["shape"]], fit[["estimate"]][["rate"]])
     
  }
  
  rd_angles_radians <- runif(number_random_steps, min = 0, max = 1) * 2 * pi
  
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
    runif(number_random_steps) <= p_binomial
  
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
