
# This code run conditional logistic regression models by using the observed 
# steps and the corresponding rd steps

# IMPORTANT: Since some XY combinations apparently do not have the species
# obsered in a given period (floral census). For that reason, the rd step info
# on the following variables is not updated: flowers_sp1_XY2,flowers_sp2_XY1,
# flowers_sp2_XY2, deltaflowers_sp1, deltaflowers_sp2. So do not use it to
# model

library(tidyverse)
library(survival)
library(car)

source("R_scripts/aux_functions/pollinator_model_exp_coef.R")
source("R_scripts/aux_functions/set_same_factor_levels_on_flower_data.R")

number_random_steps <- 20

data_path_file <- paste0("results/gorbea/total_pollinator_i_data_clogit_observations_",
                         number_random_steps,"_rd_steps_UPDATED_RANDOM_FIDELITY.csv")

total_pollinator_i_data_clogit <- read_csv(data_path_file) # %>% filter(Periodo<3)
ranking_pollinators <- total_pollinator_i_data_clogit %>% group_by(Polinizador) %>% count() %>% arrange(desc(n))

# Estimation of coeff.

main_pollinator_coef <- NULL
set.seed(1234)

for(pollinator_i in ranking_pollinators$Polinizador[1:5]) {
  
  print(pollinator_i)
  
  pollinator_i_data_clogit <- total_pollinator_i_data_clogit %>% 
    filter(Polinizador == pollinator_i)
  
  pollinator_i_data_clogit_within_field <- pollinator_i_data_clogit
  
  print(pollinator_i_data_clogit_within_field %>% 
          group_by(control,change_plant_sp) %>% count())
  
  pollinator_i_data_clogit_mod <- set_same_factor_levels_on_flower_data(pollinator_i_data_clogit_within_field)
  
  pollinator_i_data_clogit_mod$Periodo <- as.factor(pollinator_i_data_clogit_mod$Periodo)
  
  pollinator_i_data_clogit_mod$step_length <- pollinator_i_data_clogit_mod$step_length
  pollinator_i_data_clogit_mod$delta_richness <- scale(pollinator_i_data_clogit_mod$delta_richness)
  pollinator_i_data_clogit_mod$delta_total_flowers <- scale(pollinator_i_data_clogit_mod$delta_total_flowers)
  pollinator_i_data_clogit_mod$log_sl = log(pollinator_i_data_clogit_mod$step_length +1e-3)
  
  if(length(unique(pollinator_i_data_clogit_mod$Periodo))>1){
    
    model_pollinator_i <- clogit(control ~ step_length + #time_of_day +
                                   # log_sl +
                                   # step_length : time_of_day +
                                   # step_length : plot +
                                   #step_length : Year +
                                   # step_length : change_plant_sp +
                                   # time_of_day +
                                   # plot + 
                                   # Year + 
                                   change_plant_sp +
                                   # step_length : Periodo +
                                   delta_richness +
                                   delta_total_flowers +
                                   # delta_richness : change_plant_sp +
                                   # delta_total_flowers : change_plant_sp +
                                   # step_length : delta_richness +
                                   # step_length : delta_total_flowers +
                                   # delta_richness : delta_total_flowers +
                                   # cosine_turning +
                                   # step_length : cosine_turning +
                                   strata(step_ID),
                                 method = "exact",
                                 pollinator_i_data_clogit_mod)
    
  }else{
    
    model_pollinator_i <- clogit(control ~ step_length + #time_of_day +
                                # log_sl +
                                 # step_length : time_of_day +
                                 # step_length : plot +
                                 #step_length : Year +
                                 # step_length : change_plant_sp +
                                 # time_of_day +
                                 # plot + 
                                 # Year + 
                                 change_plant_sp +
                                   # step_length : Periodo +
                                   delta_richness +
                                   delta_total_flowers +
                                   # delta_richness : change_plant_sp +
                                   # delta_total_flowers : change_plant_sp +
                                   # step_length : delta_richness +
                                   # step_length : delta_total_flowers +
                                   # delta_richness : delta_total_flowers +
                                   # cosine_turning +
                                   # step_length : cosine_turning +
                                   strata(step_ID),
                                 method = "exact",
                                 pollinator_i_data_clogit_mod)
    
  }
  
  
  
  print(summary(model_pollinator_i))
  
  print(performance::check_collinearity(model_pollinator_i,component = "conditional"))
  
  # cat("concordance: ",model_pollinator_i[["concordance"]][6],"\n")
  
  print(cor.test(pollinator_i_data_clogit_mod$step_length[pollinator_i_data_clogit_mod$control == 1],
                 as.numeric(pollinator_i_data_clogit_mod$change_plant_sp[pollinator_i_data_clogit_mod$control == 1])),
        method = "spearman")
  
  print(cor.test(pollinator_i_data_clogit_mod$delta_richness[pollinator_i_data_clogit_mod$control == 1],
                 pollinator_i_data_clogit_mod$delta_total_flowers[pollinator_i_data_clogit_mod$control == 1]),
        method = "spearman")
  
  pollinator_i_coef <- broom::tidy(model_pollinator_i) %>% mutate(pollinator = pollinator_i)
  
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef)
  
}


# Save model coefficients

path_save_file <- paste0("results/gorbea/pollinator_floral_coef_observations_",
                         number_random_steps,"_rd_steps_UPDATED_RANDOM_FIDELITY.csv")

write_csv(main_pollinator_coef, path_save_file)
