

library(tidyverse)
library(survival)
library(car)

source("R_scripts/aux_functions/pollinator_model_exp_coef.R")
source("R_scripts/aux_functions/set_same_factor_levels_on_flower_data.R")
source("R_scripts/aux_functions/potential_steps_to_subplot.R")

#---------------------------------------
# Load fruitset data for Doñana 2021
fruitset_census <- read_csv("results/donana/fruitset_census_21.csv") %>%
  mutate(Subplot=paste0(X,"-",Y)) %>% filter(!is.na(Planta))

bosque_plant_week_combinations <- fruitset_census %>%
  dplyr::select(Bosque,Planta,X,Y,Periodo,) %>%
  unique()

#---------------------------------------
# Load floral data for Doñana 2021
flora_census <- read_csv("results/donana/flora_census_21.csv")


#---------------------------------------
# Load random steps data for Doñana 2021
number_random_steps <- 20

data_path_file <- paste0("results/donana/total_pollinator_i_data_clogit_observations_",
                         number_random_steps,"_rd_steps_UPDATED.csv")

total_pollinator_i_data_clogit_raw <- read_csv(data_path_file) # %>% filter(Periodo<3)


#---------------------------------------
# Model fit
set.seed(1234)

bosque_plant_week_combinations$prob_consp_step <- NA

for(combination_i in 1:nrow(bosque_plant_week_combinations)){
  
  total_pollinator_i_data_clogit <- total_pollinator_i_data_clogit_raw %>%
    filter(Bosque == bosque_plant_week_combinations$Bosque[combination_i],
           Planta2 == bosque_plant_week_combinations$Planta[combination_i],
           Periodo == bosque_plant_week_combinations$Periodo[combination_i])
  
  
  if(nrow( total_pollinator_i_data_clogit)>1){
    
    
    pollinator_i_data_clogit_mod <- set_same_factor_levels_on_flower_data_donana(total_pollinator_i_data_clogit) %>%
      mutate(cosine_turning = cos(turning_angle*pi/180))
    
    # pollinator_i_data_clogit_mod$Periodo <- as.factor(pollinator_i_data_clogit_mod$Periodo)
    # pollinator_i_data_clogit_mod$step_length <- pollinator_i_data_clogit_mod$step_length
    # pollinator_i_data_clogit_mod$delta_richness <- pollinator_i_data_clogit_mod$delta_richness
    # pollinator_i_data_clogit_mod$delta_total_flowers <- pollinator_i_data_clogit_mod$delta_total_flowers
    
    
    model_pollinator_i <- clogit(control ~ step_length + #time_of_day +
                                   #step_length : time_of_day +
                                   #step_length : Bosque +
                                   # # step_length : Year +
                                   change_plant_sp +
                                   # # time_of_day +
                                   # # Bosque +
                                   # change_plant_sp +
                                   # # step_length : Periodo +
                                   delta_richness +
                                   delta_total_flowers +
                                   # # step_length : delta_richness +
                                   # # step_length : delta_total_flowers +
                                   # # delta_richness : delta_total_flowers +
                                   # # cosine_turning +
                                   # # step_length : cosine_turning +
                                   strata(step_ID),
                                 method = "exact",
                                 control = coxph.control(iter.max = 1e4),
                                 pollinator_i_data_clogit_mod)
    
    print(summary(model_pollinator_i))
    
    potential_steps_i <- potential_steps_to_subplot_period(
      X = bosque_plant_week_combinations$X[combination_i],
      Y = bosque_plant_week_combinations$Y[combination_i],
      Plot = bosque_plant_week_combinations$Bosque[combination_i],
      Plant_sp = bosque_plant_week_combinations$Planta[combination_i],
      period_i = bosque_plant_week_combinations$Periodo[combination_i],
      flora_census) %>% mutate(change_plant_sp=F, 
                               step_ID=pollinator_i_data_clogit_mod$step_ID[1])
    
    # https://stackoverflow.com/questions/35329585/how-to-get-fitted-values-from-clogit-model
    
    risk <- predict(model_pollinator_i,newdata=potential_steps_i, type = 'risk')#,se.fit = TRUE)
    risk/(risk+1)
    
    bosque_plant_week_combinations$prob_consp_step[combination_i] <- 
      as.numeric(risk/(risk+1)) %>% prod()
    
    
  }
  
  
  
  
  # Save model coefficients
  path_save_file <- paste0("results/donana/consp_step_probability_from_",
                           number_random_steps,"_rd_steps_PERIODO.csv")
  
  write_csv(bosque_plant_week_combinations, path_save_file)
  
}

  
# Percentage of NAs in prob_consp_step
sum(is.na(bosque_plant_week_combinations$prob_consp_step))/nrow(bosque_plant_week_combinations) # 0.1612
  