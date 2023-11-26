

library(tidyverse)
library(survival)
library(car)

source("R_scripts/aux_functions/pollinator_model_exp_coef.R")
source("R_scripts/aux_functions/set_same_factor_levels_on_flower_data.R")
source("R_scripts/aux_functions/potential_steps_to_subplot.R")

#---------------------------------------
# Load random steps data for Doñana 2021
number_random_steps <- 20

data_path_file <- paste0("results/gorbea/total_pollinator_i_data_clogit_observations_",
                         number_random_steps,"_rd_steps_UPDATED_RANDOM_FIDELITY.csv")

pollinator_i_data_clogit <- read_csv(data_path_file) %>% filter(Year==2021)

Bosque_list <- unique(pollinator_i_data_clogit$Bosque)

#---------------------------------------
# Load floral data for Doñana 2021
flora_census <- read_csv("results/gorbea/flora_census_20_21.csv") %>%
  dplyr::select(Bosque, Year, Planta,X,Y,Periodo,Flores) %>% unique()


#---------------------------------------
# Create variables to safe results

probability_info <- NULL

#---------------------------------------
# Fit model for all pollinators in a given site

set.seed(1234)

for(Bosque_i in Bosque_list){
  
  pollinator_i_data_clogit_within_field <- pollinator_i_data_clogit %>% filter(Bosque==Bosque_i)
  
  pollinator_i_data_clogit_mod <- set_same_factor_levels_on_flower_data_donana(pollinator_i_data_clogit_within_field)
  
  pollinator_i_data_clogit_mod$step_length <- pollinator_i_data_clogit_mod$step_length
  pollinator_i_data_clogit_mod$delta_richness <- scale(pollinator_i_data_clogit_mod$delta_richness)
  pollinator_i_data_clogit_mod$delta_total_flowers <- scale(pollinator_i_data_clogit_mod$delta_total_flowers)
  
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
                               control = coxph.control(iter.max = 1e4),
                               pollinator_i_data_clogit_mod)
  
  
    print(summary(model_pollinator_i))
    print(performance::check_collinearity(model_pollinator_i,component = "conditional"))
    
    flora_census_Bosque_i <- flora_census %>% filter(Bosque==Bosque_i) %>%
      mutate(prob_consp_step=NA)
      
    
    for(individual_i in 1:nrow(flora_census_Bosque_i)){
      
      potential_steps_i <- potential_steps_to_subplot_period(
        X = flora_census_Bosque_i$X[individual_i],
        Y = flora_census_Bosque_i$Y[individual_i],
        Plot = flora_census_Bosque_i$Bosque[individual_i],
        Plant_sp = flora_census_Bosque_i$Planta[individual_i],
        Year = flora_census_Bosque_i$Year[individual_i],
        flora_census_Bosque_i,
        period_i = flora_census_Bosque_i$Periodo[individual_i]) %>% 
        mutate(change_plant_sp=F,
               step_ID=pollinator_i_data_clogit_mod$step_ID[1])
      
      # https://stackoverflow.com/questions/35329585/how-to-get-fitted-values-from-clogit-model
      
      risk <- predict(model_pollinator_i,newdata=potential_steps_i, type = 'risk')#,se.fit = TRUE)
      risk/(risk+1)
      
      flora_census_Bosque_i$prob_consp_step[individual_i] <- 
        as.numeric(risk/(risk+1)) %>% mean()
      
      
    }
    
    flora_census_Bosque_i_without_NANs <- flora_census_Bosque_i %>% filter(!is.na(prob_consp_step))
    
    probability_info <- bind_rows(probability_info, flora_census_Bosque_i_without_NANs)
  
}

# Save model coefficients
path_save_file <- paste0("results/gorbea/NEW_consp_step_probability_from_",
                         number_random_steps,"_rd_steps_2021.csv")

write_csv(probability_info, path_save_file)
