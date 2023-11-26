

# This code generates random samples for observed steps 

# IMPORTANT: Since some XY combinations apparently do not have the species
# obsered in a given period (floral census), we will not update the rd step info
# on the following variables: flowers_sp1_XY2,flowers_sp2_XY1,flowers_sp2_XY2
# deltaflowers_sp1, deltaflowers_sp2


library(tidyverse)
library(fitdistrplus)

source("R_scripts/aux_functions/set_same_factor_levels_on_flower_data.R")
source("R_scripts/aux_functions/generate_rd_steps_XY5.R")

# Load step data

# When insects enter the plots, we have no info about the turning angle

steps_20_21 <- read_csv("results/gorbea/observed_steps_20_21.csv") %>%
  filter(!is.na(turning_angle)) %>% # We remove those rows without the corresponding turning_angle info
  dplyr::select(-Codigo_captura) %>%
  mutate(delta_X = X2 - X1, delta_Y = Y2 - Y1,
         previous_angle = angle - turning_angle)

# We found NAs in the flora info of several XY combinations. We set those fields
# to zero
steps_20_21[is.na(steps_20_21)] <- 0

# Load flora info to estimate the flora info for rd steps
flora_census <- read_csv("results/gorbea/flora_census_20_21.csv")

# Input data
number_random_steps <- 20

# Extract parameters for main floral visitors

ranking_pollinators <- steps_20_21 %>% group_by(Polinizador) %>% count() %>% arrange(desc(n))
total_number_steps <- nrow(steps_20_21)
percentage_steps_main_pollinators <- 100*nrow(steps_20_21%>% filter(Polinizador %in% ranking_pollinators$Polinizador[1:5]))/total_number_steps

total_pollinator_i_data_clogit <-  NULL

set.seed(1234)

for(pollinator_i in ranking_pollinators$Polinizador[1:5]) {
  
  print(pollinator_i)
  
  steps_pollinator_i <- steps_20_21 %>% filter(Polinizador == pollinator_i, !is.na(turning_angle))
  
  plants_visited_pollinator_i <- c(steps_pollinator_i$Planta1,
                                   steps_pollinator_i$Planta2) %>%
    unique()
  
  # Calculating random steps for conditional logistic regression
  
  pollinator_i_data_clogit <- NULL
  
  step_ID <- 1

  steps_pollinator_i$control <- 1
  
  
  for (row.i in 1:nrow(steps_pollinator_i)) {
    
    steps_pollinator_i_row <- steps_pollinator_i[row.i,]
    
    data_for_step_distribution <- steps_20_21 %>% filter(Polinizador == pollinator_i,
                                                 Periodo ==  steps_pollinator_i_row$Periodo,
                                                 time_of_day ==  steps_pollinator_i_row$time_of_day,
                                                 Bosque == steps_pollinator_i_row$Bosque)
    
    number_observations_i <- data_for_step_distribution %>% nrow()
    
    steps_length_data <- data_for_step_distribution$step_length
    
    p_binomial <- sum(data_for_step_distribution$change_plant_sp)/nrow(data_for_step_distribution)
    
    steps_pollinator_i_row$control <- 1
    
    steps_pollinator_i_year_plot_periodo_change_row <- 
      steps_pollinator_i_row
    
    # Create random steps for each observed step----------
    
    rd_steps_pollinator_i_year_plot_periodo_change_row <- 
      generate_rd_steps_XY5(steps_pollinator_i_year_plot_periodo_change_row,
                            number_random_steps, steps_length_data,
                            p_binomial)
    
    while(nrow(rd_steps_pollinator_i_year_plot_periodo_change_row) < number_random_steps) {
      
      additional_random_steps <- number_random_steps - 
        nrow(rd_steps_pollinator_i_year_plot_periodo_change_row) 
      
      additional_rd_steps_pollinator_i_year_plot_periodo_change_row <-
        generate_rd_steps_XY5(steps_pollinator_i_year_plot_periodo_change_row,
                              additional_random_steps, 
                              steps_length_data,
                              p_binomial)
      
      rd_steps_pollinator_i_year_plot_periodo_change_row <- 
        bind_rows(rd_steps_pollinator_i_year_plot_periodo_change_row,
                  additional_rd_steps_pollinator_i_year_plot_periodo_change_row)
      
    }
    
    # Update the floral info of the random----------
    
    for (i_rd_step in 1:number_random_steps) {
      
      flora_data_rd_step_XY2 <- 
        flora_census %>% filter(X == rd_steps_pollinator_i_year_plot_periodo_change_row$X2[i_rd_step],
                                Y == rd_steps_pollinator_i_year_plot_periodo_change_row$Y2[i_rd_step],
                                Periodo == rd_steps_pollinator_i_year_plot_periodo_change_row$Periodo[i_rd_step],
                                Bosque == rd_steps_pollinator_i_year_plot_periodo_change_row$Bosque[i_rd_step],
                                Year == rd_steps_pollinator_i_year_plot_periodo_change_row$Year[i_rd_step])
      
      
      richness_censuses_rd_step_XY2 <- flora_data_rd_step_XY2 %>%
        unique() %>% group_by(X, Y, Year, Bosque, Periodo) %>% 
        count() %>% rename(richness = n) %>% ungroup() %>%
        dplyr::select(richness) %>% pull()
      
      
      
      if(length(richness_censuses_rd_step_XY2)==0){
        rd_steps_pollinator_i_year_plot_periodo_change_row$richness2[i_rd_step] <-
          0
      }else{
        rd_steps_pollinator_i_year_plot_periodo_change_row$richness2[i_rd_step] <- 
          richness_censuses_rd_step_XY2
      }
      
      rd_steps_pollinator_i_year_plot_periodo_change_row$delta_richness[i_rd_step] <- 
        rd_steps_pollinator_i_year_plot_periodo_change_row$richness2[i_rd_step]-
        rd_steps_pollinator_i_year_plot_periodo_change_row$richness1[i_rd_step]
      
      
      total_flowers_censuses_rd_step_XY2 <- flora_data_rd_step_XY2 %>%
        group_by(X, Y, Year, Bosque, Periodo) %>% count(wt = Flores) %>% 
        rename(total_number_flowers = n) %>% ungroup() %>%
        dplyr::select(total_number_flowers) %>% pull()
      
      if(length(total_flowers_censuses_rd_step_XY2)==0){
        rd_steps_pollinator_i_year_plot_periodo_change_row$total_number_flowers2[i_rd_step] <-
          0
      }else{
        rd_steps_pollinator_i_year_plot_periodo_change_row$total_number_flowers2[i_rd_step] <- 
          total_flowers_censuses_rd_step_XY2
      }
      
      
      rd_steps_pollinator_i_year_plot_periodo_change_row$delta_total_flowers[i_rd_step] <- 
        rd_steps_pollinator_i_year_plot_periodo_change_row$total_number_flowers2[i_rd_step]-
        rd_steps_pollinator_i_year_plot_periodo_change_row$total_number_flowers1[i_rd_step]
      
      
    }
    
    steps_pollinator_i_year_plot_periodo_change_row$step_ID <- step_ID
    rd_steps_pollinator_i_year_plot_periodo_change_row$step_ID <- step_ID
    
    pollinator_i_data_clogit <- bind_rows(pollinator_i_data_clogit,
                                          steps_pollinator_i_year_plot_periodo_change_row,
                                          rd_steps_pollinator_i_year_plot_periodo_change_row)
    
    
    step_ID <- step_ID + 1
    
    
    
  }
  
  total_pollinator_i_data_clogit <- bind_rows(total_pollinator_i_data_clogit,
                                              pollinator_i_data_clogit)
  
  path_save_file <- paste0("results/gorbea/total_pollinator_i_data_clogit_observations_",
                           number_random_steps,"_rd_steps_NEW.csv")
  
  write_csv(total_pollinator_i_data_clogit, path_save_file)
  
}


records_used <- steps_20_21 %>% filter(Polinizador %in% c("Bombus_pascuorum","Apis_mellifera",
                                          "Sphaerophoria_scripta",
                                          "Bombus_lapidarius","Eristalis_sp")) %>%
  nrow()

records_used / nrow(steps_20_21)

# # Identify steps with NAs due to the info in the floral inventories
# steps_with_NAS <- steps_20_21 %>% filter(is.na(richness1)|is.na(richness2))
# write_csv(steps_with_NAS,"steps_with_NAS.csv")
