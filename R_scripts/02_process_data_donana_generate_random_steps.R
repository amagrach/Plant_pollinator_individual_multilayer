

# This code generates random samples for observed steps 

# IMPORTANT: Since some XY combinations apparently do not have the species
# obsered in a given period (floral census), we will not update the rd step info
# on the following variables: flowers_sp1_XY2,flowers_sp2_XY1,flowers_sp2_XY2
# deltaflowers_sp1, deltaflowers_sp2


library(tidyverse)
library(survival)

source("R_scripts/aux_functions/pollinator_model_exp_coef.R")
source("R_scripts/aux_functions/set_same_factor_levels_no_flower_data.R")
source("R_scripts/aux_functions/generate_rd_steps_XY2.R")

# Load step data

# When insects enter the plots, we have no info about the turning angle

steps_21 <- read_csv("results/donana/observed_steps_21.csv") %>%
  filter(!is.na(turning_angle)) %>%
  mutate(delta_X = X2 - X1, delta_Y = Y2 - Y1,
         previous_angle = angle - turning_angle)

# We found NAs in the flora info of several XY combinations. We set those fields
# to zero
steps_21[is.na(steps_21)] <- 0

# Load flora info to estimate the flora info for rd steps
flora_census <- read_csv("results/donana/flora_census_21.csv")

# Input data
number_random_steps <- 5

# Extract parameters for main floral visitors

ranking_pollinators <- steps_21 %>% group_by(Polinizador) %>% count() %>% arrange(desc(n))

total_pollinator_i_data_clogit <-  NULL

set.seed(1234)

for(pollinator_i in ranking_pollinators$Polinizador[1:5]) {
  
  print(pollinator_i)
  
  steps_pollinator_i <- steps_21 %>% filter(Polinizador == pollinator_i, !is.na(turning_angle))
  
  plants_visited_pollinator_i <- c(steps_pollinator_i$Planta1,
                                   steps_pollinator_i$Planta2) %>%
    unique()
  
  # Calculating random steps for conditional logistic regression
  
  pollinator_i_data_clogit <- NULL
  
  step_ID <- 1
  
  # Extract probabilities for lengths and turning angles
  # Fit by randomizing all observed data----
  
  rows_with_turning_angle <- steps_pollinator_i %>%
    filter(!is.na(turning_angle)) %>% nrow()
  
  steps_lenght_turning_table <- table(steps_pollinator_i$step_length, #steps_pollinator_i_year_plot_periodo_change$step_length,
                                      steps_pollinator_i$turning_angle)  %>% #steps_pollinator_i_year_plot_periodo_change$turning_angle) %>% 
    as.data.frame.table() %>% filter(Freq > 0) %>% rename(length = Var1,
                                                          turning_angle = Var2)  %>% 
    mutate(prob = Freq / rows_with_turning_angle)
  
  steps_lenght_turning_table$length <-
    as.numeric(levels(steps_lenght_turning_table$length))[steps_lenght_turning_table$length]
  
  steps_lenght_turning_table$turning_angle <-
    as.numeric(levels(steps_lenght_turning_table$turning_angle))[steps_lenght_turning_table$turning_angle]
  
  p_binomial <- sum(steps_pollinator_i$change_plant_sp)/nrow(steps_pollinator_i)
  
  steps_pollinator_i$control <- 1
  
  
  for (row.i in 1:nrow(steps_pollinator_i)) {
    
    steps_pollinator_i_year_plot_periodo_change_row <- 
      steps_pollinator_i[row.i,]
    
    # Create random steps for each observed step----------
    
    rd_steps_pollinator_i_year_plot_periodo_change_row <- 
      generate_rd_steps_XY2(steps_pollinator_i_year_plot_periodo_change_row,
                            number_random_steps, steps_lenght_turning_table,
                            p_binomial)
    
    while(nrow(rd_steps_pollinator_i_year_plot_periodo_change_row) < number_random_steps) {
      
      additional_random_steps <- number_random_steps - 
        nrow(rd_steps_pollinator_i_year_plot_periodo_change_row) 
      
      additional_rd_steps_pollinator_i_year_plot_periodo_change_row <-
        generate_rd_steps_XY2(steps_pollinator_i_year_plot_periodo_change_row,
                              additional_random_steps, 
                              steps_lenght_turning_table,
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

      
      # flowers_sp_censuses_rd_step_sp1_XY2 <- flora_data_rd_step_XY2 %>%
      #   filter(Planta==
      #            rd_steps_pollinator_i_year_plot_periodo_change_row$Planta1[i_rd_step]) %>%
      #   group_by(X, Y, Year, Bosque, Periodo, Planta) %>% count(wt = Flores) %>% 
      #   rename(flowers_sp = n) %>% ungroup() %>%
      #   dplyr::select(flowers_sp) %>% pull()
      # 
      # if(length(flowers_sp_censuses_rd_step_sp1_XY2)==0){
      #   rd_steps_pollinator_i_year_plot_periodo_change_row$flowers_sp1_XY2[i_rd_step] <-
      #     0
      # }else{
      #   rd_steps_pollinator_i_year_plot_periodo_change_row$flowers_sp1_XY2[i_rd_step] <-
      #     flowers_sp_censuses_rd_step_sp1_XY2
      # }
      # 
      # 
      # rd_steps_pollinator_i_year_plot_periodo_change_row$delta_flowers_sp1[i_rd_step] <-
      #   rd_steps_pollinator_i_year_plot_periodo_change_row$flowers_sp1_XY2[i_rd_step]-
      #   rd_steps_pollinator_i_year_plot_periodo_change_row$flowers_sp1_XY1[i_rd_step]
      
    #   DATA Plant SP2
    #  
    #   if(rd_steps_pollinator_i_year_plot_periodo_change_row$change_plant_sp[i_rd_step] == T){
    #   
    #   
    #   rd_steps_pollinator_i_year_plot_periodo_change_row$change_plant_sp[i_rd_step]
    #   
    #   
    #   flowers_sp_censuses_rd_step <- flora_data_rd_step %>%
    #     group_by(X, Y, Year, Bosque, Periodo, Planta) %>% count(wt = Flores) %>% 
    #     rename(flowers_sp = n) %>% ungroup() %>%
    #     dplyr::select(flowers_sp) %>% pull()
    #   
    #   flowers_sp2_XY2
    #   
    #   
    #   Planta2
    #   
    #   
    #   
    #   flowers_sp2_XY1
    #   
    #   
    # }else{
    #   
    #   flowers_sp_censuses_rd_step <- flora_data_rd_step %>%
    #     group_by(X, Y, Year, Bosque, Periodo, Planta) %>% count(wt = Flores) %>% 
    #     rename(flowers_sp = n) %>% ungroup() %>%
    #     dplyr::select(flowers_sp) %>% pull()
    #   
    # }
    
    
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
  
  path_save_file <- paste0("results/donana/total_pollinator_i_data_clogit_observations_",
                           number_random_steps,"_rd_steps_NEW.csv")
  
  write_csv(total_pollinator_i_data_clogit, path_save_file)
  
}


records_used <- steps_21 %>% filter(Polinizador %in% pull(ranking_pollinators[1:5,1])) %>%
  nrow()

records_used / nrow(steps_21)

# # Identify steps with NAs due to the info in the floral inventories
# steps_with_NAS <- steps_21 %>% filter(is.na(richness1)|is.na(richness2))
# write_csv(steps_with_NAS,"steps_with_NAS.csv")
