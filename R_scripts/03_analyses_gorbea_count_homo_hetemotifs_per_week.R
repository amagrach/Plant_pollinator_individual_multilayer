# load libraries
library(tidyverse)
library(bipartite)
library(matlib)
library(igraph)

source("R_scripts/aux_functions/functions.R")

####################################################################
# Loadind Plant-pollinator dataset (Caracoles) for 2020: visits, abundances, seeds
####################################################################

fitness_data20 <- read_csv("results/gorbea/foraging_Gorbea_2020.csv")
fitness_data21 <- read_csv("results/gorbea/foraging_Gorbea_2021.csv")

fitness2 <- bind_rows(fitness_data20,fitness_data21) %>% 
  mutate(Subplot=paste0(X,"-",Y))



########################################################################
# ESTIMATING Gorbeas' homo + hetero motifs (without considering period)
########################################################################

for (year_i in unique(fitness2$Year)){
  
  fitness_year_i <- fitness2 %>% filter(Year == year_i)
  
  sampled_weeks <- sort(unique(fitness_year_i$Week))
  
  for (week_i in sampled_weeks){
    
    cat(year_i," WEEK: ",week_i,"\n")
    
    fitness_week_i <- fitness_year_i %>% filter(Week == week_i)

    fitness_week_i <- fitness_week_i %>% group_by(Bosque,Subplot,Planta,Polinizador) %>%
      count() %>% rename(Visits_tot = n)
    
    fitness_week_i$Subplot_Plant_Label <- paste(fitness_week_i$Subplot,fitness_week_i$Planta,
                                                sep = " ")
    
    visit_list_week <- fitness_week_i %>% ungroup() %>% select(Bosque,
                                                               Polinizador,
                                                               Subplot_Plant_Label,
                                                               Visits_tot)
    
    #x <- motifs_extraction(visit_list_week)
    
    visit_list_week <- homo_hete_motifs(visit_list_week)
    visit_list_week <- visit_list_week %>% mutate(Week=week_i,Year=year_i)
    
    if (week_i==min(fitness_year_i$Week)){
      visit_list <- visit_list_week
    }else{
      visit_list <- visit_list %>% bind_rows(visit_list_week) 
    }
  }
  
  write_csv(visit_list, paste0("results/gorbea/triplets_week/",year_i,
                               "_Gorbea_WEEK_SPECIES.csv"))
  
}



