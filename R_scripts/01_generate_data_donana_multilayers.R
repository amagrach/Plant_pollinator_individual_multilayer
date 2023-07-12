
#load libraries
library(tidyverse)
library(bipartite)
library(matlib)

####################################################################
# Loadind Plant-pollinator dataset: visits, abundances, seeds
####################################################################
fitness_data20 <- read_csv("results/donana/foraging_Donana_2020.csv") %>%
  mutate(Codigo_vuelo = as.character(Codigo_vuelo))
fitness_data21 <- read_csv("results/donana/foraging_Donana_2021.csv")

fitness2 <- bind_rows(fitness_data20,fitness_data21) %>% 
  mutate(Subplot=paste0(X,"-",Y)) %>% mutate(Polinizador = paste0(Polinizador,"_",Codigo_vuelo))


fitness <- fitness2 %>% group_by(Year,Bosque,Subplot,Polinizador,Planta) %>%
  count() %>% rename(Visits_tot = n)

fitness <- fitness %>% mutate(Subplot_Plant_Label=paste(Subplot,Planta,sep=" "))

visits_pol <- fitness %>% group_by(Year,Polinizador) %>% count()

###########################################
#Plants-interactions: Generating a bipartite network for each plot
###########################################

Year_vector <- fitness$Year %>% unique() %>% sort()
Bosque_vector <- fitness$Bosque %>% unique() %>% sort()

for (Year.i in Year_vector){
  for (Bosque.i in Bosque_vector){
    
    fitness_data <- fitness %>% filter(Bosque == Bosque.i, Year == Year.i)
    
    if(nrow(fitness_data)>1){
      multilayer_bosque <-   data.frame(higher = fitness_data$Polinizador,
                                        lower = fitness_data$Subplot_Plant_Label,
                                        webID = fitness_data$Planta,
                                        freq = fitness_data$Visits_tot)
      
      list_incid_matrix <- frame2webs(multilayer_bosque,type.out="list")
      
      for (j in 1:length(list_incid_matrix)){
        
        plant_i <- j
        print(names(list_incid_matrix)[plant_i])
        incid_matrix_i <- list_incid_matrix[[plant_i]] 
        
        layer_csv <- paste("results/donana/multilayer_species/",Year.i,"_Bosque_",Bosque.i,"_layer_",names(list_incid_matrix)[plant_i],".csv",sep="")
        write.csv(incid_matrix_i,layer_csv)
      }
    }
    
    
  }
}


