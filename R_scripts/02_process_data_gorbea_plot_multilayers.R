# Load relevant libraries
library(tidyverse)
library(igraph)
library(expm)
source("R_scripts/aux_functions/functions.R")


#Access layers files
dir_ini <- getwd()

#Load data on pollinator visits
interactions_data20 <- read_csv("results/gorbea/foraging_Gorbea_2020.csv")
interactions_data21 <- read_csv("results/gorbea/foraging_Gorbea_2021.csv")

interactions <- bind_rows(interactions_data20,interactions_data21) %>% 
  mutate(Subplot=paste0(X,"_",Y))


pollination <- interactions %>% group_by(Year,Bosque,Subplot,Polinizador,Planta) %>%
  count() %>% rename(Visits_tot = n)

pollination <- pollination %>% mutate(Subplot_Plant_Label=paste(Planta,Subplot,sep=" "))
  
stationary_prob_final <- NULL # To storage info length and number of modules

for (Year.i in unique(pollination$Year)){
  
  for (Bosque_i in 1:5){
    
    ##########################
    #ESTIMATE PHENOLOGY
    ##########################
    
    #Filter pollination data
    pollination_year_i <- pollination %>% filter(Year==Year.i,!is.na(Planta))
    
    
    ###########################
    # CREATE MULTILAYER FOR Bosque_i
    ###########################
    
    folder_base <- paste0(dir_ini,"/results/gorbea/Multilayer_Species/")
    
    files_base <- list.files(folder_base)
    
    setwd(folder_base)
    
    # Extract layer files for Bosque_i
    
    list_files_field_level <- files_base[grepl(paste("Bosque_",Bosque_i,sep = ""), files_base) &
                                           grepl(Year.i, files_base) ]
    
    # Extract edge_list for each layer
    for (i in 1:length(list_files_field_level)){
      
      # Extract the incidence matrix
      inc_matrix <- read.csv(list_files_field_level[i], header=T, row.names=1)
      
      # Create a graph for each layer
      g_i <- graph_from_incidence_matrix(inc_matrix, directed = FALSE, weighted = T)
      
      # Get the edge_list from the graph and add plant (layer) information
      Planta <- strsplit(list_files_field_level[i],".csv")
      Planta <- strsplit(Planta[[1]][1],".layer_")
      Planta <- Planta[[1]][2]
      
      g_i_edge_list <- as_tibble(igraph::as_data_frame(g_i, 'edges')) %>% mutate(species=Planta)
      
      if (i==1){
        Bosque_edge_list <- g_i_edge_list
      }
      else{
        Bosque_edge_list <- Bosque_edge_list %>% bind_rows(g_i_edge_list)
      }
    }
    
    # Extract multilayer info
    
    Polinizadores <- sort(unique(Bosque_edge_list$to)) 
    Plantas <- sort(unique(Bosque_edge_list$from))
    layer_Planta <- sort(unique(Bosque_edge_list$species))
    intersect(Polinizadores, Plantas)
    A <- length(Polinizadores) # Number of pollinators
    P <- length(Plantas) # Number of plants
    S <- A + P
    
    # Create a table with node metadata
    physical_nodes <- tibble(node_id=1:S,
                             type=c(rep('Planta',P),rep('Polinizador',A)),
                             species=c(Plantas,Polinizadores))
    layer_metadata <- tibble(layer_id=1:length(layer_Planta), layer_name=layer_Planta)
    
    # Replace the node names with node_ids
    
    Bosque_edgelist_complete <- tibble(layer_from=Bosque_edge_list$species,
                                       node_from=Bosque_edge_list$from,
                                       layer_to=Bosque_edge_list$species,
                                       node_to=Bosque_edge_list$to,
                                       weight=Bosque_edge_list$weight)
    
    ##########
    Planta_strength <- Bosque_edgelist_complete %>% group_by(layer_from,node_from) %>% 
      count(wt = weight) %>% rename(strength = n)
    
    Polinizador_strength <- Bosque_edgelist_complete %>% group_by(layer_from,node_to) %>% 
      count(wt = weight) %>% rename(strength = n)
    ##########
    
    #Create the scaled directed list (previous list was meant to be undirected)
    
    #From plant to pollinator
    
    S_Links_Planta_Poll <- Bosque_edgelist_complete %>% left_join(Planta_strength,
                                                                  by=c("layer_from","node_from")) %>%
      mutate(weight=weight/strength) %>% select(-strength)
    
    S_Links_Poll_Planta <- Bosque_edgelist_complete %>% left_join(Polinizador_strength,
                                                                  by=c("layer_from","node_to")) %>%
      
      mutate(weight=weight/strength) %>% select(-strength) %>%
      rename(node_from=node_to,node_to=node_from)
    
    
    S_edge_list <- bind_rows(S_Links_Planta_Poll,S_Links_Poll_Planta)
    
    ###############
    # To create the inter-links we rely on the previous Bosque_edgelist_complete
    # Here we can extract information on interlayer connections
    
    for (i in 1:length(Polinizadores)){
      
      polinator_edges <- Bosque_edgelist_complete %>% filter(node_to==Polinizadores[i])
      polinator_layers <- unique(polinator_edges$layer_to)
      
      if (length(polinator_layers)>1){
        combination_layers <- t(combn(polinator_layers, 2))
        for (j in 1:nrow(combination_layers)){
          
          #For directed networks
          interlink_i<- tibble(layer_from=c(combination_layers[j,1],combination_layers[j,2]),
                               node_from=c(Polinizadores[i],Polinizadores[i]),
                               layer_to=c(combination_layers[j,2],combination_layers[j,1]),
                               node_to=c(Polinizadores[i],Polinizadores[i]),
                               weight=c(1,1))
          
          
          #For directed
          S_edge_list <- bind_rows(S_edge_list,interlink_i)
        }
      }
    }
    
    S_edge_list_i <- S_edge_list %>% mutate(Bosque=Bosque_i)
    
    interlinks_Bosque_i <- S_edge_list %>% filter(node_from==node_to)
    
    if(nrow(interlinks_Bosque_i)==0){ #If there are no interlinks, we create a dummy one
      
      list_possible_layers <- layer_metadata$layer_name[layer_metadata$layer_name!=S_edge_list_i$layer_from[1]]
      
      new_row <- tibble(
        layer_from=list_possible_layers,
        node_from=S_edge_list_i$node_to[1],
        layer_to=S_edge_list_i$layer_to[1],
        node_to=S_edge_list_i$node_to[1],
        weight=0.0,
        Bosque=S_edge_list_i$Bosque[1],
      )
      
      S_edge_list <- bind_rows(S_edge_list,new_row)
      
    }
    
    #######################################
    # CREATE NETWORK OF NETWORKS LIST
    #######################################
    
    NN_edge_list <- S_edge_list %>% filter(weight>0)
    
    for (i in 1:nrow(NN_edge_list)){
      
      if(NN_edge_list$node_from[i] %in% Polinizadores){
        NN_edge_list$node_from[i] <- paste0(NN_edge_list$node_from[i]," ",NN_edge_list$layer_from[i])
      }
      
      if(NN_edge_list$node_to[i] %in% Polinizadores){
        NN_edge_list$node_to[i] <- paste0(NN_edge_list$node_to[i]," ",NN_edge_list$layer_to[i])
      }
      
    }
    
    NN_edge_list_final <- NN_edge_list %>% select(node_from,node_to,weight) %>%
      rename(from = node_from, to = node_to)
    
    #############################
    
    setwd(dir_ini)
    
    graph_Plot_i <- igraph::graph_from_edgelist(as.matrix(NN_edge_list_final[,1:2]), directed = TRUE)
    
    E(graph_Plot_i)$weight <- pull(NN_edge_list_final[,3])
    
    # Sanity check: pull nodes and edge weights
    igraph::get.data.frame(graph_Plot_i)
    ##############
    # PAgeRAnk Multilayer
    saveRDS(graph_Plot_i, file = paste0("results/gorbea/NN_networks/Bosque_",Bosque_i,"_Year_",Year.i,"_NN_intra_inter.rds"))
    
    
  }
  
}

