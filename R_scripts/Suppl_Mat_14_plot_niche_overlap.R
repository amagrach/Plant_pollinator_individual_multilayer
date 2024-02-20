

# We calculated it as the average overlap in pollinator species visiting a focal plant and
# each of the other plants in the community using the Morisita overlap index, a measure of similarity
# between two sets of data (Zhang 2016)

## niche.overlap.boot() example

library(spaa)
library(tidyverse)


####################################################################
# Loadind Plant-pollinator dataset (Caracoles): visits, abundances, seeds
####################################################################
foraging_data_raw <- read_csv("results/Donana/foraging_Donana_2021.csv") %>% 
  mutate(Subplot=paste0(X,"_",Y))


foraging <- foraging_data_raw %>% group_by(Week_ISO,Polinizador,Planta) %>%
  count() %>% rename(Visits_tot = n)


###########################################
#Plants-interactions: Generating a bipartite network for each plot
###########################################

list_weeks_iso <- unique(foraging$Week_ISO) %>% sort()

result_Halimium_calycinum <- NULL
result_Cistus_ladanifer <- NULL

for(Week_aux in list_weeks_iso){
  
  foraging_data <- foraging %>% filter(Week_ISO==Week_aux)
  
  incidence_mat <- table(foraging_data$Polinizador, foraging_data$Planta)
  
  if(ncol(incidence_mat)>1){ # there should be more than 1 species to estimate the overlap
    
    # A distance matrix contains niche overlap index between each pair of species
    results_week_iso_mat <- niche.overlap(incidence_mat, method = "morisita") %>%  as.matrix()
    results_week_iso <- results_week_iso_mat %>% as_tibble()
    colnames(results_week_iso) <- sub("_"," ",colnames(results_week_iso))
    rownames(results_week_iso) <- colnames(results_week_iso)
    
    if("Halimium calycinum" %in% colnames(results_week_iso)){
      result_Halimium_calycinum_week_iso <- results_week_iso["Halimium calycinum",] %>% 
        mutate(Week_ISO=as.numeric(Week_aux))
      result_Halimium_calycinum <- bind_rows(result_Halimium_calycinum, result_Halimium_calycinum_week_iso)
    }
    
    if("Cistus ladanifer" %in% colnames(results_week_iso)){
      result_Cistus_ladanifer_week_iso <- results_week_iso["Cistus ladanifer",] %>% 
        mutate(Week_ISO=as.numeric(Week_aux))
      result_Cistus_ladanifer <- bind_rows(result_Cistus_ladanifer, result_Cistus_ladanifer_week_iso)
    }
    
  }
  
}



df_long_Halimium_calycinum <- result_Halimium_calycinum %>% 
  pivot_longer(
    cols = -Week_ISO, # Excluye Week_ISO de la transformación
    names_to = "Especie",
    values_to = "Valor"
  )


df_long_Cistus_ladanifer <- result_Cistus_ladanifer %>% 
  pivot_longer(
    cols = -Week_ISO, # Excluye Week_ISO de la transformación
    names_to = "Especie",
    values_to = "Valor"
  )

library(RColorBrewer)

colors_set1 <- brewer.pal(9, "Set1")
colors_set2 <- brewer.pal(8, "Set2")
colors_set3 <- brewer.pal(8, "Set3")

combined_colors <- c(colors_set3[1:3],colors_set1, colors_set2[1:3])

Plant_cat_Halimium_calycinum <- df_long_Halimium_calycinum %>%
  filter(Especie!="Halimium calycinum",!is.na(Especie)) %>%
  dplyr::select(Especie) %>% unique() %>% pull()

Plant_cat_Cistus_ladanifer <- df_long_Cistus_ladanifer %>%
  filter(Especie!="Cistus_ladanifer",!is.na(Especie)) %>%
  dplyr::select(Especie) %>% unique() %>% pull()

colors4plots_Halimium_calycinum <- setNames(combined_colors[1:length(Plant_cat_Halimium_calycinum)], Plant_cat_Halimium_calycinum)
colors4plots_Cistus_ladanifer <- setNames(combined_colors[1:length(Plant_cat_Cistus_ladanifer)], Plant_cat_Cistus_ladanifer)


plot_Halimium_calycinum <- ggplot(df_long_Halimium_calycinum %>% filter(Especie!="Halimium calycinum",
                                             !is.na(Valor),
                                             Valor !=0), aes(x = Week_ISO, y = Valor, group = Especie, color = Especie)) +
  #geom_line(linewidth=1.5) + 
  geom_point(aes(shape = Especie, fill = Especie),size=5, alpha= 0.5) +
  scale_shape_manual(values = c(16, 17, 15, 23)) +
  scale_color_manual(values = colors4plots_Halimium_calycinum)+
  scale_fill_manual(values = colors4plots_Cistus_ladanifer)+
  theme_bw()+
  labs(x = "Week", y = "Morisita index of niche overlap", title = "Halimium calycinum") +
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "italic",size = 17),
        legend.text = element_text(face = "italic",size = 15), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))

plot_Cistus_ladanifer <- ggplot(df_long_Cistus_ladanifer %>% filter(Especie!="Cistus ladanifer",
                                             !is.na(Valor),
                                             Valor !=0), aes(x = Week_ISO, y = Valor, group = Especie, color = Especie)) +
  #geom_line(linewidth=1.5) + 
  geom_point(aes(shape = Especie, fill = Especie),size=5, alpha= 0.5) +
  scale_color_manual(values = colors4plots_Cistus_ladanifer)+
  scale_fill_manual(values = colors4plots_Cistus_ladanifer)+
  scale_shape_manual(values = c(16, 17, 15, 23,24, 25)) +
  theme_bw()+
  labs(x = "Week", y = "Morisita index of niche overlap", title = "Cistus ladanifer") +
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "italic",size = 17),
        legend.text = element_text(face = "italic",size = 15), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))


library(patchwork)
png("figures/morisita_index_week.png", width=1000*4.5, height = 1000*5, res=300*2)
plot_Cistus_ladanifer/plot_Halimium_calycinum
dev.off()
