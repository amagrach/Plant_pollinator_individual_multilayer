
library(tidyverse)

#-------------------------------------------------------------------------
# Load data for models
number_random_steps <- 20
path_load_file <- paste0("results/donana/NEW_data for_fruitset_models_from_",
                         number_random_steps,"_rd_steps.csv")

data_model_aux <- read_csv(path_load_file)

# Data visualization

png("figures/donana_fruitset_visits.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_visits_plot <- 
  ggplot(data_model_aux %>% filter(!is.na(Planta)), 
         aes(y = (fruitset), x = (Visits_tot), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset", x = "Total number of visits",color=NULL)+
  theme_bw()+
  theme(legend.position="none")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))
fruitset_visits_plot

dev.off()

fruitset_visits_plot

png("figures/donana_fruitset_visits_plant.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_visits_plant_plot <- 
  ggplot(data_model_aux %>% filter(!is.na(Planta)), 
         aes(y = (fruitset), x = (Visits_tot), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset", x = "Total number of visits",color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(override.aes = list(size=5)))
fruitset_visits_plant_plot
dev.off()

fruitset_visits_plant_plot

png("figures/donana_fruitset_homomotifs.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_homomotifs_plot <- 
  ggplot(data_model_aux %>% filter(!is.na(Planta)), 
         aes(y = (fruitset), x = (homo_motif), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset", x = "Total number of homo-motifs",color=NULL)+
  theme_bw()+
  theme(legend.position="none")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))
fruitset_homomotifs_plot 
dev.off()

fruitset_homomotifs_plot

png("figures/donana_fruitset_homomotifs_plant.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_homomotifs_plant_plot <- 
  ggplot(data_model_aux %>% filter(!is.na(Planta)), 
         aes(y = (fruitset), x = (homo_motif), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset", x = "Total number of homo-motifs",color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(override.aes = list(size=5)))
fruitset_homomotifs_plant_plot
dev.off()

fruitset_homomotifs_plant_plot

png("figures/donana_fruitset_hetemotifs.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_hetemotifs_plot <- 
  ggplot(data_model_aux  %>% filter(!is.na(Planta)), 
         aes(y = (fruitset), x = (hete_motif), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset", x = "Total number of hete-motifs",color=NULL)+
  theme_bw()+
  theme(legend.position="none")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))
fruitset_hetemotifs_plot
dev.off()

fruitset_hetemotifs_plot


png("figures/donana_fruitset_hetemotifs_plant.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_hetemotifs_plant_plot <- 
  ggplot(data_model_aux  %>% filter(!is.na(Planta)), 
         aes(y = (fruitset), x = (hete_motif), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset", x = "Total number of hete-motifs",color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(override.aes = list(size=5)))
fruitset_hetemotifs_plant_plot
dev.off()

fruitset_hetemotifs_plant_plot





png("figures/donana_fruitset_prop_homomotifs.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_prop_homomotifs_plot <- 
  ggplot(data_model_aux %>% filter(!is.na(Planta)) %>%
           mutate(prop_homo = (homo_motif)/(homo_motif+hete_motif)), 
         aes(y = (fruitset), x = (homo_motif), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset (fruits/plant)", x = "Percentage of homo-motifs (%)",color=NULL)+
  theme_bw()+
  theme(legend.position="none")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))
fruitset_prop_homomotifs_plot 
dev.off()

fruitset_homomotifs_plot

png("figures/donana_fruitset_homomotifs_plant.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_prop_homomotifs_plant_plot <- 
  ggplot(data_model_aux %>% filter(!is.na(Planta)) %>%
           mutate(prop_homo = (homo_motif)/(homo_motif+hete_motif)), 
         aes(y = (fruitset), x = (homo_motif), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset (fruits/plant)", x = "Percentage of homo-motifs (%)",color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(override.aes = list(size=5)))
fruitset_prop_homomotifs_plant_plot
dev.off()

fruitset_prop_homomotifs_plant_plot



png("figures/donana_fruitset_prob_consp_step.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_prob_consp_step_plot <- 
  ggplot(data_model_aux  %>% filter(!is.na(Planta)), 
         aes(y = (fruitset), x = (prob_consp_step), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset (fruits/plant)", x = "Prob. consp. step",color=NULL)+
  theme_bw()+
  theme(legend.position="none")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))
fruitset_prob_consp_step_plot
dev.off()

fruitset_prob_consp_step_plot


png("figures/donana_fruitset_prob_consp_step_plant.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_prob_consp_step_plant_plot <- 
  ggplot(data_model_aux  %>% filter(!is.na(Planta)), 
         aes(y = (fruitset), x = (prob_consp_step), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(y="Fruitset (fruits/plant)", x = "Prob. consp. step",color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(override.aes = list(size=5)))
fruitset_prob_consp_step_plant_plot
dev.off()

fruitset_prob_consp_step_plant_plot