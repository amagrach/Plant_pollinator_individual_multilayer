
library(tidyverse)

#---------------------------------------
# Load fruitset data for Doñana 2021
fruitset_census <- read_csv("results/donana/fruitset_census_21.csv") %>%
  mutate(Subplot=paste0(X,"-",Y), fruitset = number_fruits/labelled_flowers)

fruitset_census$Subplot_Plant_Label <- paste(fruitset_census$Subplot,fruitset_census$Planta,
                                                           sep = " ")


# Sanity check
# Number of plants within a subplot that has been sampled multiple times
fruitset_census %>% dplyr::select(Bosque,Planta,X,Y) %>% duplicated() %>% sum() # 224
duplicated_indices_fruitset <- fruitset_census %>% dplyr::select(Bosque,Planta,X,Y) %>% duplicated()
fruitset_census[duplicated_indices_fruitset,]

# Number of plants within a subplot that has been sampled multiple times
fruitset_census %>% dplyr::select(Bosque,Planta,X,Y, Fecha) %>% duplicated() %>% sum() # 30
duplicated_indices_fruitset_by_date <- fruitset_census %>% 
  dplyr::select(Bosque,Planta,X,Y,Fecha) %>% duplicated()
fruitset_census[duplicated_indices_fruitset_by_date,]

#---------------------------------------
# Load motif data for Doñana 2021

week_ISO_info <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  dplyr::select(Week_ISO, Week) %>% unique() %>% arrange(Week)

motifs_list_raw <- read_csv(paste0("results/donana/triplets_week/2021_donana_WEEK_SPECIES.csv")) %>%
  left_join(week_ISO_info, by = "Week")


motifs_aggregated <- motifs_list_raw %>%
  dplyr::select(-Week,  -Year, -Polinizador) %>%
  group_by(Bosque,Subplot_Plant_Label, Week_ISO) %>%
  summarise_all(sum)
    

#---------------------------------------
# Load conspecific prob data for Doñana 2021

number_random_steps <- 20
path_save_file <- paste0("results/donana/consp_step_probability_from_",
                         number_random_steps,"_rd_steps.csv")

consp_prob_step <- read_csv(path_save_file)

#---------------------------------------
# Combine fruitset, motif and consp. prob. data for Doñana 2021
data_model_aux <- fruitset_census %>% 
  left_join(motifs_aggregated, 
            by = c("Bosque","Subplot_Plant_Label","Week_ISO")) %>%
  left_join(consp_prob_step, by = c("Bosque","Planta","X", "Y","Week_ISO","Periodo"))

# Sanity check
data_model_aux %>% filter(is.na(Visits_tot)) # 70 (179) missing values without (with) Week_ISO
data_model_aux %>% filter(is.na(prob_consp_step)) # 114 missing values without (with) Week_ISO
data_model_aux %>% filter(is.na(homo_motif)) # 179 missing values without (with) Week_ISO


# Update period value
data_model_aux$Periodo <- as.character(data_model_aux$Periodo)
data_model_aux$Periodo[data_model_aux$Periodo=="1"] <- "March"
data_model_aux$Periodo[data_model_aux$Periodo=="2"] <- "April"
data_model_aux$Periodo[data_model_aux$Periodo=="3"] <- "May"

# Data visualization

png("figures/donana_fruitset_visits.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_visits_plot <- 
  ggplot(data_model_aux %>% filter(!is.na(Planta)), 
       aes(x = (fruitset), y = (Visits_tot), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(x="Total number of fruits per plant individual", y = "Total number of visits",color=NULL)+
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
       aes(x = (fruitset), y = (Visits_tot), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(x="Total number of fruits per plant individual", y = "Total number of visits",color=NULL)+
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
       aes(x = (fruitset), y = (homo_motif), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(x="Total number of fruits per plant individual", y = "Total number of homo-motifs",color=NULL)+
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
       aes(x = (fruitset), y = (homo_motif), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(x="Total number of fruits per plant individual", y = "Total number of homo-motifs",color=NULL)+
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
       aes(x = (fruitset), y = (hete_motif), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(x="Total number of fruits per plant individual", y = "Total number of hete-motifs",color=NULL)+
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
       aes(x = (fruitset), y = (hete_motif), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(x="Total number of fruits per plant individual", y = "Total number of hete-motifs",color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(override.aes = list(size=5)))
fruitset_hetemotifs_plant_plot
dev.off()

fruitset_hetemotifs_plant_plot


png("figures/donana_fruitset_prob_consp_step.png",
    width = 11.69*1.5, # The width of the plot in inches
    height = 11.69*0.8, units = "in", res=300*2)

fruitset_prob_consp_step_plot <- 
  ggplot(data_model_aux  %>% filter(!is.na(Planta)), 
         aes(x = (fruitset), y = (prob_consp_step), color = as.factor(Periodo)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(x="Total number of fruits per plant individual", y = "Prob. consp. step",color=NULL)+
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
         aes(x = (fruitset), y = (prob_consp_step), color = Planta))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~Periodo)+
  labs(x="Total number of fruits per plant individual", y = "Prob. consp. step",color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(override.aes = list(size=5)))
fruitset_prob_consp_step_plant_plot
dev.off()

fruitset_prob_consp_step_plant_plot
