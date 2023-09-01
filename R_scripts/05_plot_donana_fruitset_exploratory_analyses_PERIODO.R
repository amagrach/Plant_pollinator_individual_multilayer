
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

Periodo_info <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  dplyr::select(Day_ISO, Week) %>% unique() %>% arrange(Week)

Periodo_info$Periodo

Periodo_info$Periodo <- 1
Periodo_info$Periodo[Periodo_info$Day_ISO > 90] <- 2
Periodo_info$Periodo[Periodo_info$Day_ISO > 123] <- 3

motifs_list_raw <- read_csv(paste0("results/donana/triplets_week/2021_donana_WEEK_SPECIES.csv")) %>%
  left_join(Periodo_info %>% dplyr::select(-Day_ISO), by = "Week")


motifs_aggregated <- motifs_list_raw %>%
  dplyr::select(-Week,  -Year, -Polinizador) %>%
  group_by(Bosque,Subplot_Plant_Label, Periodo) %>%
  summarise_all(sum)
    

#---------------------------------------
# Load conspecific prob data for Doñana 2021

number_random_steps <- 20
path_save_file <- paste0("results/donana/consp_step_probability_from_",
                         number_random_steps,"_rd_steps_PERIODO.csv")

consp_prob_step <- read_csv(path_save_file)


#------------------------------------------------
# Load flora data

flora_census <- read_csv("results/donana/flora_census_21.csv")

flora_census %>% dplyr::select(Year, Bosque, Periodo, X, Y) %>%
  group_by(Year, Bosque, Periodo, X, Y) %>% count() %>% filter(n>1)

plant_richness_censuses <- flora_census %>% dplyr::select(X, Y, Planta, Bosque, Periodo) %>%
  unique() %>% group_by(X, Y, Bosque, Periodo) %>% count() %>% rename(plant_richness = n)

total_flowers_censuses <- flora_census %>% dplyr::select(X, Y, Flores, Bosque, Periodo) %>%
  group_by(X, Y, Bosque, Periodo) %>% count(wt = Flores) %>% rename(total_number_flowers = n) %>%
  filter(total_number_flowers > 0)

#------------------------------------------------
# Load flora data

flights_raw <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  rename(node = Codigo_within_sequence)


flights_raw$Periodo_hora %>% sort() %>% unique()


# Change variables
flights_raw$time_of_day <- "10:00 - 11:59" 
flights_raw$time_of_day[lubridate::hour(flights_raw$Periodo_hora) >= 12] <- "12:00 - 13:59" 
flights_raw$time_of_day[lubridate::hour(flights_raw$Periodo_hora) >= 14] <- "14:00 - 16:05" 

flights_raw$Periodo <- 1
flights_raw$Periodo[flights_raw$Day_ISO > 90] <- 2
flights_raw$Periodo[flights_raw$Day_ISO > 123] <- 3

pollinator_richness_censuses <- flights_raw %>% dplyr::select(X, Y, Planta, Polinizador, Bosque, Periodo) %>%
  unique() %>% group_by(X, Y, Bosque, Periodo, Planta) %>% count() %>% rename(poll_richness = n)


#---------------------------------------
# Combine fruitset, motif and consp. prob. data for Doñana 2021
data_model_aux <- fruitset_census %>% 
  left_join(motifs_aggregated, 
            by = c("Bosque","Subplot_Plant_Label","Periodo")) %>%
  left_join(consp_prob_step, by = c("Bosque","Planta","X", "Y","Periodo")) %>%
  left_join(pollinator_richness_censuses, by = c("Bosque","Planta","X", "Y","Periodo")) %>%
  left_join(plant_richness_censuses, by = c("Bosque","X", "Y","Periodo")) %>%
  left_join(total_flowers_censuses, by = c("Bosque","X", "Y","Periodo"))

# Sanity check
data_model_aux %>% filter(is.na(Visits_tot)) # 125 missing values 
data_model_aux %>% filter(is.na(prob_consp_step)) # 71 missing values 
data_model_aux %>% filter(is.na(homo_motif)) # 125 missing values 
data_model_aux %>% filter(is.na(poll_richness)) # 131 missing values 
data_model_aux %>% filter(is.na(plant_richness)) # 15 missing values
data_model_aux %>% filter(is.na(total_number_flowers)) # 17 missing values with Week_ISO


# Update period value
data_model_aux$Periodo <- as.character(data_model_aux$Periodo)
data_model_aux$Periodo[data_model_aux$Periodo=="1"] <- "March"
data_model_aux$Periodo[data_model_aux$Periodo=="2"] <- "April"
data_model_aux$Periodo[data_model_aux$Periodo=="3"] <- "May"


#-------------------------------------------------------------------------
# Save data for models
path_save_file <- paste0("results/donana/data for_fruitset_models_from_",
                         number_random_steps,"_rd_steps_PERIODO.csv")

write_csv(data_model_aux, path_save_file)

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
