
library(tidyverse)

#---------------------------------------
# Load fruitset data for Doñana 2021

fruitset_census <- read_csv("results/donana/fruitset_census_21.csv")  %>% filter(!is.na(Planta))

plant_subset_seeds <- c("Cistus_salviifolius","Cistus_crispus", 
                        "Cistus_ladanifer", "Cistus_libanotis", "Halimium_halimifolium",
                        "Halimium_calycinum","Lavandula_stoechas")

unique(fruitset_census$Planta)

sum(fruitset_census$labelled_flowers[fruitset_census$Planta %in% plant_subset_seeds])

sum(fruitset_census$labelled_flowers[fruitset_census$Planta %in% plant_subset_seeds])/sum(fruitset_census$labelled_flowers)

fruitset_census %>% group_by(Planta,Periodo) %>% count(wt = labelled_flowers)
fruitset_census %>% group_by(Planta) %>% count(wt = labelled_flowers)

fruitset_census <- read_csv("results/donana/fruitset_census_21.csv") %>% filter(!is.na(Planta)) %>%
  mutate(Subplot=paste0(X,"-",Y), fruitset = number_fruits/labelled_flowers)

fruitset_census$Subplot_Plant_Label <- paste(fruitset_census$Subplot,fruitset_census$Planta,
                                                           sep = " ")


# Sanity check
# Number of plants within a subplot that has been sampled multiple times
fruitset_census %>% dplyr::select(Bosque,Planta,X,Y) %>% duplicated() %>% sum() # 223
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

# We add the contributions of all pollinators
motifs_per_week <- motifs_list_raw %>%
  dplyr::select(-Week,  -Year, -Polinizador) %>% 
  group_by(Bosque,Subplot_Plant_Label,Week_ISO) %>% 
  summarise(Visits_tot = sum(Visits_tot),
            homo_motif = sum(homo_motif), 
            hete_motif = sum(hete_motif))

# motifs_per_week %>% duplicated()
# duplicated_indices_motifs_per_week <- motifs_per_week %>% duplicated()
# duplicated_motifs <- motifs_per_week[duplicated_indices_motifs_per_week,]
# 
# unique(duplicated_motifs)


motifs_aggregated <- motifs_per_week %>% unique()

motifs_per_week$Week_ISO <- as.numeric(motifs_per_week$Week_ISO)

for(i in 1:nrow(motifs_per_week)){
  
  previous_motifs <- motifs_per_week %>%
    filter(Subplot_Plant_Label == motifs_per_week$Subplot_Plant_Label[i],
           Week_ISO <= motifs_per_week$Week_ISO[i],
           Bosque == motifs_per_week$Bosque[i])
  
  if(nrow(previous_motifs)>1){
    motifs_aggregated$Visits_tot[i] <- sum(previous_motifs$Visits_tot)
    motifs_aggregated$homo_motif[i] <- sum(previous_motifs$homo_motif)
    motifs_aggregated$hete_motif[i] <- sum(previous_motifs$hete_motif)
  }
  
}

total_motifs <- unique(motifs_aggregated)

#---------------------------------------
# Load conspecific prob data for Doñana 2021

number_random_steps <- 20
path_save_file <- paste0("results/donana/NEW_consp_step_probability_from_",
                         number_random_steps,"_rd_steps.csv")

consp_prob_step <- read_csv(path_save_file) %>%
  dplyr::select(-Flores) %>% unique()


#------------------------------------------------
# Create plant_richness_censuses and total_flowers_censuses

# Load flora census
flora_census <- read_csv("results/donana/flora_census_21.csv")

flora_census %>% dplyr::select(Year, Bosque, Periodo, Week_ISO, X, Y) %>%
  group_by(Year, Bosque, Periodo, Week_ISO, X, Y) %>% count() %>% filter(n>1)

plant_richness_censuses <- flora_census %>% dplyr::select(X, Y, Planta, Bosque, Periodo, Week_ISO) %>%
  unique() %>% group_by(X, Y, Bosque, Periodo, Week_ISO) %>% count() %>% rename(plant_richness = n)

total_flowers_censuses <- flora_census %>% dplyr::select(X, Y, Flores, Bosque, Periodo, Week_ISO) %>%
  group_by(X, Y, Bosque, Periodo, Week_ISO) %>% count(wt = Flores) %>% rename(total_number_flowers = n) %>%
  filter(total_number_flowers > 0)

#------------------------------------------------
# Create pollinator_richness_censuses


# Load flight sequences
flights_raw <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  rename(node = Codigo_within_sequence)

flights_raw$Periodo_hora %>% sort() %>% unique()

# Fix the values for time of the day and period
flights_raw$time_of_day <- "10:00 - 11:59" 
flights_raw$time_of_day[lubridate::hour(flights_raw$Periodo_hora) >= 12] <- "12:00 - 13:59" 
flights_raw$time_of_day[lubridate::hour(flights_raw$Periodo_hora) >= 14] <- "14:00 - 16:05" 

flights_raw$Periodo <- 1
flights_raw$Periodo[flights_raw$Day_ISO > 90] <- 2
flights_raw$Periodo[flights_raw$Day_ISO > 123] <- 3

pollinator_richness_censuses <- flights_raw %>% dplyr::select(X, Y, Planta, Polinizador, Bosque, Periodo,Week_ISO) %>%
  unique() %>% group_by(X, Y, Bosque, Periodo, Planta,Week_ISO) %>% count() %>% rename(poll_richness = n)

#---------------------------------------
# Number of unloyal steps

# Load step data
steps_flora_data <- read_csv("results/donana/observed_steps_21.csv")

# Select unloyal steps
unloyal_steps <- steps_flora_data %>% filter(change_plant_sp==T)

number_unloyal_steps_per_week <- unloyal_steps %>% 
  group_by(X2,Y2,Planta2,Week_ISO,Bosque) %>% count() %>% 
  rename(unloyal_steps_per_week=n,
         X = X2,
         Y = Y2,
         Planta = Planta2)

number_unloyal_steps <- number_unloyal_steps_per_week %>% 
  rename(unloyal_steps = unloyal_steps_per_week)


number_unloyal_steps_per_week$Week_ISO <- as.numeric(number_unloyal_steps_per_week$Week_ISO)

for(i in 1:nrow(number_unloyal_steps )){
  
  previous_unloyal_steps <- number_unloyal_steps_per_week %>%
    filter(X == number_unloyal_steps_per_week$X[i],
           Y == number_unloyal_steps_per_week$Y[i],
           Planta == number_unloyal_steps_per_week$Planta[i],
           Week_ISO <= number_unloyal_steps_per_week$Week_ISO[i],
           Bosque == number_unloyal_steps_per_week$Bosque[i])
  
  if(nrow(previous_unloyal_steps)>1){
    number_unloyal_steps$unloyal_steps[i] <- sum(previous_unloyal_steps$unloyal_steps_per_week)
  }
  
}

#------------------------------------------------
# Create pollinator_abundance_censuses

pollinator_abundance_censuses <- flights_raw %>% dplyr::select(X, Y, Codigo_vuelo, Bosque, Periodo,Week_ISO) %>%
  unique() %>% group_by(X, Y, Bosque, Periodo, Week_ISO) %>% count() %>% rename(poll_abundance = n)


#---------------------------------------
# Combine fruitset, motif and consp. prob. data for Doñana 2021
data_model_aux <- fruitset_census %>% 
  left_join(total_motifs, 
            by = c("Bosque","Subplot_Plant_Label","Week_ISO")) %>%
  left_join(consp_prob_step, by = c("Bosque","Planta","X", "Y","Periodo","Year")) %>%
  left_join(pollinator_richness_censuses, by = c("Bosque","Planta","X", "Y","Week_ISO","Periodo")) %>%
  left_join(plant_richness_censuses, by = c("Bosque","X", "Y","Week_ISO","Periodo")) %>%
  left_join(total_flowers_censuses, by = c("Bosque","X", "Y","Week_ISO","Periodo")) %>%
  left_join(number_unloyal_steps, by = c("Planta","Bosque","X", "Y","Week_ISO"))  %>%
  left_join(pollinator_abundance_censuses, by = c("Bosque","X", "Y","Week_ISO","Periodo"))

# Sanity check
data_model_aux %>% filter(is.na(Visits_tot)) # 177 missing values without (with) Week_ISO
data_model_aux %>% filter(is.na(prob_consp_step)) # 25 missing values without (with) Week_ISO
data_model_aux %>% filter(is.na(homo_motif)) # 177 missing values without (with) Week_ISO
data_model_aux %>% filter(is.na(poll_richness)) # 193 missing values with Week_ISO
data_model_aux %>% filter(is.na(plant_richness)) # 22 missing values with Week_ISO
data_model_aux %>% filter(is.na(total_number_flowers)) # 22 missing values with Week_ISO
data_model_aux %>% filter(is.na(poll_abundance)) # 177 missing values with Week_ISO


# Update period value
data_model_aux$Periodo <- as.character(data_model_aux$Periodo)
data_model_aux$Periodo[data_model_aux$Periodo=="1"] <- "March"
data_model_aux$Periodo[data_model_aux$Periodo=="2"] <- "April"
data_model_aux$Periodo[data_model_aux$Periodo=="3"] <- "May"


#-------------------------------------------------------------------------
# Save data for models
path_save_file <- paste0("results/donana/NEW_data for_fruitset_models_from_",
                         number_random_steps,"_rd_steps.csv")

write_csv(data_model_aux, path_save_file)

