
library(tidyverse)
library(lme4)
library(lmerTest)
library(wesanderson)
library(glmmTMB)

# pollinator richness---------------------------------------------------

# Load flight sequences
flights_raw_donana <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  rename(node = Codigo_within_sequence)

flights_raw_donana$X[flights_raw_donana$X==17] <- 7

flights_raw_donana$Periodo_hora %>% sort() %>% unique()

# Fix the values for time of the day and period
flights_raw_donana$time_of_day <- "10:00 - 11:59" 
flights_raw_donana$time_of_day[lubridate::hour(flights_raw_donana$Periodo_hora) >= 12] <- "12:00 - 13:59" 
flights_raw_donana$time_of_day[lubridate::hour(flights_raw_donana$Periodo_hora) >= 14] <- "14:00 - 16:05" 

flights_raw_donana$Periodo <- 1
flights_raw_donana$Periodo[flights_raw_donana$Day_ISO > 90] <- 2
flights_raw_donana$Periodo[flights_raw_donana$Day_ISO > 123] <- 3

flights_raw_donana$Bosque[flights_raw_donana$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"

flights_raw_donana$Bosque <- gsub("Pinar ","",flights_raw_donana$Bosque)
flights_raw_donana$Bosque <- gsub("Villamanrique ","Villam. ",flights_raw_donana$Bosque)

flights_raw_donana$Periodo <- paste0("Period ",flights_raw_donana$Periodo)

MAIN_pollinator_species <- c("Bombus_terrestris","Apis_mellifera",
                             "Xylocopa_cantabrita","Dasypoda_cingulata",
                             "Anthophora_dispar")

plant_sp_visited_MAIN_poll <- flights_raw_donana %>%
  filter(Polinizador %in% MAIN_pollinator_species) %>%
  dplyr::select(Planta) %>% unique() %>% pull() %>% sort()

pollinator_richness_censuses_donana <- flights_raw_donana %>% filter(Planta %in% plant_sp_visited_MAIN_poll) %>%
  dplyr::select(Year, Polinizador, Bosque, Planta, Periodo,X,Y) %>%
  unique() %>% group_by(Year,Planta, Bosque, Periodo,X,Y) %>% count() %>% rename(poll_richness = n)



# Load data--------------------------------------------------------------------
number_random_steps <- 20
path_load_file <- paste0("results/donana/NEW_consp_step_probability_from_",
                         number_random_steps,"_rd_steps.csv")

probability_info <- read_csv(path_load_file)


# Community effectivenes---------------------------------------------------

probability_info$Bosque[probability_info$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"

probability_info$Bosque <- gsub("Pinar ","",probability_info$Bosque)
probability_info$Bosque <- gsub("Villamanrique ","Villam. ",probability_info$Bosque)

probability_info$Periodo <- paste0("Period ",probability_info$Periodo)

probability_info$prob_consp_step[probability_info$prob_consp_step==1] <- 1-1e-4

probability_info_agg <- probability_info %>% filter(Planta %in% plant_sp_visited_MAIN_poll) %>%
  group_by(Bosque,Year,Planta,Periodo,X,Y) %>% summarise_all(mean)



# Combining dataframes---------------------------------------------------

data_model <- pollinator_richness_censuses_donana %>%
  full_join(probability_info_agg, 
            by = c("Bosque","Year","Planta","Periodo","X","Y")) %>%
  filter(!is.na(prob_consp_step),!is.na(poll_richness))

plant_sp_with_more_ind <- data_model %>% group_by(Planta) %>% unique() %>% 
  count() %>% ungroup() %>%
  filter(n>6) %>% dplyr::select(Planta) %>% pull()

ggplot(data_model %>% filter(Planta %in% plant_sp_with_more_ind),
       aes(x=poll_richness,y=log(prob_consp_step),color=Planta))+
  geom_point(size=3, alpha=.2)+
  geom_smooth(method = "lm")+facet_wrap(~Planta)+
  labs(title="Doñana (2021)")

ggplot(data_model %>% filter(Planta %in% plant_sp_with_more_ind),
       aes(x=poll_richness,y=log(prob_consp_step)))+
  geom_point(size=3, alpha=.2)+
  geom_smooth(method = "lm")+
  labs(title="Doñana (2021)")

model_av_eff_av_richness <- glmmTMB((prob_consp_step) ~ scale(poll_richness)+Planta+Periodo+(1|Bosque),
                                    #family = beta_family(link="logit"),
                                    data = data_model %>% filter(Planta %in% plant_sp_with_more_ind))
summary(model_av_eff_av_richness)

write_csv(data_model %>% filter(Planta %in% plant_sp_with_more_ind),"results/donana/comm_eff_VS_poll_rich_data_model.csv")
