
library(tidyverse)
library(lme4)
library(lmerTest)
library(wesanderson)
library(glmmTMB)

# pollinator richness---------------------------------------------------

# Load flight sequences
flights_raw_gorbea_20 <- read_csv("results/gorbea/foraging_Gorbea_2020.csv")
flights_raw_gorbea_21 <- read_csv("results/gorbea/foraging_Gorbea_2021.csv")


flights_raw_gorbea <- bind_rows(flights_raw_gorbea_20,flights_raw_gorbea_21)

flights_raw_gorbea$Periodo_hora %>% sort() %>% unique()
flights_raw_gorbea$Periodo <- 1
flights_raw_gorbea$Periodo[flights_raw_gorbea$Year==2020 & flights_raw_gorbea$Day_ISO > 149] <- 2
flights_raw_gorbea$Periodo[flights_raw_gorbea$Year==2021 & flights_raw_gorbea$Day_ISO > 127] <- 2
flights_raw_gorbea$Periodo[flights_raw_gorbea$Year==2021 & flights_raw_gorbea$Day_ISO > 152] <- 3

# We need to fix the numbering of the flowering period in Gorbea
flights_raw_gorbea$Periodo[flights_raw_gorbea$Year==2020] <- 
  flights_raw_gorbea$Periodo[flights_raw_gorbea$Year==2020] + 1



MAIN_pollinator_species <- c("Bombus_pascuorum","Apis_mellifera",
                        "Bombus_lapidarius","Eristalis_sp",
                        "Sphaerophoria_scripta")

plant_sp_visited_MAIN_poll <- flights_raw_gorbea %>%
  filter(Polinizador %in% MAIN_pollinator_species) %>%
  dplyr::select(Planta) %>% unique() %>% pull() %>% sort()

pollinator_richness_censuses_gorbea <- flights_raw_gorbea %>% filter(Planta %in% plant_sp_visited_MAIN_poll) %>%
  dplyr::select(Year, Bosque, Periodo, Planta, Polinizador, X, Y) %>%
  unique() %>% group_by(Year, Bosque, Periodo, Planta, X, Y) %>% count() %>% rename(poll_richness = n)

pollinator_richness_censuses_gorbea %>% filter(Planta=="Polygala_vulgaris")

# Load data--------------------------------------------------------------------
number_random_steps <- 20
path_load_file <- paste0("results/gorbea/NEW_consp_step_probability_from_",
                         number_random_steps,"_rd_steps_2021.csv")

probability_info <- read_csv(path_load_file)


# Community effectivenes---------------------------------------------------

probability_info$prob_consp_step[probability_info$prob_consp_step==1] <- 1-1e-4

probability_info_agg <- probability_info %>% 
  filter(Planta %in% plant_sp_visited_MAIN_poll) %>%
  dplyr::select(-Flores) %>%
  group_by(Bosque, Year, Planta, Periodo, X, Y) %>% summarise_all(mean)


probability_info_agg %>% filter(Planta=="Polygala_vulgaris")

# Combining dataframes---------------------------------------------------


data_model <- pollinator_richness_censuses_gorbea %>%
  full_join(probability_info_agg, 
            by = c("Bosque","Year","Planta","Periodo","X","Y")) %>%
  filter(!is.na(prob_consp_step),!is.na(poll_richness))

data_model %>% filter(Planta=="Polygala_vulgaris")

plant_sp_with_more_ind <- data_model %>% group_by(Planta) %>% unique() %>% 
  count() %>% ungroup() %>%
  filter(n>6) %>% dplyr::select(Planta) %>% pull()

data_model_final <- data_model %>% filter(Planta %in% c(plant_sp_with_more_ind,"Polygala_vulgaris")) %>%
  mutate(Year=as.factor(Year),
         Periodo=as.factor(Periodo),
         Bosque=as.factor(Bosque))

ggplot(data_model_final %>% filter(Year== 2020),
       aes(x=poll_richness,y=log(prob_consp_step),color=Planta))+
  geom_point(size=3, alpha=.2)+
  geom_smooth(method = "lm")+facet_wrap(~Planta)+
  labs(title="Gorbea (2020)")

ggplot(data_model_final %>% filter(Year== 2021),
       aes(x=poll_richness,y=log(prob_consp_step),color=Planta))+
  geom_point(size=3, alpha=.2)+
  geom_smooth(method = "lm")+facet_wrap(~Planta)+
  labs(title="Gorbea (2021)")

ggplot(data_model_final %>% filter(Year== 2020),
       aes(x=poll_richness,y=log(prob_consp_step)))+
  geom_point(size=3, alpha=.2)+
  geom_smooth(method = "lm")+
  labs(title="Gorbea (2020)")

ggplot(data_model_final %>% filter(Year== 2021),
       aes(x=poll_richness,y=log(prob_consp_step)))+
  geom_point(size=3, alpha=.2)+
  geom_smooth(method = "lm")+
  labs(title="Gorbea (2021)")

model_av_eff_av_richness <- glmmTMB((prob_consp_step) ~ scale(poll_richness)+Planta+Periodo+(1|Bosque)+Year,
                                    family = beta_family(link="logit"),
                                    data = data_model_final)
summary(model_av_eff_av_richness)

unique(data_model_final$Planta)

write_csv(data_model_final,"results/gorbea/comm_eff_VS_poll_rich_data_model.csv")
