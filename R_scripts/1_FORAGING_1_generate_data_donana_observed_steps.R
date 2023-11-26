
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(naniar)

# For each path (codigo_vuelo), we add the point ids (nodes): 1-> 2 -> 3...

flights_raw <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  rename(node = Codigo_within_sequence)


flights_raw$Periodo_hora %>% sort() %>% unique()
flights_raw %>% select(Bosque,Week_ISO) %>% unique() %>% group_by(Bosque) %>% count()
visits_per_plant <- flights_raw %>% group_by(Planta) %>% count()

plant_subset_seeds <- c("Cistus_salviifolius","Cistus_crispus", 
                        "Cistus_ladanifer", "Cistus_libanotis", "Halimium_halimifolium",
                        "Halimium_calycinum","Lavandula_stoechas")

plant_subset_seeds2 <- c("Cistus_salviifolius","Cistus_crispus", 
                        "Cistus_ladanifer", "Cistus_libanotis", "Halimium_halimifolium",
                        "Halimium_calycinum")

100*sum(visits_per_plant$n[visits_per_plant$Planta %in% plant_subset_seeds])/sum(visits_per_plant$n)
100*sum(visits_per_plant$n[visits_per_plant$Planta %in% plant_subset_seeds2])/sum(visits_per_plant$n)
sum(visits_per_plant$n[visits_per_plant$Planta == "Lavandula_stoechas"])


x <- flights_raw %>% group_by(Polinizador) %>% count()
100*8876/sum(x$n)

x <- flights_raw %>% group_by(Planta) %>% count()
sum(x$n)

# Change variables
flights_raw$time_of_day <- "10:00 - 11:59" 
flights_raw$time_of_day[lubridate::hour(flights_raw$Periodo_hora) >= 12] <- "12:00 - 13:59" 
flights_raw$time_of_day[lubridate::hour(flights_raw$Periodo_hora) >= 14] <- "14:00 - 16:05" 

#-------------------------------------------------------------------
# For a given path, we create steps. The following path (1-> 2 -> 3)
# has 2 steps (1-> 2 and 2 -> 3)

# First, we remove paths with a single observation
paths_with_single_observation <- flights_raw %>% group_by(Codigo_vuelo) %>% 
  count() %>% filter(n==1) %>% select(Codigo_vuelo) %>% pull()

flights <- flights_raw %>% filter(!Codigo_vuelo %in% paths_with_single_observation)

# Secondly, we create the steps

steps_raw <- flights %>% rename(X1 = X, Y1 = Y, node1 = node, Planta1 = Planta) %>% 
  mutate(X2 = NA, Y2 = NA, node2 = NA, Planta2 = NA)

for (i in 1:(nrow(steps_raw)-1)) {
  if(steps_raw$Codigo_vuelo[i] == steps_raw$Codigo_vuelo[i+1]){
    steps_raw$X2[i] <-  steps_raw$X1[i+1]
    steps_raw$Y2[i] <-  steps_raw$Y1[i+1]
    steps_raw$node2[i] <-  steps_raw$node1[i+1]
    steps_raw$Planta2[i] <-  steps_raw$Planta1[i+1]
  }
}

steps <- steps_raw %>% filter(!is.na(X2))

# Thirdly, add derived quantities such as step_lengths, angles, turning angles,
# change_plant_sp

steps$step_length <- NA
steps$angle <- NA
steps$turning_angle <- NA
steps$change_plant_sp <- !(steps$Planta2 == steps$Planta1)

for (i in 1:nrow(steps)){
  
  steps$step_length[i] <- sqrt((steps$X2[i]-steps$X1[i])^2+(steps$Y2[i]-steps$Y1[i])^2)
  
  if((steps$step_length[i] == 0) & (!is.na(steps$step_length[i]))){
    steps$angle[i] <- 0
  }else if(!is.na(steps$step_length[i])){
    steps$angle[i] <- atan((steps$Y2[i]-steps$Y1[i])/(steps$X2[i]-steps$X1[i]))*180/pi
  }
  
  if(steps$node1[i] != 1){
    steps$turning_angle[i] <- steps$angle[i]-steps$angle[i-1]
  }
                         
}


#------------------------------------------------------------------------------
# 

steps_data <- steps

steps_data %>% group_by(Polinizador) %>% count() %>% arrange(desc(n))

# Visualization step statistics for main pollinators
Year_i <- 2021


pollinator_i <- "Apis_mellifera"
steps_pollinator_i <- steps_data %>% filter(Polinizador == pollinator_i)


ggplot(steps_pollinator_i, aes(x=step_length, fill = as.factor(time_of_day)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~Bosque)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count",
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Bosque~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=turning_angle, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 18)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Bosque~time_of_day)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")


pollinator_i <- "Bombus_terrestris"
steps_pollinator_i <- steps_data %>% filter(Polinizador == pollinator_i)


ggplot(steps_pollinator_i, aes(x=step_length, fill = as.factor(time_of_day)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~Bosque)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Bosque~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")


pollinator_i <- "Xylocopa_cantabrita"
steps_pollinator_i <- steps_data %>% filter(Polinizador == pollinator_i)


ggplot(steps_pollinator_i, aes(x=step_length, fill = as.factor(time_of_day)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~Bosque)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Bosque~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")


#------------------------------------------------
# Processing flora data

flora_census <- read_csv("results/donana/flora_census_21.csv")

plant_subset_seeds <- c("Cistus_salviifolius","Cistus_crispus", 
                        "Cistus_ladanifer", "Cistus_libanotis", "Halimium_halimifolium",
                        "Halimium_calycinum","Lavandula_stoechas")

flora_census %>% filter(Planta %in% plant_subset_seeds) %>% group_by(Planta,Periodo) %>% count(wt = Flores)

flora_census %>% select(Year, Bosque, Periodo, Day_ISO, X, Y) %>%
  group_by(Year, Bosque, Periodo, Day_ISO, X, Y) %>% count() %>% filter(n>1)

day_ISO_sampling <- flora_census %>% select(Year, Bosque, Periodo, Day_ISO, Fecha) %>%
  group_by(Year, Bosque, Periodo, Day_ISO, Fecha) %>% count() %>% select(-n)

richness_censuses <- flora_census %>% select(X, Y, Planta, Year, Bosque, Periodo) %>%
  unique() %>% group_by(X, Y, Year, Bosque, Periodo) %>% count() %>% rename(richness = n)

total_flowers_censuses <- flora_census %>% select(X, Y, Flores, Year, Bosque, Periodo) %>%
  group_by(X, Y, Year, Bosque, Periodo) %>% count(wt = Flores) %>% rename(total_number_flowers = n) %>%
  filter(total_number_flowers > 0)

flowers_sp_censuses <- flora_census %>% select(X, Y, Planta, Flores, Year, Bosque, Periodo) %>%
  group_by(X, Y, Year, Bosque, Periodo, Planta) %>% count(wt = Flores) %>% rename(flowers_sp = n) %>%
  filter(flowers_sp > 0)

#------------------------------------------------
# Adding flora data:
# 2021 -> Periodo 1: March
# 2021 -> Periodo 3: May

steps_data$Periodo <- 1
steps_data$Periodo[steps_data$Day_ISO > 90] <- 2
steps_data$Periodo[steps_data$Day_ISO > 123] <- 3

steps_flora_data <- steps_data %>% 
  left_join(richness_censuses %>% rename(X1 = X, Y1 = Y, richness1 = richness),
            by = c("X1", "Y1", "Year", "Bosque", "Periodo")) %>%
  left_join(richness_censuses %>% rename(X2 = X, Y2 = Y, richness2 = richness),
            by = c("X2", "Y2", "Year", "Bosque", "Periodo")) %>%
  left_join(total_flowers_censuses %>% rename(X1 = X, Y1 = Y, total_number_flowers1 = total_number_flowers),
            by = c("X1", "Y1", "Year", "Bosque", "Periodo")) %>%
  left_join(total_flowers_censuses %>% rename(X2 = X, Y2 = Y, total_number_flowers2 = total_number_flowers),
            by = c("X2", "Y2", "Year", "Bosque", "Periodo")) %>% 
  left_join(flowers_sp_censuses %>% rename(X1 = X, Y1 = Y, Planta1 = Planta, flowers_sp1_XY1 = flowers_sp),
            by = c("X1", "Y1", "Planta1", "Year", "Bosque", "Periodo")) %>% 
  left_join(flowers_sp_censuses %>% rename(X2 = X, Y2 = Y, Planta1 = Planta, flowers_sp1_XY2 = flowers_sp),
            by = c("X2", "Y2", "Planta1", "Year", "Bosque", "Periodo")) %>% 
  left_join(flowers_sp_censuses %>% rename(X1 = X, Y1 = Y, Planta2 = Planta, flowers_sp2_XY1 = flowers_sp),
            by = c("X1", "Y1", "Planta2", "Year", "Bosque", "Periodo")) %>% 
  left_join(flowers_sp_censuses %>% rename(X2 = X, Y2 = Y, Planta2 = Planta, flowers_sp2_XY2 = flowers_sp),
            by = c("X2", "Y2", "Planta2", "Year", "Bosque", "Periodo")) %>%
  mutate(delta_richness = richness2- richness1,
         delta_total_flowers = total_number_flowers2-total_number_flowers1,
         delta_flowers_sp1 = flowers_sp1_XY2-flowers_sp1_XY1,
         delta_flowers_sp2 = flowers_sp2_XY2-flowers_sp2_XY1)

# Visualize NAs
naniar::vis_miss(steps_flora_data %>% select(Year, Bosque, Periodo, X1, Y1, X2, Y2, richness1, richness2,
                                     total_number_flowers1, total_number_flowers2, flowers_sp1_XY1,
                                     flowers_sp1_XY2,flowers_sp2_XY1,flowers_sp2_XY2))

#------------------------------------------------------------------------------
# Save steps data
write_csv(steps_flora_data,"results/donana/observed_steps_21.csv")
