
library(tidyverse)
library(lubridate)
library(RColorBrewer)

flights21 <- read_csv("results/gorbea/foraging_Gorbea_2021.csv")
flights20 <- read_csv("results/gorbea/foraging_Gorbea_2020.csv")

#################################################################
# Extract info on study sites
# Sampling weeks
unique(flights20$Week_ISO) %>% sort()
unique(flights21$Week_ISO) %>% sort()

# Sampling rounds per site
flights20 %>% select(Bosque,Week_ISO) %>% unique() %>% group_by(Bosque) %>% count()
flights21 %>% select(Bosque,Week_ISO) %>% unique() %>% group_by(Bosque) %>% count()

# codigo_vuelo resets each year. We create a codigo_vuelo list for 2020 and 2021
max(flights20$Codigo_vuelo)
min(flights21$Codigo_vuelo)

flights21$Codigo_vuelo <- flights21$Codigo_vuelo+max(flights20$Codigo_vuelo)

# new code for 2021 stats where it should: sanity check
min(flights21$Codigo_vuelo)

# For each path (codigo_vuelo), we add the point ids (nodes): 1-> 2 -> 3...

flights_raw <- bind_rows(flights20,flights21)
flights_raw$node <- 1

for (i in 2:nrow(flights_raw)) {
  if(flights_raw$Codigo_vuelo[i-1] == flights_raw$Codigo_vuelo[i]){
    flights_raw$node[i] <-  1 + flights_raw$node[i-1]
  }
}

# Change variables
flights_raw$time_of_day <- NA
flights_raw$time_of_day[flights_raw$Periodo_hora == 1] <- "10:00 - 12:39" 
flights_raw$time_of_day[flights_raw$Periodo_hora == 2] <- "12:40 - 15:19" 
flights_raw$time_of_day[flights_raw$Periodo_hora == 3] <- "15:20 - 18:05" 

flights_raw$plot <- NA
flights_raw$plot[flights_raw$Bosque == 1] <- "Plot 1" 
flights_raw$plot[flights_raw$Bosque == 2] <- "Plot 2" 
flights_raw$plot[flights_raw$Bosque == 3] <- "Plot 3" 
flights_raw$plot[flights_raw$Bosque == 4] <- "Plot 4" 
flights_raw$plot[flights_raw$Bosque == 5] <- "Plot 5" 

x <- flights_raw %>% group_by(Polinizador) %>% count()
sum(x$n)

x <- flights_raw %>% group_by(Planta) %>% count()
100*5256/sum(x$n)
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

steps_data <- steps %>% filter(Planta1 != "Ulex_europaeus",
                               Planta2 != "Ulex_europaeus")

steps_data %>% group_by(Polinizador) %>% count() %>% arrange(desc(n))

# Visualization step statistics for main pollinators

pollinator_i <- "Bombus_pascuorum"
steps_pollinator_i <- steps_data %>% filter(Polinizador == pollinator_i)


ggplot(steps_pollinator_i, aes(x=step_length, fill = as.factor(time_of_day)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~plot)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count",
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

Year_i = 2020
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")
Year_i = 2021
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

ggplot(steps_pollinator_i, aes(x=turning_angle, fill = as.factor(time_of_day)))+
  geom_histogram()+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~plot)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count",
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

Year_i = 2020
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=turning_angle, fill = as.factor(change_plant_sp)))+
  geom_histogram()+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

Year_i = 2021
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=turning_angle, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 18)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")


pollinator_i <- "Apis_mellifera"
steps_pollinator_i <- steps_data %>% filter(Polinizador == pollinator_i)


ggplot(steps_pollinator_i, aes(x=step_length, fill = as.factor(time_of_day)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~plot)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

Year_i = 2020
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

Year_i = 2021
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

ggplot(steps_pollinator_i, aes(x=turning_angle, fill = as.factor(time_of_day)))+
  geom_histogram()+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~plot)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

Year_i = 2020
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=turning_angle, fill = as.factor(change_plant_sp)))+
  geom_histogram()+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")
Year_i = 2021
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=turning_angle, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 18)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

pollinator_i <- "Sphaerophoria_scripta"
steps_pollinator_i <- steps_data %>% filter(Polinizador == pollinator_i)


ggplot(steps_pollinator_i, aes(x=step_length, fill = as.factor(time_of_day)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~plot)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

Year_i = 2020
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

Year_i = 2021
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=step_length, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 1)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Step length [m]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

ggplot(steps_pollinator_i, aes(x=turning_angle, fill = as.factor(time_of_day)))+
  geom_histogram()+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Year~Bosque)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = gsub("_", " ", pollinator_i), fill = "Time of day")

Year_i = 2020
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=turning_angle, fill = as.factor(change_plant_sp)))+
  geom_histogram()+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(Bosque~time_of_day)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")
Year_i = 2021
ggplot(steps_pollinator_i %>% filter(Year==Year_i), aes(x=turning_angle, fill = as.factor(change_plant_sp)))+
  geom_histogram(binwidth = 18)+
  scale_fill_brewer(palette ="Paired")+
  facet_grid(plot~time_of_day)+
  theme_bw()+
  labs(x="Turning angle [degrees]", y = "Count", 
       title = paste0(gsub("_", " ", pollinator_i)," (",Year_i,")"), fill = "Change plant sp.")

#------------------------------------------------
# Processing flora data

flora_census <- read_csv("results/gorbea/flora_census_20_21.csv")

flora_census %>% select(Year, Bosque, Periodo, Day_ISO, X, Y) %>%
  group_by(Year, Bosque, Periodo, Day_ISO, X, Y) %>% count() %>% filter(n>1)

flora_census %>% filter(Year==2020, Bosque == 1, Periodo == 1, Day_ISO== 127,
                        X==0, Y==20)

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
# Adding flora data

# 2020 -> Periodo 1: day <= 149
# 2021 -> Periodo 1: day <= 127
# 2021 -> Periodo 3: day > 152

steps_data$Periodo <- 1
steps_data$Periodo[steps_data$Year==2020 & steps_data$Day_ISO > 149] <- 2
steps_data$Periodo[steps_data$Year==2021 & steps_data$Day_ISO > 127] <- 2
steps_data$Periodo[steps_data$Year==2021 & steps_data$Day_ISO > 152] <- 3

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

library(naniar)
vis_miss(steps_flora_data %>% select(Year, Bosque, Periodo, X1, Y1, X2, Y2, richness1, richness2,
                                     total_number_flowers1, total_number_flowers2, flowers_sp1_XY1,
                                     flowers_sp1_XY2,flowers_sp2_XY1,flowers_sp2_XY2))

steps_flora_data %>% filter(is.na(richness1)) %>% select(X1,Y1,Bosque,Year,Periodo)
richness_censuses %>% filter(X==8,Y==6,Bosque==4,Year==2020,Periodo==2)
#------------------------------------------------------------------------------
# Save steps data
write_csv(steps_flora_data,"results/gorbea/observed_steps_20_21.csv")
