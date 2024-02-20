
library(tidyverse)

flora_census <- read_csv("results/donana/flora_census_21.csv")

unique(flora_census$Bosque)

flora_census$Bosque[flora_census$Bosque=="Pinar Aznalcazar"] <- "Aznalcazar"
flora_census$Bosque[flora_census$Bosque=="Pinar Hinojos"] <- "Hinojos"
flora_census$Bosque[flora_census$Bosque=="Pinar Puebla"] <- "Puebla"
flora_census$Bosque[flora_census$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Vill. Este"
flora_census$Bosque[flora_census$Bosque=="Pinar Villamanrique Sur"] <- "Vill. Sur"
flora_census$Planta <- sub("_", " ", flora_census$Planta)

flowers_subplot_plant <- flora_census %>% 
  group_by(Year,Bosque,Periodo,Planta,X,Y) %>% count(wt=Flores) %>%
  rename(number_flowers = n) %>% ungroup()

#sanity check
any(duplicated(flowers_subplot_plant))


flights_raw <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  rename(node = Codigo_within_sequence)

flights_raw$Bosque[flights_raw$Bosque=="Pinar Aznalcazar"] <- "Aznalcazar"
flights_raw$Bosque[flights_raw$Bosque=="Pinar Hinojos"] <- "Hinojos"
flights_raw$Bosque[flights_raw$Bosque=="Pinar Puebla"] <- "Puebla"
flights_raw$Bosque[flights_raw$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Vill. Este"
flights_raw$Bosque[flights_raw$Bosque=="Pinar Villamanrique Sur"] <- "Vill. Sur"
flights_raw$Planta <- sub("_", " ", flights_raw$Planta)

flights_raw$Periodo <- 1
flights_raw$Periodo[flights_raw$Day_ISO > 90] <- 2
flights_raw$Periodo[flights_raw$Day_ISO > 123] <- 3

visits_subplot_pollinator_plant <- flights_raw %>% 
  group_by(Year,Bosque,Periodo,Polinizador,Planta,X,Y) %>% count() %>%
  rename(number_visits = n) %>% ungroup()

visits_plant_subplot <- flights_raw %>% 
  group_by(Year,Bosque,Periodo,Planta,X,Y) %>% count() %>%
  rename(number_visits = n) %>% ungroup()

#sanity check
any(duplicated(visits_plant_subplot))

total_visits_pollinator_subplot <- visits_subplot_pollinator_plant %>%
  group_by(Year,Bosque,Periodo,Polinizador,X,Y) %>% count(wt=number_visits) %>%
  rename(number_visits_XY = n) %>% ungroup()

total_census_plant_nodes <- flowers_subplot_plant %>% 
  full_join(visits_plant_subplot,by=c("Year","Bosque","Periodo","Planta","X","Y"))

subplots_without_flower_but_visits <- sum(is.na(total_census_plant_nodes$number_flowers))
visits2subplots_without_flower <- sum(total_census_plant_nodes$number_visits[is.na(total_census_plant_nodes$number_flowers)])

100*visits2subplots_without_flower/sum(total_census_plant_nodes$number_visits[!is.na(total_census_plant_nodes$number_visits)])

total_census_plant_nodes_updated <- total_census_plant_nodes 
total_census_plant_nodes_updated$number_flowers[is.na(total_census_plant_nodes_updated$number_flowers)] <- 
  total_census_plant_nodes_updated$number_visits[is.na(total_census_plant_nodes_updated$number_flowers)]

total_census_plant_nodes_updated$number_visits[is.na(total_census_plant_nodes_updated$number_visits)] <- 0

sum(is.na(total_census_plant_nodes_updated$number_flowers)) #OK
sum(is.na(total_census_plant_nodes_updated$X)) #OK
sum(is.na(total_census_plant_nodes_updated$Y)) #OK


total_census_plant_nodes_updated <- total_census_plant_nodes_updated %>%
  filter(!is.na(X),!is.na(Y)) %>%
  group_by(Year, Bosque, Periodo, Planta, X, Y) %>% 
  summarize(
    number_flowers = sum(number_flowers, na.rm = TRUE),
    number_visits = sum(number_visits, na.rm = TRUE),
    .groups = 'drop'
  )

total_census_plant_nodes_updated

candidate_plant_nodes <- total_census_plant_nodes_updated %>%
  dplyr::select(Year,Bosque,Periodo,Planta,X,Y) %>% unique()

###################

pollinator_species <- c("Bombus_terrestris","Apis_mellifera",
                        "Xylocopa_cantabrita","Dasypoda_cingulata",
                        "Anthophora_dispar")

total_visits_MAIN_pollinators_subplot <- total_visits_pollinator_subplot %>%
  filter(Polinizador %in% pollinator_species)

any(total_visits_MAIN_pollinators_subplot %>% dplyr::select(-number_visits_XY) %>% 
  duplicated())
sum(is.na(total_visits_MAIN_pollinators_subplot$X)) #OK
sum(is.na(total_visits_MAIN_pollinators_subplot$Y)) #OK

#############

rd_visits_subplot_pollinator_plant <- NULL

repetitions <- 100

set.seed(125)

for(rep in 1:repetitions){
  
  for(i in 1:nrow(total_visits_MAIN_pollinators_subplot)){
    
    candidate_plant_nodes_i <- candidate_plant_nodes %>%
      filter(Year==total_visits_MAIN_pollinators_subplot$Year[i],
             Bosque==total_visits_MAIN_pollinators_subplot$Bosque[i],
             Periodo==total_visits_MAIN_pollinators_subplot$Periodo[i],
             X==total_visits_MAIN_pollinators_subplot$X[i],
             Y==total_visits_MAIN_pollinators_subplot$Y[i]) %>%
      dplyr::select(Planta) %>% pull()
    
    random_visits_XY_i <- sample(candidate_plant_nodes_i,
                                 size = total_visits_MAIN_pollinators_subplot$number_visits_XY[i], 
                                 replace = TRUE)
    
    visits_XY_per_plant_sp_i <- table(random_visits_XY_i) %>% as.data.frame() %>%
      rename(number_visits=Freq,Planta=random_visits_XY_i)
    
    rd_results <- tibble(Year = total_visits_MAIN_pollinators_subplot$Year[i],
                         Bosque = total_visits_MAIN_pollinators_subplot$Bosque[i],
                         Periodo = total_visits_MAIN_pollinators_subplot$Periodo[i],
                         Polinizador =  total_visits_MAIN_pollinators_subplot$Polinizador[i],
                         Planta = as.character(visits_XY_per_plant_sp_i$Planta),
                         X = total_visits_MAIN_pollinators_subplot$X[i],
                         Y = total_visits_MAIN_pollinators_subplot$Y[i],
                         number_visits = visits_XY_per_plant_sp_i$number_visits,
                         repetition = rep,
                         type = "random")
    
    rd_visits_subplot_pollinator_plant <- 
      bind_rows(rd_visits_subplot_pollinator_plant,rd_results)

  }
  
}

# write_csv(rd_visits_subplot_pollinator_plant,
#           "results/donana/rd_visits_subplot_pollinator_plant.csv") # Commented for security reasons

rd_visits_subplot_pollinator_plant <- read_csv("results/donana/rd_visits_subplot_pollinator_plant.csv")

sum(is.na( rd_visits_subplot_pollinator_plant$X))
sum(is.na( rd_visits_subplot_pollinator_plant$Y))


rd_visits_subplot_pollinator_plant

percentil_rd_visits_subplot_pollinator_plant <- NULL


visits_subplot_MAIN_pollinator_plant <- visits_subplot_pollinator_plant %>% 
  filter(Polinizador %in% pollinator_species) %>%
  group_by(Year,Bosque,Periodo,Planta) %>% #group_by(Year,Bosque,Periodo,Planta) %>%
  count(wt=number_visits) %>%
  rename(number_visits=n) %>% ungroup()
  

for (i in 1:nrow(visits_subplot_MAIN_pollinator_plant)) {
  
  rd_visits <- rd_visits_subplot_pollinator_plant %>%
    group_by(Year,Bosque,Periodo,Planta,repetition) %>%
    count(wt=number_visits) %>%
    rename(number_visits=n)  %>% ungroup() %>%
    filter(Year==visits_subplot_MAIN_pollinator_plant$Year[i],
           Bosque==visits_subplot_MAIN_pollinator_plant$Bosque[i],
           Periodo==visits_subplot_MAIN_pollinator_plant$Periodo[i],
           # Polinizador==visits_subplot_MAIN_pollinator_plant$Polinizador[i],
           Planta==visits_subplot_MAIN_pollinator_plant$Planta[i]) %>%#,
           #X==visits_subplot_MAIN_pollinator_plant$X[i],
           #Y==visits_subplot_MAIN_pollinator_plant$Y[i]) %>%
    dplyr::select(number_visits) %>% pull()
  
  percentil_i <- sum(rd_visits <= visits_subplot_MAIN_pollinator_plant$number_visits[i]) / length(rd_visits) * 100
  check_i <- all(rd_visits == visits_subplot_MAIN_pollinator_plant$number_visits[i])
  
  percentil_rd_results <- tibble(Year = visits_subplot_MAIN_pollinator_plant$Year[i],
                                 Bosque = visits_subplot_MAIN_pollinator_plant$Bosque[i],
                                 Periodo = visits_subplot_MAIN_pollinator_plant$Periodo[i],
                                 #Polinizador =  visits_subplot_MAIN_pollinator_plant$Polinizador[i],
                                 Planta = visits_subplot_MAIN_pollinator_plant$Planta[i],
                                 #X = visits_subplot_MAIN_pollinator_plant$X[i],
                                 #Y = visits_subplot_MAIN_pollinator_plant$Y[i],
                                 number_visits = visits_subplot_MAIN_pollinator_plant$number_visits[i],
                                 percentil = percentil_i,
                                check_percertil = check_i
                                )
    
    percentil_rd_visits_subplot_pollinator_plant <- 
      bind_rows(percentil_rd_visits_subplot_pollinator_plant,percentil_rd_results)
  
}


data_model <- percentil_rd_visits_subplot_pollinator_plant %>%
  left_join(total_census_plant_nodes_updated %>%
              dplyr::select(-number_visits)%>%
              group_by(Year,Bosque,Periodo,Planta) %>%
              count(wt=number_flowers) %>%
              rename(number_flowers=n) %>% ungroup(),
            by=c("Year","Bosque","Periodo","Planta")) %>%
  mutate(percentil = percentil/100)

library(scales)
ggplot(data_model)+
  geom_point(aes(x=percentil,y=number_flowers, color=Planta),size=3, alpha=0.3)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(Periodo~Bosque, scale= "free")+
  theme_bw()

data_model$percentil[data_model$percentil==1] <- 1-1e-3
data_model$percentil[data_model$percentil==0] <- 1e-3

#############################################
# EXTRACT 95% CI for Pollinators percentiles
#############################################

observations_planta <- data_model %>% group_by(Planta) %>% count()

not_enoug_observations <- observations_planta$Planta[observations_planta$n<3]

data_model %>% filter(Planta %in% not_enoug_observations)

planta_fits <- data_model$Planta[!data_model$Planta %in% not_enoug_observations] %>% unique()
planta_fits_CI <- tibble(position = planta_fits)
planta_fits_CI$mean <- NA
planta_fits_CI$lower_CI <- NA
planta_fits_CI$upper_CI <- NA

# library(lme4)
# library(lmerTest)
#library(nlme)
library(glmmTMB)

for(pol_i in 1:length(planta_fits)){
  
  data_i = data_model %>% #filter(Year==2021) %>%
    filter(Planta == planta_fits[pol_i]) %>%
    mutate(Periodo=as.factor(Periodo),Year=as.factor(Year),
           Bosque=as.factor(Bosque))
  
  plant_m <- glmmTMB(percentil ~ 1+(1|Periodo),
                          family=beta_family(link="logit"),
                       data = data_i)
 
  summary(plant_m)
  CI_m <- confint(plant_m)
  intercept_row <- which(row.names(CI_m)=="cond.(Intercept)")
  planta_fits_CI$mean[pol_i] <-  exp(CI_m[intercept_row,3]) / (1 + exp(CI_m[intercept_row,3])) 
  planta_fits_CI$lower_CI[pol_i] <- exp(CI_m[intercept_row,1]) / (1 + exp(CI_m[intercept_row,1]))
  planta_fits_CI$upper_CI[pol_i] <- exp(CI_m[intercept_row,2]) / (1 + exp(CI_m[intercept_row,2]))
}

planta_fits_CI

abundance_fits_CI <- tibble(position = planta_fits)
abundance_fits_CI$mean <- NA
abundance_fits_CI$lower_CI <- NA
abundance_fits_CI$upper_CI <- NA

# library(lme4)
# library(lmerTest)
#library(nlme)
library(glmmTMB)

for(pol_i in 1:length(planta_fits)){
  
  data_i = data_model %>% #filter(Year==2021) %>%
    filter(Planta == planta_fits[pol_i]) %>%
    mutate(Periodo=as.factor(Periodo),Year=as.factor(Year),
           Bosque=as.factor(Bosque))
  
  plant_m <- glmmTMB(number_flowers ~ 1+(1|Periodo),
                          family = nbinom2(),
                          data = data_i)
  
  summary(plant_m)
  CI_m <- confint(plant_m)
  intercept_row <- which(row.names(CI_m)=="cond.(Intercept)")
  abundance_fits_CI$mean[pol_i] <-  exp(CI_m[intercept_row,3])
  abundance_fits_CI$lower_CI[pol_i] <- exp(CI_m[intercept_row,1])
  abundance_fits_CI$upper_CI[pol_i] <- exp(CI_m[intercept_row,2])
}


library(RColorBrewer)

colores_brewer <- brewer.pal(12, "Set2")



df_plant_visits_fl_ab <- inner_join(planta_fits_CI, abundance_fits_CI, by = "position", suffix = c("_percentil", "_flores")) %>%
  mutate(mean_percentil = 100 * mean_percentil,
         lower_CI_percentil = 100 * lower_CI_percentil,
         upper_CI_percentil = 100 * upper_CI_percentil)


png("figures/Donana_visits_abundance.png",
    width = 800*3, # The width of the plot in inches
    height = 520*4.5,
    res=300)
# Crear el gráfico
ggplot(df_plant_visits_fl_ab, aes(x = mean_flores, y = mean_percentil, color = position)) +
  geom_errorbar(aes(ymin = lower_CI_percentil, ymax = upper_CI_percentil), width = .1,size = 1.2) + 
  geom_errorbarh(aes(xmin = lower_CI_flores, xmax = upper_CI_flores), height = .1,size = 1.2) + 
  geom_point(size = 3) +
  scale_color_manual(values = colores_brewer)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  ylim(0,100)+
  labs(x = "Average number of flowers (flowers)", y = "Average percentil of observed visits (%)", color=NULL,
       title = "Doñana (2021)") +
  theme_bw()+
  theme(legend.position="bottom",
        legend.text = element_text(size = 15,face = "italic"), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))

dev.off()
