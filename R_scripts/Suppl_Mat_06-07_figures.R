
library(tidyverse)
library(lme4)
library(lmerTest)
library(scales)
library(wesanderson)

flora_census_georbeia <- read_csv("results/gorbea/flora_census_20_21.csv")
unique(flora_census_georbeia$Planta)
sum(flora_census_georbeia$Flores[!is.na(flora_census_georbeia$Flores)])


# We need to fix the numbering of the flowering period in Gorbea
flora_census_georbeia$Periodo[flora_census_georbeia$Year==2020] <- 
  flora_census_georbeia$Periodo[flora_census_georbeia$Year==2020] + 1


fl_richness_plot_gorbeia <- flora_census_georbeia %>% dplyr::select(Year,Bosque, Planta) %>% unique() %>% group_by(Year,Bosque) %>% count()
mean(fl_richness_plot_gorbeia$n)
sd(fl_richness_plot_gorbeia$n)

fl_abundance_plot_gorbeia <- flora_census_georbeia %>% dplyr::select(Year,Bosque, Flores) %>% group_by(Year,Bosque) %>% count(wt=Flores)
mean(fl_abundance_plot_gorbeia$n)
sd(fl_abundance_plot_gorbeia$n)


# Heatmaps
fl_richness_plot_gorbeia_XY <- flora_census_georbeia %>% 
  dplyr::select(Year,Bosque, Planta,X,Y) %>% unique() %>% 
  group_by(Year,Bosque,X,Y) %>% count()

fl_abundance_plot_gorbeia_XY <- flora_census_georbeia %>% 
  dplyr::select(Year,Bosque, Flores,X,Y) %>% group_by(Year,Bosque,X,Y) %>% 
  count(wt=Flores)

fl_richness_plot_gorbeia_XY_periodo <- flora_census_georbeia %>% 
  dplyr::select(Year,Bosque, Planta,X,Y,Periodo) %>% unique() %>% 
  group_by(Year,Bosque,X,Y,Periodo) %>% count() %>%
  mutate(Bosque = paste0("Plot ",Bosque),
         Periodo = paste0("Period ",Periodo))

fl_abundance_plot_gorbeia_XY_periodo <- flora_census_georbeia %>% 
  dplyr::select(Year,Bosque, Flores,X,Y,Periodo) %>% 
  group_by(Year,Bosque,X,Y,Periodo) %>% count(wt=Flores) %>%
  mutate(Bosque = paste0("Plot ",Bosque),
         Periodo = paste0("Period ",Periodo))




fl_richness_plot_gorbeia_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))   %>% filter(Year==2020)

fl_abundance_plot_gorbeia_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))   %>% filter(Year==2020)


fl_richness_plot_gorbeia_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))   %>% filter(Year==2021)

fl_abundance_plot_gorbeia_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))   %>% filter(Year==2021)



plot_names <- c(
  `1` = "Plot 1",
  `2` = "Plot 2",
  `3` = "Plot 3",
  `4` = "Plot 4",
  `5` = "Plot 5"
)

png("figures/plant_richness_gorbea_2020.png", width=1200*5, height = 300*5, res=300*2)
ggplot(fl_richness_plot_gorbeia_XY %>% filter(Year==2020), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_wrap(~Bosque,labeller = as_labeller(plot_names), ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +
  labs(title = "Accumulated plant richness (Gorbeia N.P., 2020)",x="X(m)",y="Y(m)",
       fill = "Number of\nplant species")#+theme(legend.position = "bottom")
dev.off()

png("figures/plant_richness_gorbea_2020_periodo.png", width=1200*5, height = 450*5, res=300*2)
ggplot(fl_richness_plot_gorbeia_XY_periodo %>% filter(Year==2020), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Plant richness by flowering period (Gorbeia N.P., 2020)",x="X(m)",y="Y(m)",
       fill = "Number of\nplant species")#+theme(legend.position = "bottom")
dev.off()

png("figures/plant_richness_gorbea_2021.png", width=1200*5, height = 300*5, res=300*2)
ggplot(fl_richness_plot_gorbeia_XY %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_wrap(~Bosque,labeller = as_labeller(plot_names), ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Accumulated plant richness (Gorbeia N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\nplant species")#+theme(legend.position = "bottom")
dev.off()

png("figures/plant_richness_gorbea_2021_periodo.png", width=1200*5, height = 700*5, res=300*2)
ggplot(fl_richness_plot_gorbeia_XY_periodo %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Plant richness by flowering period (Gorbeia N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\nplant species")#+theme(legend.position = "bottom")
dev.off()


png("figures/fl_abundance_gorbea_2020.png", width=1200*5, height = 300*5, res=300*2)
ggplot(fl_abundance_plot_gorbeia_XY %>% filter(Year==2020), aes(X, Y, fill= (n))) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_wrap(~Bosque,labeller = as_labeller(plot_names), ncol = 5)+
  scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous")),trans = "log10") +theme_bw()+
  labs(title = "Accumulated flower abundance (Gorbeia N.P., 2020)",x="X(m)",y="Y(m)",
       fill = "Number of flowers")#+theme(legend.position = "bottom")
dev.off()


png("figures/fl_abundance_gorbea_2020_periodo.png", width=1200*5, height = 450*5, res=300*2)
ggplot(fl_abundance_plot_gorbeia_XY_periodo %>% filter(Year==2020), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous")),trans = "log10") +theme_bw()+
  labs(title = "Flower abundance by flowering period (Gorbeia N.P., 2020)",x="X(m)",y="Y(m)",
       fill = "Number of flowers")#+theme(legend.position = "bottom")
dev.off()




png("figures/fl_abundance_gorbea_2021.png",width=1200*5, height = 300*5, res=300*2)
ggplot(fl_abundance_plot_gorbeia_XY %>% filter(Year==2021), aes(X, Y, fill= (n))) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_wrap(~Bosque,labeller = as_labeller(plot_names), ncol = 5)+
  scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous")),trans = "log10") +theme_bw()+
  labs(title = "Accumulated flower abundance (Gorbeia N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of flowers")#+theme(legend.position = "bottom")
dev.off()

png("figures/fl_abundance_gorbea_2021_periodo.png", width=1200*5, height = 700*5, res=300*2)
ggplot(fl_abundance_plot_gorbeia_XY_periodo %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous")),trans = "log10") +theme_bw()+
  labs(title = "Flower abundance by flowering period (Gorbeia N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of flowers")#+theme(legend.position = "bottom")
dev.off()


fl_richness_plot_gorbeia_XY_periodo_2021 <- fl_richness_plot_gorbeia_XY_periodo %>% filter(Year==2021)
###############################
# Models
model_gorbeia_plant_richness <- lmer(n ~ Periodo+(1|Bosque/Year), data = fl_richness_plot_gorbeia_XY_periodo %>%
                                       mutate(Periodo =as.factor(Periodo),
                                              Bosque = as.factor(Bosque),Year = as.factor(Year)))
summary(model_gorbeia_plant_richness)

model_gorbeia_plant_abundance <- lmer(n ~ Periodo+(1|Bosque/Year), data = fl_abundance_plot_gorbeia_XY_periodo %>%
                                        mutate(Periodo =as.factor(Periodo),
                                               Bosque = as.factor(Bosque),Year = as.factor(Year)))

summary(model_gorbeia_plant_abundance)

#######################



#############################
# DONANA ####################
#############################


flora_census_donana <- read_csv("results/donana/flora_census_21.csv")
unique(flora_census_donana$Planta)
sum(flora_census_donana$Flores[!is.na(flora_census_donana$Flores)])

fl_richness_plot_donana <- flora_census_donana %>% dplyr::select(Year,Bosque, Planta) %>% unique() %>% group_by(Year,Bosque) %>% count()
mean(fl_richness_plot_donana$n)
sd(fl_richness_plot_donana$n)

fl_abundance_plot_donana <- flora_census_donana %>% dplyr::select(Year,Bosque, Flores) %>% group_by(Year,Bosque) %>% count(wt=Flores)
mean(fl_abundance_plot_donana$n)
sd(fl_abundance_plot_donana$n)


# Heatmaps

# Heatmaps
fl_richness_plot_donana_XY <- flora_census_donana %>% 
  dplyr::select(Year,Bosque, Planta,X,Y) %>% unique() %>% 
  group_by(Year,Bosque,X,Y) %>% count()

fl_abundance_plot_donana_XY <- flora_census_donana %>% 
  dplyr::select(Year,Bosque, Flores,X,Y) %>% group_by(Year,Bosque,X,Y) %>% 
  count(wt=Flores)

fl_richness_plot_donana_XY_periodo <- flora_census_donana %>% 
  dplyr::select(Year,Bosque, Planta,X,Y,Periodo) %>% unique() %>% 
  group_by(Year,Bosque,X,Y,Periodo) %>% count()%>%
  mutate(Periodo = paste0("Period ",Periodo))

fl_abundance_plot_donana_XY_periodo <- flora_census_donana %>% 
  dplyr::select(Year,Bosque, Flores,X,Y,Periodo) %>% 
  group_by(Year,Bosque,X,Y,Periodo) %>% count(wt=Flores) %>%
  mutate(Periodo = paste0("Period ",Periodo))







fl_richness_plot_donana_XY$Bosque[fl_richness_plot_donana_XY$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"
fl_abundance_plot_donana_XY$Bosque[fl_abundance_plot_donana_XY$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"
fl_richness_plot_donana_XY_periodo$Bosque[fl_richness_plot_donana_XY_periodo$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"
fl_abundance_plot_donana_XY_periodo$Bosque[fl_abundance_plot_donana_XY_periodo$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"


fl_richness_plot_donana_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))   %>% filter(Year==2021)

fl_abundance_plot_donana_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))   %>% filter(Year==2021)



png("figures/plant_richness_donana_2021.png", width=1200*5, height = 300*5, res=300*2)
ggplot(fl_richness_plot_donana_XY %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_wrap(~Bosque,ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Accumulated plant richness (Doñana N.P., 2021)",
       fill = "Number of\nplant species")#+theme(legend.position = "bottom")
dev.off()

png("figures/plant_richness_donana_2021_periodo.png", width=1200*5, height = 700*5, res=300*2)
ggplot(fl_richness_plot_donana_XY_periodo %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Plant richness by flowering period (Doñana N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\nplant species")#+theme(legend.position = "bottom")
dev.off()





png("figures/fl_abundance_donana_2021.png",width=1200*5, height = 300*5, res=300*2)
ggplot(fl_abundance_plot_donana_XY %>% filter(Year==2021), aes(X, Y, fill= (n))) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_wrap(~Bosque,ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous")),trans = "log10") +theme_bw()+
  labs(title = "Accumulated flower abundance (Doñana N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of flowers")#+theme(legend.position = "bottom")
dev.off()

png("figures/fl_abundance_donana_2021_periodo.png", width=1200*5, height = 700*5, res=300*2)
ggplot(fl_abundance_plot_donana_XY_periodo %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous")),trans = "log10") +theme_bw()+
  labs(title = "Flower abundance by flowering period (Doñana N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of flowers")#+theme(legend.position = "bottom")
dev.off()



# Models
model_donana_plant_richness <- lmer(n ~ Periodo+(1|Bosque/Year), data = fl_richness_plot_donana_XY_periodo %>%
                                      mutate(Periodo =as.factor(Periodo),
                                             Bosque = as.factor(Bosque),Year = as.factor(Year)))
summary(model_donana_plant_richness)
performance::r2(model_donana_plant_richness)

model_donana_plant_abundance <- lmer(n ~ Periodo+(1|Bosque/Year), data = fl_abundance_plot_donana_XY_periodo %>%
                                       mutate(Periodo =as.factor(Periodo),
                                              Bosque = as.factor(Bosque),Year = as.factor(Year)))

summary(model_donana_plant_abundance)


#############################################################################
# Plant pollinator interactions
#############################################################################


flights_gorbeia_2020 <- read_csv("results/gorbea/foraging_Gorbea_2020.csv")
flights_gorbeia_2021 <- read_csv("results/gorbea/foraging_Gorbea_2021.csv")

number_contacts_gorbeia_2020 <- nrow(flights_gorbeia_2020)
number_contacts_gorbeia_2021 <- nrow(flights_gorbeia_2021)

number_contacts_gorbeia_2020 + number_contacts_gorbeia_2021

number_seq_gorbeia_2020 <- length(unique(flights_gorbeia_2020$Codigo_vuelo))
number_seq_gorbeia_2021 <- length(unique(flights_gorbeia_2021$Codigo_vuelo))
number_seq_gorbeia_2020 + number_seq_gorbeia_2021

number_poll_gorbeia_2020 <- length(unique(flights_gorbeia_2020$Polinizador))
number_poll_gorbeia_2021 <- length(unique(flights_gorbeia_2021$Polinizador))
number_poll_gorbeia_2020 + number_poll_gorbeia_2021


total_flights_gorbeia <- bind_rows(flights_gorbeia_2020,flights_gorbeia_2021)

contacts_poll <- total_flights_gorbeia %>% select(Polinizador) %>% group_by(Polinizador) %>% count() %>% arrange(desc(n))

most_abundand_poll_gorbeia <- contacts_poll$Polinizador[1:5]
sum(contacts_poll$n[1:5]) # 5 most recorded species
sum(contacts_poll$n[1:5])/sum(contacts_poll$n) # percentage of contacts of 5 most visited species

total_flights_gorbeia %>% filter(Polinizador %in% most_abundand_poll_gorbeia) %>% select(Codigo_vuelo) %>% unique() %>% nrow() # number of seq most


poll_visits_gorbeia <- total_flights_gorbeia %>% dplyr::select(Codigo_vuelo) %>% group_by(Codigo_vuelo) %>% count()
mean(poll_visits_gorbeia$n) # flower visited per seq
sd(poll_visits_gorbeia$n)

plants_visited_gorbeia_by_poll_in_seq <- total_flights_gorbeia %>% 
  dplyr::select(Codigo_vuelo, Planta) %>% unique() %>% group_by(Codigo_vuelo) %>% count()
mean(plants_visited_gorbeia_by_poll_in_seq$n) # flower visited per seq
sd(plants_visited_gorbeia_by_poll_in_seq$n)



flights_donana <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  rename(node = Codigo_within_sequence)

nrow(flights_donana)
length(unique(flights_donana$Codigo_vuelo))
length(unique(flights_donana$Polinizador))

contacts_poll_donana <- flights_donana %>% select(Polinizador) %>% group_by(Polinizador) %>% count() %>% arrange(desc(n))

most_abundand_poll_donana <- contacts_poll_donana$Polinizador[1:5]
sum(contacts_poll_donana$n[1:5]) # 5 most recorded species
sum(contacts_poll_donana$n[1:5])/sum(contacts_poll_donana$n)

flights_donana %>% filter(Polinizador %in% most_abundand_poll_donana) %>% select(Codigo_vuelo) %>% unique() %>% nrow() # number of seq most


poll_visits_donana <- flights_donana %>% dplyr::select(Codigo_vuelo) %>% group_by(Codigo_vuelo) %>% count()
mean(poll_visits_donana$n) # flower visited per seq
sd(poll_visits_donana$n)


plants_visited_donana_by_poll_in_seq <-flights_donana %>% 
  dplyr::select(Codigo_vuelo, Planta) %>% unique() %>% group_by(Codigo_vuelo) %>% count()
mean(plants_visited_donana_by_poll_in_seq$n) # flower visited per seq
sd(plants_visited_donana_by_poll_in_seq$n)
