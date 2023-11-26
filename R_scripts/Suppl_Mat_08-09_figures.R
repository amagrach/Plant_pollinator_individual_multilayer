
library(tidyverse)
library(scales)
library(lme4)
library(lmerTest)
library(wesanderson)
###########################################################################
# Create pollinator_richness_censuses

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


pollinator_richness_censuses_gorbea <- flights_raw_gorbea %>% 
  dplyr::select(Year, X, Y, Polinizador, Bosque, Periodo,Week_ISO) %>%
  unique() %>% group_by(Year, X, Y, Bosque, Periodo, Week_ISO) %>% count() %>% rename(poll_richness = n)


pollinator_abundance_censuses_gorbea <- flights_raw_gorbea %>% 
  dplyr::select(Year, X, Y, Codigo_vuelo, Bosque, Periodo,Week_ISO) %>%
  unique() %>% group_by(Year, X, Y, Bosque, Periodo, Week_ISO) %>% count() %>% rename(poll_abundance = n)

poll_richness_plot_gorbea <- pollinator_richness_censuses_gorbea %>% ungroup() %>%
  dplyr::select(Year, Bosque,poll_richness) %>% unique() %>% group_by(Year,Bosque) %>% count(wt=poll_richness)
mean(poll_richness_plot_gorbea$n)
sd(poll_richness_plot_gorbea$n)




# Heatmaps
pol_richness_plot_gorbeia_XY <- pollinator_richness_censuses_gorbea %>% ungroup() %>%
  dplyr::select(Year,Bosque,X,Y,poll_richness) %>% 
  group_by(Year,Bosque,X,Y) %>% count(wt=poll_richness)%>%
  mutate(Bosque = paste0("Plot ",Bosque))

pol_abundance_plot_gorbeia_XY <- pollinator_abundance_censuses_gorbea %>% ungroup() %>%
  dplyr::select(Year,Bosque,X,Y,poll_abundance) %>% group_by(Year,Bosque,X,Y) %>% 
  count(wt=poll_abundance)%>%
  mutate(Bosque = paste0("Plot ",Bosque))


pol_richness_plot_gorbeia_XY_periodo <- pollinator_richness_censuses_gorbea %>% ungroup() %>%
  dplyr::select(Year,Bosque,X,Y,poll_richness,Periodo) %>% 
  group_by(Year,Bosque,X,Y,Periodo) %>% count(wt=poll_richness) %>%
  mutate(Bosque = paste0("Plot ",Bosque),
         Periodo = paste0("Period ",Periodo))

pol_abundance_plot_gorbeia_XY_periodo <- pollinator_abundance_censuses_gorbea %>% 
  ungroup() %>% dplyr::select(Year,Bosque,X,Y,poll_abundance,Periodo) %>% 
  group_by(Year,Bosque,X,Y,Periodo) %>% 
  count(wt=poll_abundance) %>%
  mutate(Bosque = paste0("Plot ",Bosque),
         Periodo = paste0("Period ",Periodo))


# Models
library(lme4)
library(lmerTest)
model_richness <- lmer(n ~ Periodo+(1|Bosque/Year), data = pol_richness_plot_gorbeia_XY_periodo %>%
                          mutate(Periodo =as.factor(Periodo),
                                 Bosque = as.factor(Bosque),Year = as.factor(Year)))
summary(model_richness)

model_abundance <- lmer(n ~ Periodo+(1|Bosque/Year), data = pol_abundance_plot_gorbeia_XY_periodo %>%
                          mutate(Periodo =as.factor(Periodo),
                                 Bosque = as.factor(Bosque),Year = as.factor(Year)))
summary(model_abundance)



plot_names <- c(
  `1` = "Plot 1",
  `2` = "Plot 2",
  `3` = "Plot 3",
  `4` = "Plot 4",
  `5` = "Plot 5"
)

png("figures/pollinator_richness_gorbea_2020.png", width=1200*5, height = 300*5, res=300*2)
ggplot(pol_richness_plot_gorbeia_XY %>% filter(Year==2020), aes(X, Y, fill= n)) + 
  geom_tile()+
  facet_wrap(~Bosque,ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Accumulated pollinator richness (Gorbeia N.P., 2020)",x="X(m)",y="Y(m)",
       fill = "Number of\npoll. species")#+theme(legend.position = "bottom")
dev.off()

png("figures/pollinator_richness_gorbea_2020_periodo.png", width=1200*5, height = 450*5, res=300*2)
ggplot(pol_richness_plot_gorbeia_XY_periodo %>% filter(Year==2020), aes(X, Y, fill= n)) + 
  geom_tile()+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Pollinator richness by flowering period (Gorbeia N.P., 2020)",x="X(m)",y="Y(m)",
       fill = "Number of\npoll. species")#+theme(legend.position = "bottom")
dev.off()

png("figures/pollinator_richness_gorbea_2021.png", width=1200*5, height = 300*5, res=300*2)
ggplot(pol_richness_plot_gorbeia_XY %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  facet_wrap(~Bosque, ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Accumulated pollinator richness (Gorbeia N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\npoll. species")#+theme(legend.position = "bottom")
dev.off()

png("figures/pollinator_richness_gorbea_2021_periodo.png", width=1200*5, height = 700*5, res=300*2)
ggplot(pol_richness_plot_gorbeia_XY_periodo %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Pollinator richness by flowering period (Gorbeia N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\npoll. species")#+theme(legend.position = "bottom")
dev.off()


# Abundance plots
png("figures/pol_abundance_gorbea_2020.png", width=1200*5, height = 300*5, res=300*2)
ggplot(pol_abundance_plot_gorbeia_XY %>% filter(Year==2020), aes(X, Y, fill= (n))) + 
  geom_tile()+
  facet_wrap(~Bosque, ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Accumulated pollinator abundance (Gorbeia N.P., 2020)",x="X(m)",y="Y(m)",
       fill = "Number of\npollinators")#+theme(legend.position = "bottom")
dev.off()


png("figures/pol_abundance_gorbea_2020_periodo.png", width=1200*5, height = 450*5, res=300*2)
ggplot(pol_abundance_plot_gorbeia_XY_periodo %>% filter(Year==2020), aes(X, Y, fill= n)) + 
  geom_tile()+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Pollinator abundance by flowering period (Gorbeia N.P., 2020)",x="X(m)",y="Y(m)",
       fill = "Number of\npollinators")#+theme(legend.position = "bottom")
dev.off()




png("figures/pol_abundance_gorbea_2021.png",width=1200*5, height = 300*5, res=300*2)
ggplot(pol_abundance_plot_gorbeia_XY %>% filter(Year==2021), aes(X, Y, fill= (n))) + 
  geom_tile()+
  facet_wrap(~Bosque, ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Accumulated pollinator abundance (Gorbeia N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\npollinators")#+theme(legend.position = "bottom")
dev.off()

png("figures/pol_abundance_gorbea_2021_periodo.png", width=1200*5, height = 700*5, res=300*2)
ggplot(pol_abundance_plot_gorbeia_XY_periodo %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Pollinator abundance by flowering period (Gorbeia N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\npollinators")#+theme(legend.position = "bottom")
dev.off()




pol_richness_plot_gorbeia_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))  %>% filter(Year==2020)


pol_richness_plot_gorbeia_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))  %>% filter(Year==2021)


pol_abundance_plot_gorbeia_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))  %>% filter(Year==2020)


pol_abundance_plot_gorbeia_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))  %>% filter(Year==2021)


#############################
# DONANA ####################
#############################

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

pollinator_richness_censuses_donana <- flights_raw_donana %>% dplyr::select(Year, X, Y, Polinizador, Bosque, Periodo,Week_ISO) %>%
  unique() %>% group_by(Year, X, Y, Bosque, Periodo, Week_ISO) %>% count() %>% rename(poll_richness = n)

pollinator_abundance_censuses_donana <- flights_raw_donana %>% 
  dplyr::select(Year, X, Y, Codigo_vuelo, Bosque, Periodo,Week_ISO) %>%
  unique() %>% group_by(Year, X, Y, Bosque, Periodo,Week_ISO) %>% count() %>% rename(poll_abundance = n)


poll_richness_plot_donana <- pollinator_richness_censuses_donana %>% ungroup() %>%
  dplyr::select(Year, Bosque,poll_richness) %>% unique() %>% group_by(Bosque) %>% count(poll_richness)
mean(poll_richness_plot_donana$n)
sd(poll_richness_plot_donana$n)


# Heatmaps
pol_richness_plot_donana_XY <- pollinator_richness_censuses_donana %>% ungroup() %>%
  dplyr::select(Year,Bosque, X,Y,poll_richness) %>%
  group_by(Year,Bosque,X,Y) %>% count(wt=poll_richness)


pol_richness_plot_donana_XY_periodo <- pollinator_richness_censuses_donana %>% ungroup() %>%
  dplyr::select(Year,Bosque, X,Y,poll_richness,Periodo) %>%
  group_by(Year,Bosque,X,Y,Periodo) %>% count(wt=poll_richness)%>%
  mutate(Periodo = paste0("Period ",Periodo))


pol_abundance_plot_donana_XY <- pollinator_abundance_censuses_donana %>% ungroup() %>%
  dplyr::select(Year,Bosque,X,Y,poll_abundance) %>% group_by(Year,Bosque,X,Y) %>% 
  count(wt=poll_abundance)

pol_abundance_plot_donana_XY_periodo <- pollinator_abundance_censuses_donana %>% 
  ungroup() %>% dplyr::select(Year,Bosque,X,Y,poll_abundance,Periodo) %>% 
  group_by(Year,Bosque,X,Y,Periodo) %>% 
  count(wt=poll_abundance) %>%
  mutate(Bosque = paste0("Plot ",Bosque),
         Periodo = paste0("Period ",Periodo))

# Models
model_donana_richness <- lmer(n ~ Periodo+(1|Bosque/Year), data = pol_richness_plot_donana_XY_periodo %>%
                         mutate(Periodo =as.factor(Periodo),
                                Bosque = as.factor(Bosque),Year = as.factor(Year)))
summary(model_donana_richness)

model_donana_abundance <- lmer(n ~ Periodo+(1|Bosque/Year), data = pol_abundance_plot_donana_XY_periodo %>%
                          mutate(Periodo =as.factor(Periodo),
                                 Bosque = as.factor(Bosque),Year = as.factor(Year)))

summary(model_donana_richness)



pol_richness_plot_donana_XY$Bosque[pol_richness_plot_donana_XY$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"
pol_richness_plot_donana_XY_periodo$Bosque[pol_richness_plot_donana_XY_periodo$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"

pol_richness_plot_donana_XY %>% ungroup() %>%  group_by(Year,Bosque) %>% summarise(mean = mean(n), sd =sd(n))
pol_richness_plot_donana_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% summarise(mean = mean(n), sd =sd(n))

pol_abundance_plot_donana_XY$Bosque[pol_abundance_plot_donana_XY$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"
pol_abundance_plot_donana_XY_periodo$Bosque[pol_abundance_plot_donana_XY_periodo$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"

pol_abundance_plot_donana_XY_periodo %>% ungroup() %>%  group_by(Year,Bosque,Periodo) %>% 
  summarise(mean = mean(n), sd =sd(n))  %>% filter(Year==2021)

png("figures/pollinator_richness_donana_2021.png", width=1200*5, height = 300*5, res=300*2)
ggplot(pol_richness_plot_donana_XY %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_wrap(~Bosque,ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Accumulated pollinator richness (Doñana N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\npoll. species")#+theme(legend.position = "bottom")
dev.off()

png("figures/pollinator_richness_donana_2021_periodo.png", width=1200*5, height = 700*5, res=300*2)
ggplot(pol_richness_plot_donana_XY_periodo %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Pollinator richness by flowering period (Doñana N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\npoll. species")#+theme(legend.position = "bottom")
dev.off()

png("figures/pol_abundance_donana_2021.png",width=1200*5, height = 300*5, res=300*2)
ggplot(pol_abundance_plot_donana_XY %>% filter(Year==2021), aes(X, Y, fill= (n))) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_wrap(~Bosque,ncol = 5)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Accumulated pollinator abundance (Doñana N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\npollinators")#+theme(legend.position = "bottom")
dev.off()

png("figures/pol_abundance_donana_2021_periodo.png", width=1200*5, height = 700*5, res=300*2)
ggplot(pol_abundance_plot_donana_XY_periodo %>% filter(Year==2021), aes(X, Y, fill= n)) + 
  geom_tile()+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  facet_grid(Periodo~Bosque)+scale_fill_gradientn(colors = (wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  labs(title = "Pollinator abundance by flowering period (Doñana N.P., 2021)",x="X(m)",y="Y(m)",
       fill = "Number of\npollinators")#+theme(legend.position = "bottom")
dev.off()
