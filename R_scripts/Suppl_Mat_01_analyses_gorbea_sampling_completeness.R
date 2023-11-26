# Some consultants are unaware of the difference between rarefaction and species
# accumulation curves, and often calculate and present rarefaction curves as species
# accumulation curves in fauna reports. Rarefaction curves are useful for comparing
# species richness values for different sampling efforts. Rarefaction cannot be used for
# extrapolation as it does not provide an estimate of asymptotic richness.

library(tidyverse)
library(lubridate)
library(vegan)
library(iNEXT)

plot_labs <-c(
  "Plot 1",
  "Plot 2",
  "Plot 3",
  "Plot 4",
  "Plot 5"
)
names(plot_labs) <- c(
  '1',
  '2',
  '3',
  '4',
  '5'
)

#####################################################################
# Load flight sequences data

flights21 <- read_csv("results/gorbea/foraging_Gorbea_2021.csv")
flights20 <- read_csv("results/gorbea/foraging_Gorbea_2020.csv")

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


##################################
# INTERACTIONS
##################################



counts_interactions_subplot_2020 <- flights_raw %>% filter(Year==2020) %>%
  mutate(ID = paste(Polinizador,Planta,sep = "_")) %>% 
  group_by(plot,ID,Day_ISO) %>%
  count() %>% rename(Visits_tot = n) %>%
  spread(ID,Visits_tot)

counts_interactions_subplot_2021 <- flights_raw %>% filter(Year==2021) %>%
  mutate(ID = paste(Polinizador,Planta,sep = "_")) %>% 
  group_by(plot,ID,Day_ISO) %>%
  count() %>% rename(Visits_tot = n) %>%
  spread(ID,Visits_tot)


counts_interactions_subplot_2020[is.na(counts_interactions_subplot_2020)] <- 0
counts_interactions_subplot_2021[is.na(counts_interactions_subplot_2021)] <- 0


#################
# RAREFACTION CURVES FOR 2020

x_2020 <- counts_interactions_subplot_2020 %>% ungroup() %>% select(-Day_ISO) %>% 
  group_by(plot) %>% summarise_all(sum)
col_names_x_2020 <- paste0("",x_2020$plot)

x_2020 <- x_2020 %>% ungroup() %>% select(-plot)

rownames(x_2020) <- col_names_x_2020

x_2020.list <- setNames(split(x_2020, seq(nrow(x_2020))), rownames(x_2020))

# Convertir la lista de tibbles en una lista de números
x_2020.number.list <- x_2020.list
x_2020.number.list$`Plot 1` <- unlist(x_2020.number.list$`Plot 1`) %>% as.numeric()
x_2020.number.list$`Plot 2` <- unlist(x_2020.number.list$`Plot 2`) %>% as.numeric()
x_2020.number.list$`Plot 3` <- unlist(x_2020.number.list$`Plot 3`) %>% as.numeric()
x_2020.number.list$`Plot 4` <- unlist(x_2020.number.list$`Plot 4`) %>% as.numeric()
x_2020.number.list$`Plot 5` <- unlist(x_2020.number.list$`Plot 5`) %>% as.numeric()

out_2020 <- iNEXT(x_2020.number.list, q=c(0), datatype="abundance")
# Sample‐size‐based R/E curves, separating by "site""

library(scales)
g_2020 <- ggiNEXT(out_2020,type = 2)+
  #scale_shape_manual(values = rep(19,91))+
  theme_bw(base_size = 18)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(x="Number of interactions",
       title = "Gorbeia N.P. (2020)")

g_2020

png("Figures/figA31_2020.png",
    width = 11.69*0.5, # The width of the plot in inches
    height = 8.27*0.65, units = "in", res=300*2)
g_2020
dev.off()

#################
# RAREFACTION CURVES FOR 2021

x_2021 <- counts_interactions_subplot_2021 %>% ungroup() %>% select(-Day_ISO) %>% 
  group_by(plot) %>% summarise_all(sum)
col_names_x_2021 <- paste0("",x_2021$plot)

x_2021 <- x_2021 %>% ungroup() %>% select(-plot)

rownames(x_2021) <- col_names_x_2021

x_2021.list <- setNames(split(x_2021, seq(nrow(x_2021))), rownames(x_2021))

# Convertir la lista de tibbles en una lista de números
x_2021.number.list <- x_2021.list
x_2021.number.list$`Plot 1` <- unlist(x_2021.number.list$`Plot 1`) %>% as.numeric()
x_2021.number.list$`Plot 2` <- unlist(x_2021.number.list$`Plot 2`) %>% as.numeric()
x_2021.number.list$`Plot 3` <- unlist(x_2021.number.list$`Plot 3`) %>% as.numeric()
x_2021.number.list$`Plot 4` <- unlist(x_2021.number.list$`Plot 4`) %>% as.numeric()
x_2021.number.list$`Plot 5` <- unlist(x_2021.number.list$`Plot 5`) %>% as.numeric()

out_2021 <- iNEXT(x_2021.number.list, q=c(0), datatype="abundance")
# Sample‐size‐based R/E curves, separating by "site""

library(scales)
g_2021 <- ggiNEXT(out_2021,type = 2)+
  #scale_shape_manual(values = rep(19,91))+
  theme_bw(base_size = 18)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(x="Number of interactions",
       title = "Gorbeia N.P. (2021)")

g_2021

png("Figures/figA31_2021.png",
    width = 11.69*0.5, # The width of the plot in inches
    height = 8.27*0.65, units = "in", res=300*2)
g_2021
dev.off()

