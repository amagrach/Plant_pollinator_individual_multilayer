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

flights21 <- read_csv("results/donana/foraging_Donana_2021.csv")


##################################
# INTERACTIONS
##################################


counts_interactions_subplot_2021 <- flights21 %>% filter(Year==2021) %>%
  mutate(ID = paste(Polinizador,Planta,sep = "_")) %>% 
  group_by(Bosque,ID,Day_ISO) %>%
  count() %>% rename(Visits_tot = n) %>%
  spread(ID,Visits_tot)

counts_interactions_subplot_2021[is.na(counts_interactions_subplot_2021)] <- 0



#################
# RAREFACTION CURVES FOR 2021

x_2021 <- counts_interactions_subplot_2021 %>% ungroup() %>% select(-Day_ISO) %>% 
  group_by(Bosque) %>% summarise_all(sum)
col_names_x_2021 <- paste0("",x_2021$Bosque)
col_names_x_2021[col_names_x_2021=="Pinar Villamanrique Este (Chaparral)"] <- "Villaman. Este"
col_names_x_2021[col_names_x_2021=="Pinar Villamanrique Sur"] <- "Villaman. Sur"
col_names_x_2021[col_names_x_2021=="Pinar Puebla"] <- "Puebla"
col_names_x_2021[col_names_x_2021=="Pinar Hinojos"] <- "Hinojos"
col_names_x_2021[col_names_x_2021=="Pinar Aznalcazar"] <- "Aznalcazar"

x_2021 <- x_2021 %>% ungroup() %>% select(-Bosque)

rownames(x_2021) <- col_names_x_2021

x_2021.list <- setNames(split(x_2021, seq(nrow(x_2021))), rownames(x_2021))

# Convertir la lista de tibbles en una lista de números
x_2021.number.list <- x_2021.list
x_2021.number.list$`Villaman. Este` <- unlist(x_2021.number.list$`Villaman. Este`) %>% as.numeric()
x_2021.number.list$`Villaman. Sur` <- unlist(x_2021.number.list$`Villaman. Sur`) %>% as.numeric()
x_2021.number.list$`Puebla` <- unlist(x_2021.number.list$`Puebla`) %>% as.numeric()
x_2021.number.list$`Hinojos` <- unlist(x_2021.number.list$`Hinojos`) %>% as.numeric()
x_2021.number.list$`Aznalcazar` <- unlist(x_2021.number.list$`Aznalcazar`) %>% as.numeric()

out_2021 <- iNEXT(x_2021.number.list, q=c(0), datatype="abundance")
# Sample‐size‐based R/E curves, separating by "site""

library(scales)
g_2021 <- ggiNEXT(out_2021,type = 2)+
  #scale_shape_manual(values = rep(19,91))+
  theme_bw(base_size = 18)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(x="Number of interactions",
       title = "Doñana N.P. (2021)")

g_2021

png("Figures/figA32_2021.png",
    width = 11.69, # The width of the plot in inches
    height = 8.27*0.65, units = "in", res=300*2)
g_2021
dev.off()

