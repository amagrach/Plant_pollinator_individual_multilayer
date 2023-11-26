
#load libraries
library(tidyverse)
library(lubridate)

####################################################################
# Loadind Plant-pollinator dataset (Caracoles): visits, abundances, seeds
####################################################################

# coord2 data contain forbiden separators (","). To avoid errors,
# firstly, we get the coord2 data independently, and, then, we use it
# to replace the damaged data.

fitness_data2_aux <- read_csv("data/gorbea/cuad_gorbea_2020_clean.csv")
fitness_data2_aux$Y <- as.numeric(fitness_data2_aux$Y)



x <- fitness_data2_aux %>% group_by(Periodo_hora,Hora) %>% count()

fitness_data2_aux$coord2 <- sub(",", ".", fitness_data2_aux$coord2)
coord2_2020 <- as.numeric(fitness_data2_aux$coord2)

# Sanity checks
fitness_data2_aux %>% filter(X>24)
fitness_data2_aux %>% filter(Y>24)


fitness_data2 <- read_csv("data/gorbea/cuad_gorbea_2020_clean.csv",
                          locale = readr::locale(encoding = "latin1"))


fitness_data2$coord2 <- coord2_2020
fitness_data2$Y <- as.numeric(fitness_data2$Y)

fitness_data2$Periodo_fecha  %>% unique() %>% sort()
fitness_data2$Periodo_hora  %>% unique() %>% sort()
fitness_data2$Bosque  %>% unique() %>% sort()

# Add year info
fitness_data2$Year <- 2020
fitness_data2$Day_ISO <- yday(fitness_data2$Fecha)
fitness_data2$Day <- (fitness_data2$Day_ISO-(min(fitness_data2$Day_ISO)-1))
fitness_data2$Week_ISO <- strftime(fitness_data2$Fecha, format = "%V")
fitness_data2$Week <- ceiling((fitness_data2$Day)/7)

fitness_data2$Week %>% unique() %>% sort()

# Fix X, Y coordinates
fitness_data2$X <- round(fitness_data2$coord2,0)
fitness_data2$Y <- round(100*(fitness_data2$coord2-fitness_data2$X),0)

# Sanity checks
sum(fitness_data2$X>=0)==nrow(fitness_data2)
sum(fitness_data2$Y>=0)==nrow(fitness_data2)
sum(fitness_data2$X<=24)==nrow(fitness_data2)
sum(fitness_data2$Y<=24)==nrow(fitness_data2)

# Number of sites without coordinates
sum(is.na(fitness_data2$X))+sum(is.na(fitness_data2$Y)) # 0

# Review names: Remove dots and spaces

fitness_data2$Planta %>% unique() %>% sort()
fitness_data2$Polinizador %>% unique() %>% sort()


# All entries contain spaces
fitness_data2$Planta[grep(" ",fitness_data2$Planta)] 
fitness_data2$Polinizador[grep(" ",fitness_data2$Polinizador)]

# Labels also contain dots
fitness_data2$Planta[grep("\\.",fitness_data2$Planta)]
fitness_data2$Polinizador[grep("\\.",fitness_data2$Polinizador)]

# Remove spaces from labels
fitness_data2$Planta <- sub(" ", "_", fitness_data2$Planta)
fitness_data2$Polinizador <- sub(" ", "_", fitness_data2$Polinizador)
fitness_data2$Polinizador <- sub(" ", "_", fitness_data2$Polinizador)

# Remove dots from labels
fitness_data2$Planta <- sub("\\.", "", fitness_data2$Planta)
fitness_data2$Polinizador <- sub("\\.", "", fitness_data2$Polinizador)

#Sanity check
# All entries contain spaces
fitness_data2$Planta[grep(" ",fitness_data2$Planta)] 
fitness_data2$Polinizador[grep(" ",fitness_data2$Polinizador)]

# Labels also contain dots
fitness_data2$Planta[grep("\\.",fitness_data2$Planta)]
fitness_data2$Polinizador[grep("\\.",fitness_data2$Polinizador)]

# Recheck labels
fitness_data2$Planta %>% unique() %>% sort()
fitness_data2$Polinizador %>% unique() %>% sort()

# Fix plant names
fitness_data2$Planta[grep("Cerastium_sp",fitness_data2$Planta)] <- "Cerastium_vulgare"
fitness_data2$Planta[grep("Chinopodium_sp",fitness_data2$Planta)] <- "Clinopodium_sp"
fitness_data2$Planta[grep("Erinus_sp",fitness_data2$Planta)] <- "Clinopodium_sp"
fitness_data2$Planta[grep("Euphrasia_sp",fitness_data2$Planta)] <- "Euphrasia_alpina"
fitness_data2$Planta[grep("Galium_sp",fitness_data2$Planta)] <- "Gallium_saxatile"
fitness_data2$Planta[grep("Gallium_sp",fitness_data2$Planta)] <- "Gallium_saxatile"
fitness_data2$Planta[grep("Helianthemum_sp",fitness_data2$Planta)] <- "Helianthemum_nummularium"
fitness_data2$Planta[grep("Linaria_sp",fitness_data2$Planta)] <- "Linaria_propinqua"
fitness_data2$Planta[grep("Moeringia_trinervia",fitness_data2$Planta)] <- "Moehringia_trinervia"
fitness_data2$Planta[grep("Myosotis_sp",fitness_data2$Planta)] <- "Myosotis_lamottiana"
fitness_data2$Planta[grep("Scorzonera_sp",fitness_data2$Planta)] <- "Scorzonera_humilis"
fitness_data2$Planta[grep("Teucrium_pyrenaica",fitness_data2$Planta)] <- "Teucrium_pyrenaicum"
fitness_data2$Planta[grep("Thymus_sp",fitness_data2$Planta)] <- "Thymus_praecox"
# Save data

fitness_data2_final <- fitness_data2 %>%
  select(Year,Day_ISO,Week_ISO,Week,
         Periodo_hora,Bosque,Codigo_vuelo_dia,Codigo_vuelo,
         Polinizador,Codigo_captura,Planta,X,Y)

write_csv(fitness_data2_final,"results/gorbea/foraging_Gorbea_2020.csv")

####################################################
# DATA FOR 2021

fitness_data21 <- read_csv("data/gorbea/cuad_gorbea_2021_clean.csv") %>% 
  rename(coord2 = xy) %>%
  select(-X1) # row number

# Exploring the data
fitness_data21$Periodo_fecha  %>% unique() %>% sort()
fitness_data21$Periodo_hora  %>% unique() %>% sort()
fitness_data21$Bosque  %>% unique() %>% sort()

# Add year info

fitness_data21$Year <- 2021
fitness_data21$Day_ISO <- yday(fitness_data21$Fecha)
fitness_data21$Day <- (fitness_data21$Day_ISO-(min(fitness_data21$Day_ISO)-1))
fitness_data21$Week_ISO <- strftime(fitness_data21$Fecha, format = "%V")
fitness_data21$Week <- ceiling((fitness_data21$Day)/7)

fitness_data21$Week %>% unique() %>% sort()

# Fix X, Y coordinates
fitness_data21$coord2[fitness_data21$coord2==7.27] <- 7.24
fitness_data21$Y[fitness_data21$Y=="27"] <- "24"
fitness_data21$X <- as.numeric(fitness_data21$X)
fitness_data21$Y <- as.numeric(fitness_data21$Y)

# Sanity checks
sum(fitness_data21$X>=0)==nrow(fitness_data21)
sum(fitness_data21$Y>=0)==nrow(fitness_data21)
sum(fitness_data21$X<=24)==nrow(fitness_data21)
sum(fitness_data21$Y<=24)==nrow(fitness_data21)


# Number of sites without coordinates
sum(is.na(fitness_data21$X))+sum(is.na(fitness_data21$Y))

# Clean species names

fitness_data21$Planta %>% unique() %>% sort()
fitness_data21$Polinizador %>% unique() %>% sort()
fitness_data21$Planta[grep("Thymus praecox subsp. polytrichus",fitness_data21$Planta)] <- "Thymus praecox"
fitness_data21$Planta[grep("Pedicularis sylvatica subsp. sylvatica",fitness_data21$Planta)] <- "Pedicularis sylvatica"


fitness_data21$Polinizador[grep("Andrena cf. Morio",fitness_data21$Polinizador)] <- "Andrena cf morio"
fitness_data21$Polinizador[grep("?=80",fitness_data21$Polinizador)] <- "?=C80"
fitness_data21$Polinizador[grep("?=81",fitness_data21$Polinizador)] <- "?=C81"




# All entries contain spaces
fitness_data21$Planta[grep(" ",fitness_data21$Planta)] 
fitness_data21$Polinizador[grep(" ",fitness_data21$Polinizador)]

# Labels also contain dots
fitness_data21$Planta[grep("\\.",fitness_data21$Planta)]
fitness_data21$Polinizador[grep("\\.",fitness_data21$Polinizador)]

# Remove spaces from labels
fitness_data21$Planta <- sub(" ", "_", fitness_data21$Planta)
fitness_data21$Planta <- sub(" ", "_", fitness_data21$Planta)
fitness_data21$Polinizador <- sub(" ", "_", fitness_data21$Polinizador)
fitness_data21$Polinizador <- sub(" ", "_", fitness_data21$Polinizador)

# Remove dots from labels
fitness_data21$Planta <- sub("\\.", "", fitness_data21$Planta)
fitness_data21$Polinizador <- sub("\\.", "", fitness_data21$Polinizador)

#Sanity check
# All entries contain spaces
fitness_data21$Planta[grep(" ",fitness_data21$Planta)] 
fitness_data21$Polinizador[grep(" ",fitness_data21$Polinizador)]


# Fix plant names
fitness_data21$Planta[grep("Cerastium_sp",fitness_data21$Planta)] <- "Cerastium_vulgare"
fitness_data21$Planta[grep("Chinopodium_sp",fitness_data21$Planta)] <- "Clinopodium_sp"
fitness_data21$Planta[grep("Erinus_sp",fitness_data21$Planta)] <- "Clinopodium_sp"
fitness_data21$Planta[grep("Euphrasia_sp",fitness_data21$Planta)] <- "Euphrasia_alpina"
fitness_data21$Planta[grep("Galium_sp",fitness_data21$Planta)] <- "Gallium_saxatile"
fitness_data21$Planta[grep("Gallium_sp",fitness_data21$Planta)] <- "Gallium_saxatile"
fitness_data21$Planta[grep("Helianthemum_sp",fitness_data21$Planta)] <- "Helianthemum_nummularium"
fitness_data21$Planta[grep("Linaria_sp",fitness_data21$Planta)] <- "Linaria_propinqua"
fitness_data21$Planta[grep("Moeringia_trinervia",fitness_data21$Planta)] <- "Moehringia_trinervia"
fitness_data21$Planta[grep("Myosotis_sp",fitness_data21$Planta)] <- "Myosotis_lamottiana"
fitness_data21$Planta[grep("Scorzonera_sp",fitness_data21$Planta)] <- "Scorzonera_humilis"
fitness_data21$Planta[grep("Teucrium_pyrenaica",fitness_data21$Planta)] <- "Teucrium_pyrenaicum"
fitness_data21$Planta[grep("Thymus_sp",fitness_data21$Planta)] <- "Thymus_praecox"

# Recheck labels
fitness_data21$Planta %>% unique() %>% sort()
fitness_data21$Polinizador %>% unique() %>% sort()

fitness_data21_final <- fitness_data21 %>%
  select(Year,Day_ISO,Week_ISO,Week,
         Periodo_hora,Bosque,Codigo_vuelo_dia,Codigo_vuelos,
         Polinizador,Codigo_captura,Planta,X,Y) %>%
  rename(Codigo_vuelo = Codigo_vuelos)

write_csv(fitness_data21_final,"results/gorbea/foraging_Gorbea_2021.csv")

