library(tidyverse)
library(lubridate)

# Load census data and fixed dates

flora20_raw <- read_csv("data/donana/cuad_flora_donana_20.csv") %>% 
  mutate(Fecha = lubridate::dmy(Dia)) %>%
  rename(Flores = Numero.flores, Ronda = Tanda, X = x, Y = y,
         Planta = Especie.planta) %>% 
  dplyr::select(-X1, -GPS, -Dia, -ID, -Codigo)  

flora21_raw <- read_csv("data/donana/cuad_flora_donana_21.csv") %>% 
  mutate(Fecha = lubridate::dmy(Fecha)) %>%
  rename(Flores = flores_disponibles,
         Planta = SP) %>% 
  dplyr::select(-X1, -Escapos_indivi, -i) 


# Load flights data
flights20_raw <- read_csv("results/donana/foraging_Donana_2020.csv") %>%
  mutate(Codigo_vuelo = as.character(Codigo_vuelo))
flights21_raw <- read_csv("results/donana/foraging_Donana_2021.csv")

# Aggregate census and fight information
# NOTE: We do not consider data from 2020 because some plant sp are not
# included in floral censuses, among other reasons (see Donana multilayers for 2020)

flora_census <- flora21_raw #bind_rows(flora20_raw, flora21_raw)

plants_census <- flora21_raw %>% #bind_rows(flora20_raw, flora21_raw) %>% 
  dplyr::select(Planta) %>%
  pull() %>% unique() %>% sort()

plants_flights <- flights21_raw %>% #bind_rows(flights20_raw,flights21_raw) %>% 
  dplyr::select(Planta) %>%
  pull() %>% unique() %>% sort()



#------------------------------------------------------

# Add year info
flora_census$Year <- year(flora_census$Fecha)
flora_census$Day_ISO <- yday(flora_census$Fecha)
flora_census$Day <- (flora_census$Day_ISO-(min(flora_census$Day_ISO)-1))
flora_census$Week_ISO <- strftime(flora_census$Fecha, format = "%V")
flora_census$Week <- ceiling((flora_census$Day)/7)


# All entries contain spaces
flora_census$Planta[grep(" ",flora_census$Planta)]

# Labels also contain dots
flora_census$Planta[grep("\\.",flora_census$Planta)]

# Remove spaces from labels
flora_census$Planta <- sub(" ", "_", flora_census$Planta)
flora_census$Planta <- sub(" ", "_", flora_census$Planta)

# Remove dots from labels
flora_census$Planta <- sub("\\.", "", flora_census$Planta)

plants_flights
plants_census <- unique(flora_census$Planta) %>% sort()
plants_census

plants_flights[!(plants_flights %in% plants_census)]

# clean flora_census dataframe

flora_census$Fecha %>% unique() %>% sort()
flora_census$Day_ISO %>% unique() %>% sort()


# Fix Periodo info:
# 2021 -> Periodo 1: March
# 2021 -> Periodo 3: May

flora_census$Periodo <- 1
flora_census$Periodo[flora_census$Day_ISO > 90] <- 2
flora_census$Periodo[flora_census$Day_ISO > 123] <- 3

# Save flora data
write_csv(flora_census,"results/donana/flora_census_21.csv")

