library(tidyverse)
library(lubridate)

# Load fruitset data for 2021 and fixed dates

fruitset_census <- read_csv("data/donana/fruit_set_21.csv") %>% 
  mutate(Fecha = lubridate::dmy(Fecha)) %>%
  rename(labelled_flowers = `flores etiquetadas`,
         number_fruits = `frutos recogidos`,
         number_plants = planta,
         Planta = SP) %>% 
  dplyr::select(-X12, -comentarios,-number_plants) 


# Load flights data
flights21_raw <- read_csv("results/donana/foraging_Donana_2021.csv")

# Aggregate census and fight information
# NOTE: We do not consider data from 2020 because some plant sp are not
# included in floral censuses, among other reasons (see Donana multilayers for 2020)

plants_census <- fruitset_census %>% #bind_rows(flora20_raw, flora21_raw) %>% 
  dplyr::select(Planta) %>%
  pull() %>% unique() %>% sort()

plants_flights <- flights21_raw %>% #bind_rows(flights20_raw,flights21_raw) %>% 
  dplyr::select(Planta) %>%
  pull() %>% unique() %>% sort()



#------------------------------------------------------

# Add year info
fruitset_census$Year <- year(fruitset_census$Fecha)
fruitset_census$Day_ISO <- yday(fruitset_census$Fecha)
fruitset_census$Day <- (fruitset_census$Day_ISO-(min(fruitset_census$Day_ISO)-1))
fruitset_census$Week_ISO <- strftime(fruitset_census$Fecha, format = "%V")
fruitset_census$Week <- ceiling((fruitset_census$Day)/7)


# All entries contain spaces
fruitset_census$Planta[grep(" ",fruitset_census$Planta)]

# Labels also contain dots
fruitset_census$Planta[grep("\\.",fruitset_census$Planta)]

# Remove spaces from labels
fruitset_census$Planta <- sub(" ", "_", fruitset_census$Planta)
fruitset_census$Planta <- sub(" ", "_", fruitset_census$Planta)

# Remove dots from labels
fruitset_census$Planta <- sub("\\.", "", fruitset_census$Planta)

# Sanity check
plants_flights
plants_census <- unique(fruitset_census$Planta) %>% sort()
plants_census

plants_flights[!(plants_flights %in% plants_census)]
plants_census[!(plants_census %in% plants_flights)]

# Fix fruit set sp names
fruitset_census$Planta[fruitset_census$Planta=="Halimiun_calycinum"] <- "Halimium_calycinum"
fruitset_census$Planta[fruitset_census$Planta=="Halimiun_halimifolium"] <- "Halimium_halimifolium"


# Sanity check
plants_census <- unique(fruitset_census$Planta) %>% sort()
plants_census
plants_census[!(plants_census %in% plants_flights)]

# clean fruitset_census dataframe

fruitset_census$Fecha %>% unique() %>% sort()
fruitset_census$Day_ISO %>% unique() %>% sort()


# Fix Periodo info:
# 2021 -> Periodo 1: March
# 2021 -> Periodo 3: May

fruitset_census$Periodo <- 1
fruitset_census$Periodo[fruitset_census$Day_ISO > 90] <- 2
fruitset_census$Periodo[fruitset_census$Day_ISO > 123] <- 3


# Fix bosque names
fruitset_census$Bosque[fruitset_census$Bosque=="Pinar Villamanrique Este"] <- "Pinar Villamanrique Este (Chaparral)"


# Save fruitset data
write_csv(fruitset_census,"results/donana/fruitset_census_21.csv")

