library(tidyverse)
library(lubridate)

# Load census data and fixed dates

flora20_raw <- read_csv("data/gorbea/cuad_flora_20_clean.csv") %>% 
  mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y")) %>%
  rename(Flores = flores, Periodo = periodo)

flora21_raw <- read_csv("data/gorbea/cuad_flora_21_clean.csv") %>% 
  filter(Planta != "0") %>% mutate(Fecha = as.character(Fecha))

flora21_raw$Fecha <- gsub("0021", "2021", flora21_raw$Fecha) 
flora21_raw$Fecha <- as.Date(flora21_raw$Fecha, format = "%Y-%m-%d")

# Load flights data
flights20_raw <- read_csv("results/gorbea/foraging_Gorbea_2020.csv")
flights21_raw <- read_csv("results/gorbea/foraging_Gorbea_2021.csv")

# Aggregate census and fight information
flora_census <- bind_rows(flora20_raw, flora21_raw) %>% filter(Planta != "0")

plants_census <- bind_rows(flora20_raw, flora21_raw) %>% select(Planta) %>%
  pull() %>% unique() %>% sort()

plants_flights <- bind_rows(flights20_raw,flights21_raw) %>% select(Planta) %>%
  pull() %>% unique() %>% sort()


#------------------------------------------------------

# Add year info
flora_census$Year <- year(flora_census$Fecha)
flora_census$Day_ISO <- yday(flora_census$Fecha)
flora_census$Day <- (flora_census$Day_ISO-(min(flora_census$Day_ISO)-1))
flora_census$Week_ISO <- strftime(flora_census$Fecha, format = "%V")
flora_census$Week <- ceiling((flora_census$Day)/7)

# Clean species names
flora_census$Planta[grep("Thymus praecox subsp. polytrichus",flora_census$Planta)] <- "Thymus praecox"
flora_census$Planta[grep("Pedicularis sylvatica subsp. sylvatica",flora_census$Planta)] <- "Pedicularis sylvatica"
flora_census$Planta[grep("Teucrium pyrenaicum subsp. pyrenaicum",flora_census$Planta)] <- "Teucrium_pyrenaicum"

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

which(!(plants_flights %in% plants_census))

flora_census$Planta[grep("Cerastium_sp",flora_census$Planta)] <- "Cerastium_vulgare"
flora_census$Planta[grep("Galium_saxatila",flora_census$Planta)] <- "Gallium_saxatile"
plants_census <- unique(flora_census$Planta) %>% sort()


which(!(plants_flights %in% plants_census)) # Ulex_europaeus


# clean flora_census dataframe

flora_census_clean <- flora_census %>% select(-xy, -xy2, -X.Y, -Observaciones) %>% 
  rename(X = x, Y = y) %>% mutate(X = as.numeric(X), Y = as.numeric(Y))

# Fix Periodo info:
# 2020 -> Periodo 1: day <= 149
# 2021 -> Periodo 1: day <= 127
# 2021 -> Periodo 3: day > 152

flora_census_clean$Periodo <- 1
flora_census_clean$Periodo[flora_census_clean$Year==2020 & flora_census_clean$Day_ISO > 149] <- 2
flora_census_clean$Periodo[flora_census_clean$Year==2021 & flora_census_clean$Day_ISO > 127] <- 2
flora_census_clean$Periodo[flora_census_clean$Year==2021 & flora_census_clean$Day_ISO > 152] <- 3

# Save flora data
write_csv(flora_census_clean,"results/gorbea/flora_census_20_21.csv")

