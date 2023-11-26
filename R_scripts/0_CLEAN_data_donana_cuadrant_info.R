
#load libraries
library(tidyverse)
library(lubridate)

####################################################################
# Loadind Plant-pollinator dataset (Doñana): visits, abundances, seeds
####################################################################

# coord2 data contain forbiden separators (","). To avoid errors,
# firstly, we get the coord2 data independently, and, then, we use it
# to replace the damaged data.

plant_position_info <- read_csv("data/donana/cuad_donana_20_clean.csv",
                              col_types = cols_only(
                                'Bosque'   = col_character(),
                                'Codigo'   = col_character(),
                                'Plant_id'  = col_character(),
                                'Plant' = col_character(),
                                'quad2' = col_character()),
                              locale = readr::locale(encoding = "latin1")) %>%
  separate(quad2,c("X","Y"),",") %>% distinct(.keep_all = TRUE)

plant_position_info$X <- as.numeric(plant_position_info$X)
plant_position_info$Y <- as.numeric(plant_position_info$Y)


# Sanity checks
plant_position_info %>% filter(X>24)
plant_position_info %>% filter(Y>24)

# Sequences consist in visiting 2 subplots. Each row refers to a sequence
# Since there are no coordinates for subplot cc19, we remove it from the dataset

fitness_data2_aux <- read_csv("data/donana/cuad_donana_20_clean.csv") %>%
  dplyr::select(-quad,-quad2) %>%
  filter(!Planta.2 %in% c("cc19","RO7"))

fitness_data2_aux$Codigo_vuelo <- 1:nrow(fitness_data2_aux)

fitness_data2 <- NULL

for(i in 1:nrow(fitness_data2_aux)){
  
  row_i <- fitness_data2_aux[i,]
  
  sequence_i_plant_1 <- row_i %>% dplyr::slice(rep(row_number(), row_i$numero.visitas.1)) %>%
    dplyr::select(year,Bosque,Codigo,Tanda,Dia,Codigo_vuelo,Pollinator_id,Planta.1) %>%
    mutate(Plant_id = paste0(Codigo,Planta.1)) %>% dplyr::select(-Planta.1)
  
  sequence_i <- sequence_i_plant_1
  
  sequence_i$Codigo_within_sequence <- 1:nrow(sequence_i)
  
  if(row_i$numero.visitas2 > 1){
    
    sequence_i_plant_2 <- row_i %>% dplyr::slice(rep(row_number(), row_i$numero.visitas.1)) %>%
      dplyr::select(year,Bosque,Codigo,Tanda,Dia,Codigo_vuelo,Pollinator_id,Planta.2) %>%
      mutate(Plant_id = paste0(Codigo,Planta.2)) %>% dplyr::select(-Planta.2)
    
    sequence_i <- bind_rows(sequence_i,sequence_i_plant_2)
    
    sequence_i$Codigo_within_sequence <- 1:nrow(sequence_i)
    
  }
  
  fitness_data2 <- bind_rows(fitness_data2,sequence_i)
}



fitness_data3 <- fitness_data2 %>% 
  left_join(plant_position_info, by = c("Bosque", "Codigo", "Plant_id")) %>%
  rename(Year = year, Polinizador = Pollinator_id, Planta = Plant) %>%
  mutate(Periodo_hora = NA, Fecha = lubridate::dmy(Dia)) %>%
  dplyr::select(-Codigo,-Dia)


# Add year info
fitness_data3$Day_ISO <- yday(fitness_data3$Fecha)
fitness_data3$Day <- (fitness_data3$Day_ISO-(min(fitness_data3$Day_ISO)-1))
fitness_data3$Week_ISO <- strftime(fitness_data3$Fecha, format = "%V")
fitness_data3$Week <- ceiling((fitness_data3$Day)/7)

fitness_data3$Week %>% unique() %>% sort()


# Sanity checks
sum(fitness_data3$X>=0)==nrow(fitness_data3)
sum(fitness_data3$Y>=0)==nrow(fitness_data3)
sum(fitness_data3$X<=24)==nrow(fitness_data3)
sum(fitness_data3$Y<=24)==nrow(fitness_data3)

# Number of sites without coordinates
sum(is.na(fitness_data2$X))+sum(is.na(fitness_data2$Y)) # 0

# Review names: Remove dots and spaces

fitness_data3$Planta %>% unique() %>% sort()
fitness_data3$Polinizador %>% unique() %>% sort()


# All entries contain spaces
fitness_data3$Planta[grep(" ",fitness_data3$Planta)] 
fitness_data3$Polinizador[grep(" ",fitness_data3$Polinizador)]

# Labels also contain dots
fitness_data3$Planta[grep("\\.",fitness_data3$Planta)]
fitness_data3$Polinizador[grep("\\.",fitness_data3$Polinizador)]

# Remove spaces from labels
fitness_data3$Planta <- sub(" ", "_", fitness_data3$Planta)
fitness_data3$Polinizador <- sub(" ", "_", fitness_data3$Polinizador)
fitness_data3$Polinizador <- sub(" ", "_", fitness_data3$Polinizador)

# Remove dots from labels
fitness_data3$Planta <- sub("\\.", "", fitness_data3$Planta)
fitness_data3$Polinizador <- sub("\\.", "", fitness_data3$Polinizador)

#Sanity check
# All entries contain spaces
fitness_data3$Planta[grep(" ",fitness_data3$Planta)] 
fitness_data3$Polinizador[grep(" ",fitness_data3$Polinizador)]

# Labels also contain dots
fitness_data3$Planta[grep("\\.",fitness_data3$Planta)]
fitness_data3$Polinizador[grep("\\.",fitness_data3$Polinizador)]

# Recheck labels
fitness_data3$Planta %>% unique() %>% sort()
fitness_data3$Polinizador %>% unique() %>% sort()

# Fix plant names
fitness_data3$Polinizador[grep("abeja",fitness_data3$Polinizador)] <- "Abeja"

# Save data
fitness_data3_final <- fitness_data3 %>%
  select(Year,Day_ISO,Week_ISO,Week,
         Periodo_hora,Bosque,Codigo_vuelo,Codigo_within_sequence,
         Polinizador,Planta,X,Y)

write_csv(fitness_data3_final,"results/donana/foraging_Donana_2020.csv")

####################################################
# DATA FOR 2021

fitness_data21_raw <- read_csv("data/donana/cuad_donana_21_clean.csv") %>%
  mutate(Polinizador = paste0(Pollinator_genus,"_",Pollinator_species)) %>%
  filter(!is.na(Planta_vistada))

colnames(fitness_data21_raw)

fitness_data21_aux <- fitness_data21_raw[,-c(15:51)] %>%
  dplyr::select(-`coord_planta(x)`,-`coord_planta(y)`,
                -`planta(dentro cuadro)`,-`Codigo-Vuelos`,-Orden_vuelo) %>%
  rename(X = `coord_flores(x)`, Y = `coord_flores(y)`, 
         n_visitas = `nº visitas`, Planta = Planta_vistada,
         Codigo_vuelo = `Codigo-vuelo-dia`,
         Periodo_hora = Hora)

# Fix n_visitas = NA

fitness_data21_aux$n_visitas[is.na(fitness_data21_aux$n_visitas)] <- 1


fitness_data21 <- NULL

for(i in 1:nrow(fitness_data21_aux)){
  
  row_i <- fitness_data21_aux[i,]
  sequence_i <- row_i %>% dplyr::slice(rep(row_number(), row_i$n_visitas))
  sequence_i$Codigo_within_sequence <- 1:nrow(sequence_i)
  fitness_data21 <- bind_rows(fitness_data21,sequence_i)
}

# Add year info

fitness_data21$Year <- 2021
fitness_data21$Fecha <- lubridate::dmy(fitness_data21$Fecha)
fitness_data21$Day_ISO <- yday(fitness_data21$Fecha)
fitness_data21$Day <- (fitness_data21$Day_ISO-(min(fitness_data21$Day_ISO)-1))
fitness_data21$Week_ISO <- strftime(fitness_data21$Fecha, format = "%V")
fitness_data21$Week <- ceiling((fitness_data21$Day)/7)


fitness_data21$Week %>% unique() %>% sort()
fitness_data21$Periodo_hora %>% unique() %>% sort()

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

# Fix polinator names
fitness_data21$Polinizador[grep("Andrea_flavipes",fitness_data21$Polinizador)] <- "Andrena_flavipes"
fitness_data21$Polinizador[grep("Anthophora_NA",fitness_data21$Polinizador)] <- "Anthophora_sp"
fitness_data21$Polinizador[grep("Empis_NA",fitness_data21$Polinizador)] <- "Empis_sp"
fitness_data21$Polinizador[grep("Episyrphus_NA",fitness_data21$Polinizador)] <- "Episyrphus_sp"
fitness_data21$Polinizador[grep("Eucera_NA",fitness_data21$Polinizador)] <- "Eucera_sp"
fitness_data21$Polinizador[grep("Hymenoptera_NA",fitness_data21$Polinizador)] <- "Hymenoptera_sp"
fitness_data21$Polinizador[grep("Lasioglossum_NA",fitness_data21$Polinizador)] <- "Lasioglossum_sp"
fitness_data21$Polinizador[grep("Ucera_NA",fitness_data21$Polinizador)] <- "Eucera_sp"

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

# Recheck labels
fitness_data21$Planta %>% unique() %>% sort()
fitness_data21$Polinizador %>% unique() %>% sort()

fitness_data21_final <- fitness_data21 %>%
  select(Year,Day_ISO,Week_ISO,Week,
         Periodo_hora,Bosque,Codigo_vuelo,Codigo_within_sequence,
         Polinizador,Planta,X,Y)

write_csv(fitness_data21_final,"results/donana/foraging_Donana_2021.csv")

