
library(tidyverse)

flights_donana <- read_csv("results/donana/foraging_Donana_2021.csv")

flights_gorbea_21 <- read_csv("results/gorbea/foraging_Gorbea_2021.csv")
flights_gorbea_20 <- read_csv("results/gorbea/foraging_Gorbea_2020.csv")

flights_gorbea <- bind_rows(flights_gorbea_20,flights_gorbea_21)

most_abundant_donana <- flights_donana %>% dplyr::select(Polinizador) %>% 
  group_by(Polinizador) %>% count() %>% arrange(desc(n))

most_abundant_gorbea <- flights_gorbea %>% dplyr::select(Polinizador) %>% 
  group_by(Polinizador) %>% count() %>% arrange(desc(n))


most_abundant_pollinators_donana <- c("Apis_mellifera","Bombus_terrestris",
                                      "Xylocopa_cantabrita","Dasypoda_cingulata",
                                      "Anthophora_dispar")

most_abundant_pollinators_gorbea <- c("Bombus_pascuorum","Apis_mellifera",
                                      "Sphaerophoria_scripta","Bombus_lapidarius",
                                      "Eristalis_sp")


flights_gorbea_most_abundant_pol <- flights_gorbea %>% 
  filter(Polinizador %in% most_abundant_pollinators_gorbea)

flights_donana_most_abundant_pol <- flights_donana %>% 
  filter(Polinizador %in% most_abundant_pollinators_donana)


number_sequences_gorbea <- flights_gorbea_most_abundant_pol %>% 
  dplyr::select(Codigo_vuelo) %>%
  unique() %>% nrow()

number_sequences_donana <- flights_donana_most_abundant_pol %>% 
  dplyr::select(Codigo_vuelo) %>%
  unique() %>% nrow()

steps_donana <- read_csv("results/donana/observed_steps_21.csv") %>%
  filter(Polinizador %in% most_abundant_pollinators_donana)
steps_gorbea <- read_csv("results/gorbea/observed_steps_20_21.csv") %>%
  filter(Polinizador %in% most_abundant_pollinators_gorbea)

number_steps_per_seq_donana <- steps_donana %>% 
  dplyr::select(Codigo_vuelo,node1) %>%
  group_by(Codigo_vuelo) %>% count()

mean(number_steps_per_seq_donana$n)
sd(number_steps_per_seq_donana$n)

number_steps_per_seq_gorbea <- steps_gorbea %>% 
  dplyr::select(Codigo_vuelo,node1) %>%
  group_by(Codigo_vuelo) %>% count()

mean(number_steps_per_seq_gorbea$n)
sd(number_steps_per_seq_gorbea$n)
