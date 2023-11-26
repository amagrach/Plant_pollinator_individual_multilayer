
potential_steps_to_subplot <- function(X, Y, Plot, Plant_sp, Week_ISO_i, 
                                       flora_census){
  
  
  richness_censuses <- flora_census %>% dplyr::select(X, Y, Planta, Year, Bosque, Periodo) %>%
    unique() %>% group_by(X, Y, Year, Bosque, Periodo) %>% count() %>% rename(richness = n)
  
  total_flowers_censuses <- flora_census %>% dplyr::select(X, Y, Flores, Year, Bosque, Periodo) %>%
    group_by(X, Y, Year, Bosque, Periodo) %>% count(wt = Flores) %>% rename(total_number_flowers = n) %>%
    filter(total_number_flowers > 0)
  
  potential_steps <- flora_census %>% 
    filter(Plot == Bosque, Planta == Plant_sp, Week_ISO == Week_ISO_i) %>%
    rename(X1 = X, Y1 = Y) %>% mutate(X2 = X, Y2 = Y) %>% 
    left_join(richness_censuses %>% rename(X1 = X, Y1 = Y, richness1 = richness),
              by = c("X1", "Y1", "Year", "Bosque", "Periodo")) %>%
    left_join(richness_censuses %>% rename(X2 = X, Y2 = Y, richness2 = richness),
              by = c("X2", "Y2", "Year", "Bosque", "Periodo")) %>%
    left_join(total_flowers_censuses %>% rename(X1 = X, Y1 = Y, total_number_flowers1 = total_number_flowers),
              by = c("X1", "Y1", "Year", "Bosque", "Periodo")) %>%
    left_join(total_flowers_censuses %>% rename(X2 = X, Y2 = Y, total_number_flowers2 = total_number_flowers),
              by = c("X2", "Y2", "Year", "Bosque", "Periodo")) %>%
    mutate(delta_richness = richness2- richness1,
           delta_total_flowers = total_number_flowers2-total_number_flowers1,
           step_length  = sqrt((X1-X2)^2+(Y1-Y2)^2))
  
  return(potential_steps)
  
}


potential_steps_to_subplot_period <- function(X, Y, Plot, Plant_sp, Year, 
                                       flora_census, period_i){
  
  
  richness_censuses <- flora_census %>% dplyr::select(X, Y, Planta, Year, Bosque, Periodo) %>%
    unique() %>% group_by(X, Y, Year, Bosque, Periodo) %>% count() %>% rename(richness = n)
  
  total_flowers_censuses <- flora_census %>% dplyr::select(X, Y, Flores, Year, Bosque, Periodo) %>%
    group_by(X, Y, Year, Bosque, Periodo) %>% count(wt = Flores) %>% rename(total_number_flowers = n) %>%
    filter(total_number_flowers > 0)
  
  potential_steps <- flora_census %>% 
    filter(Plot == Bosque, Planta == Plant_sp, Periodo == period_i) %>%
    rename(X1 = X, Y1 = Y) %>% mutate(X2 = X, Y2 = Y) %>% 
    left_join(richness_censuses %>% rename(X1 = X, Y1 = Y, richness1 = richness),
              by = c("X1", "Y1", "Year", "Bosque", "Periodo")) %>%
    left_join(richness_censuses %>% rename(X2 = X, Y2 = Y, richness2 = richness),
              by = c("X2", "Y2", "Year", "Bosque", "Periodo")) %>%
    left_join(total_flowers_censuses %>% rename(X1 = X, Y1 = Y, total_number_flowers1 = total_number_flowers),
              by = c("X1", "Y1", "Year", "Bosque", "Periodo")) %>%
    left_join(total_flowers_censuses %>% rename(X2 = X, Y2 = Y, total_number_flowers2 = total_number_flowers),
              by = c("X2", "Y2", "Year", "Bosque", "Periodo")) %>%
    mutate(delta_richness = richness2- richness1,
           delta_total_flowers = total_number_flowers2-total_number_flowers1,
           step_length  = sqrt((X1-X2)^2+(Y1-Y2)^2))
  
  return(potential_steps)
  
}
