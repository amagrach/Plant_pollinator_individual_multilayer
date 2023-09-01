
# This code estimates if there are significant differences between some
# step features of a given pollinator (change of plant species, change of plant 
# richness or change in the total number of flowers, or step length), when considering
# the location, period of sampling and time of day.

library(tidyverse)
library(sandwich)
library(multcomp)


# source("R_scripts/aux_functions/pollinator_model_exp_coef.R")
# source("R_scripts/aux_functions/set_same_factor_levels_on_flower_data.R")

number_random_steps <- 20

data_path_file <- paste0("results/gorbea/total_pollinator_i_data_clogit_observations_",
                         number_random_steps,"_rd_steps_UPDATED.csv")

total_pollinator_i_data_clogit <- read_csv(data_path_file) %>% filter(control==1) %>%
  dplyr::select(Bosque, Periodo, time_of_day, Codigo_vuelo, Polinizador, change_plant_sp, 
         delta_richness, delta_total_flowers,step_ID,Periodo_hora, plot,Year,step_length) %>%
  mutate(condition = paste(Bosque, Periodo, time_of_day)) %>%
  mutate(condition = as.factor(condition))%>%
  mutate(Bosque = as.factor(Bosque))%>%
  mutate(Periodo = as.factor(Periodo))%>%
  mutate(time_of_day = as.factor(time_of_day))

pollinator_i = "Bombus_pascuorum"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count()
pollinator_i = "Apis_mellifera"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count()# Remove 5?
pollinator_i = "Sphaerophoria_scripta"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count()
pollinator_i = "Bombus_lapidarius"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count() # Remove 3?
pollinator_i = "Eristalis_sp"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count() # Remove 5?

pollinator_i = "Bombus_pascuorum"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()
pollinator_i = "Apis_mellifera"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()# Remove 3?
pollinator_i = "Sphaerophoria_scripta"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()
pollinator_i = "Bombus_lapidarius"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()
pollinator_i = "Eristalis_sp"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()

pollinator_i = "Bombus_pascuorum"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()
pollinator_i = "Apis_mellifera"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()
pollinator_i = "Sphaerophoria_scripta"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()
pollinator_i = "Bombus_lapidarius"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()
pollinator_i = "Eristalis_sp"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()# Remove 15:20 - 18:05?

#########################################################################################################
#########################################################################################################

# # A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# # Comparing means  change_plant_sp
# 
# pollinator_i = "Bombus_pascuorum"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 24]
# amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 
# pollinator_i = "Apis_mellifera"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, Periodo!=3) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 50]
# amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i,Bosque!=5, Periodo!=3, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 
# pollinator_i = "Sphaerophoria_scripta"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 20]
# amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 
# pollinator_i = "Bombus_lapidarius"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 10]
# amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 
# pollinator_i = "Eristalis_sp"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 6]
# amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 

#########################################################################################################
#########################################################################################################

# A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# Comparing means for delta_richness

# RESULTS: NON-SIGNIFICANT MEAN DIFFERENCES
  
# pollinator_i = "Bombus_pascuorum"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 15]
# amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))

pollinator_i = "Apis_mellifera"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 4]
amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

# pollinator_i = "Sphaerophoria_scripta"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 14]
# amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 
# pollinator_i = "Bombus_lapidarius"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 4]
# amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))

pollinator_i = "Eristalis_sp"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 6]
amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))


#########################################################################################################
#########################################################################################################

# A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# Comparing means for delta_total_flowers:
# Result: we found no differences between conditions for any species.

pollinator_i = "Bombus_pascuorum"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 8]
amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Apis_mellifera"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 4]
amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Sphaerophoria_scripta"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 8]
amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

# pollinator_i = "Bombus_lapidarius"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 4]
# amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))

pollinator_i = "Eristalis_sp"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 6]
amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))


#########################################################################################################
#########################################################################################################

# A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# Comparing means for step_length:
# Result: 

pollinator_i = "Bombus_pascuorum"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 8]
amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

ggplot(total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, 
                                                 condition %in% c("3 3 12:40 - 15:19",
                                                                  "4 3 12:40 - 15:19",
                                                                  "2 1 12:40 - 15:19",
                                                                  "2 1 15:20 - 18:05",
                                                                  "2 1 10:00 - 12:39",
                                                                  "1 2 12:40 - 15:19",
                                                                  "1 2 15:20 - 18:05")),
       aes(x=condition,y=step_length))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(~Bosque)

# pollinator_i = "Apis_mellifera"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 4]
# amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 
# pollinator_i = "Sphaerophoria_scripta"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 8]
# amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 
# pollinator_i = "Bombus_lapidarius"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 4]
# amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht)) # No differences
# 
# pollinator_i = "Eristalis_sp"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 2]
# amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
