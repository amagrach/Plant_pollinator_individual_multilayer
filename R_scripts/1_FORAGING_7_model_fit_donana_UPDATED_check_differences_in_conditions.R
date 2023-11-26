
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

data_path_file <- paste0("results/donana/total_pollinator_i_data_clogit_observations_",
                         number_random_steps,"_rd_steps_UPDATED_RANDOM_FIDELITY.csv")

total_pollinator_i_data_clogit <- read_csv(data_path_file) %>% filter(control==1) %>%
  dplyr::select(Bosque, Periodo, time_of_day, Codigo_vuelo, Polinizador, change_plant_sp, 
         delta_richness, delta_total_flowers,step_ID,Periodo_hora, Year,step_length)

total_pollinator_i_data_clogit$Periodo <- paste0("Flow. period ", total_pollinator_i_data_clogit$Periodo)

total_pollinator_i_data_clogit$Bosque[total_pollinator_i_data_clogit$Bosque== "Pinar Aznalcazar"] <- "Aznalcazar"
total_pollinator_i_data_clogit$Bosque[total_pollinator_i_data_clogit$Bosque== "Pinar Hinojos"] <- "Hinojos"
total_pollinator_i_data_clogit$Bosque[total_pollinator_i_data_clogit$Bosque== "Pinar Puebla"] <- "Puebla"
total_pollinator_i_data_clogit$Bosque[total_pollinator_i_data_clogit$Bosque== "Pinar Villamanrique Este (Chaparral)"] <- "Villam. Este"
total_pollinator_i_data_clogit$Bosque[total_pollinator_i_data_clogit$Bosque== "Pinar Villamanrique Sur"] <- "Villam. Sur"

total_pollinator_i_data_clogit <- total_pollinator_i_data_clogit %>%
  mutate(condition = paste0(Bosque," (",Periodo,": ",time_of_day,")")) %>%
  mutate(condition = as.factor(condition)) %>%
  mutate(Bosque = as.factor(Bosque)) %>%
  mutate(Periodo = as.factor(Periodo)) %>%
  mutate(time_of_day = as.factor(time_of_day))


total_pollinator_i_data_clogit$condition %>% unique() %>% sort()

total_pollinator_i_data_clogit$Polinizador %>% unique() %>% sort()

pollinator_i = "Anthophora_dispar"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count() # Remove Pinar Villamanrique Este (Chaparral)
pollinator_i = "Apis_mellifera"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count()# Remove Pinar Hinojos?
pollinator_i = "Bombus_terrestris"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count() # Remove Pinar Villamanrique Este (Chaparral)
pollinator_i = "Dasypoda_cingulata"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count()
pollinator_i = "Xylocopa_cantabrita"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Bosque) %>% count() # Remove Pinar Villamanrique Este (Chaparral)?

pollinator_i = "Anthophora_dispar"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()
pollinator_i = "Apis_mellifera"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()
pollinator_i = "Bombus_terrestris"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()# Remove 3?
pollinator_i = "Dasypoda_cingulata"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()
pollinator_i = "Xylocopa_cantabrita"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Periodo) %>% count()

pollinator_i = "Anthophora_dispar"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()
pollinator_i = "Apis_mellifera"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()
pollinator_i = "Bombus_terrestris"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()
pollinator_i = "Dasypoda_cingulata"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()
pollinator_i = "Xylocopa_cantabrita"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(time_of_day) %>% count()

#########################################################################################################
#########################################################################################################

# # A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# # Comparing means  change_plant_sp

# Differences in loyalty are not computable, but almost all of them are not significant
sum(total_pollinator_i_data_clogit$change_plant_sp)
sum(total_pollinator_i_data_clogit$change_plant_sp)/nrow(total_pollinator_i_data_clogit)

total_pollinator_i_data_clogit %>% filter(change_plant_sp != F)
# 
# pollinator_i = "Anthophora_dispar"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 24]
# amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))

pollinator_i = "Apis_mellifera"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos", Periodo!=3) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 400]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i,Bosque!="Pinar Hinojos", Periodo!=3, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Bombus_terrestris"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 400]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Dasypoda_cingulata"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 30]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Xylocopa_cantabrita"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 6]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))


#########################################################################################################
#########################################################################################################

# A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# Comparing means for delta_richness

# RESULTS: NON-SIGNIFICANT MEAN DIFFERENCES
  
pollinator_i = "Anthophora_dispar"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n<1]
amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))
x <- confint(amod_glht)
x[["confint"]]

pollinator_i = "Apis_mellifera"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 91]
amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Bombus_terrestris"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 20]
amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Dasypoda_cingulata"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 20]
amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Xylocopa_cantabrita"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 50]
amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))


#########################################################################################################
#########################################################################################################

# A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# Comparing means for delta_total_flowers:
# Result: we found no differences between conditions for any species.

pollinator_i = "Anthophora_dispar"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 1]
amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Apis_mellifera"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 100]
amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

# pollinator_i = "Bombus_terrestris"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)") %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 18]
# amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)", !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
# 
# pollinator_i = "Dasypoda_cingulata"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 18]
# amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))

# pollinator_i = "Xylocopa_cantabrita"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i,Bosque != "Pinar Villamanrique Este (Chaparral)") %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 15]
# amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)", !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))


#########################################################################################################
#########################################################################################################

# A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# Comparing means for step_length:
# Result: 

pollinator_i = "Anthophora_dispar"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 8]
amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))


pollinator_i = "Apis_mellifera"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 1]
amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!="Pinar Hinojos", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht))

results_step_length_Apis <- tibble(comparisson = names(coefficients),
                           coefficients = as.numeric(coefficients),
                           sigma = as.numeric(sigma),
                           species = "Apis mellifera")



ggplot(total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, 
                                                 condition %in% c("Pinar Villamanrique Sur 2 12:00 - 13:59",
                                                                  "Pinar Puebla 1 10:00 - 11:59",
                                                                  "Pinar Aznalcazar 1 10:00 - 11:59",
                                                                  "Pinar Villamanrique Este (Chaparral) 2 14:00 - 16:05",
                                                                  "Pinar Villamanrique Sur 1 10:00 - 11:59")),
       aes(x=condition,y=step_length))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(~Bosque)

pollinator_i = "Bombus_terrestris"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 10]
amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht))


results_step_length_Bombus <- tibble(comparisson = names(coefficients),
                       coefficients = as.numeric(coefficients),
                       sigma = as.numeric(sigma),
                       species = "Bombus terrestris")


ggplot(total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, 
                                                 condition %in% c("Pinar Aznalcazar 3 10:00 - 11:59",
                                                                  "Pinar Aznalcazar 1 10:00 - 11:59",
                                                                  "Pinar Aznalcazar 1 12:00 - 13:59",
                                                                  "Pinar Aznalcazar 1 14:00 - 16:05",
                                                                  "Pinar Villamanrique Sur 1 12:00 - 13:59",
                                                                  "Pinar Aznalcazar 2 10:00 - 11:59",
                                                                  "Pinar Aznalcazar 2 14:00 - 16:05",
                                                                  "Pinar Villamanrique Sur 2 10:00 - 11:59")),
       aes(x=condition,y=step_length))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(~Bosque)

pollinator_i = "Dasypoda_cingulata"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 20]
amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht)) # No differences

results_step_length_Dasypoda <- tibble(comparisson = names(coefficients),
                         coefficients = as.numeric(coefficients),
                         sigma = as.numeric(sigma),
                         species = "Dasypoda cingulata")


ggplot(total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, 
                                                 condition %in% c("Pinar Villamanrique Sur 3 12:00 - 13:59",
                                                                  "Pinar Hinojos 2 14:00 - 16:05",
                                                                  "Pinar Villamanrique Este (Chaparral) 2 12:00 - 13:59")),
       aes(x=condition,y=step_length))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(~Bosque)

pollinator_i = "Xylocopa_cantabrita"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)") %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 1]
amod <- aov(step_length ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque != "Pinar Villamanrique Este (Chaparral)", !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht))

results_step_length_Xylocopa <- tibble(comparisson = names(coefficients),
                                       coefficients = as.numeric(coefficients),
                                       sigma = as.numeric(sigma),
                                       species = "Xylocopa cantabrita")


ggplot(total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, 
                                                 condition %in% c("Pinar Aznalcazar 3 12:00 - 13:59", # Changes in  conditions labelling affect this plot
                                                                  "Pinar Aznalcazar 3 10:00 - 11:59",
                                                                  "Pinar Puebla 2 12:00 - 13:59",
                                                                  "Pinar Puebla 1 10:00 - 11:59",
                                                                  "Pinar Puebla 1 14:00 - 16:05")),
       aes(x=condition,y=step_length))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(~Bosque)




results_step_length_df <- bind_rows(results_step_length_Apis, results_step_length_Bombus, results_step_length_Dasypoda, results_step_length_Xylocopa)
# write_csv(results_step_length_df,"results/donana/significant_differences_main_pollinator_conditions.csv") # Commented for security reasons

results_step_length_df <- read_csv("results/donana/significant_differences_main_pollinator_conditions.csv")
results_step_length_df$species[results_step_length_df$species=="Dasypodacingulata"] <- "Dasypoda cingulata"
results_step_length_df$comparisson %>% unique()


png("figures/donana_conditions_diferences_1.png",
    width = 11.69*.9, # The width of the plot in inches
    height = 11.69*0.7*600/450, units = "in", res=300*2)

ggplot(results_step_length_df %>% filter(species %in% c("Apis mellifera", "Bombus terrestris")),aes(y = as.factor(comparisson)))+
  geom_point(aes(x=coefficients,size=2))+
  geom_errorbar(aes(xmin=coefficients-1.96*sigma, xmax=coefficients+1.96*sigma), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(~species, ncol = 1, scales = "free_y")+
  theme_bw()+
  guides(size = "none")+
  labs(title="Doñana (Step length)", x=NULL, y = NULL)+
  theme(axis.text=element_text(size=14),  plot.title=element_text(size=19))+
  theme(strip.text = element_text(size=15,face = "italic"))

dev.off()

png("figures/donana_conditions_diferences_2.png",
    width = 11.69*.9, # The width of the plot in inches
    height = 11.69*0.6*0.7*600/450, units = "in", res=300*2)

ggplot(results_step_length_df %>% filter(!species %in% c("Apis mellifera", "Bombus terrestris")),aes(y = as.factor(comparisson)))+
  geom_point(aes(x=coefficients,size=2))+
  geom_errorbar(aes(xmin=coefficients-1.96*sigma, xmax=coefficients+1.96*sigma), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(~species, ncol = 1, scales = "free_y")+
  theme_bw()+
  guides(size = "none")+
  labs(title="Doñana (Step length)", x=NULL, y = NULL)+
  theme(axis.text=element_text(size=14),  plot.title=element_text(size=19))+
  theme(strip.text = element_text(size=15,face = "italic"))

dev.off()

