
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
                         number_random_steps,"_rd_steps_UPDATED_RANDOM_FIDELITY.csv")

total_pollinator_i_data_clogit <- read_csv(data_path_file) %>% filter(control==1) %>%
  dplyr::select(Bosque, Periodo, time_of_day, Codigo_vuelo, Polinizador, change_plant_sp, 
                delta_richness, delta_total_flowers,step_ID,Periodo_hora, Year,step_length)

total_pollinator_i_data_clogit$Periodo <- paste0("Flow. period ", total_pollinator_i_data_clogit$Periodo)
total_pollinator_i_data_clogit$Bosque <- paste0("Site ", total_pollinator_i_data_clogit$Bosque)

total_pollinator_i_data_clogit <- total_pollinator_i_data_clogit %>%
  mutate(condition = paste0(Bosque," (",Periodo,": ",time_of_day,", ",Year,")")) %>%
  mutate(condition = as.factor(condition)) %>%
  mutate(Bosque = as.factor(Bosque)) %>%
  mutate(Periodo = as.factor(Periodo)) %>%
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

pollinator_i = "Bombus_pascuorum"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Year) %>% count()
pollinator_i = "Apis_mellifera"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Year) %>% count()
pollinator_i = "Sphaerophoria_scripta"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Year) %>% count()
pollinator_i = "Bombus_lapidarius"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Year) %>% count()
pollinator_i = "Eristalis_sp"
total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(Year) %>% count()# Remove 15:20 - 18:05?

#########################################################################################################
#########################################################################################################

# # A Robust Procedure for Comparing Multiple Means under Heteroscedasticity in Unbalanced Designs---------
# # Comparing means  change_plant_sp
# 
pollinator_i = "Bombus_pascuorum" #There are differences
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 50]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht))

results_fidelity_Pascuorum <- tibble(comparisson = names(coefficients),
                                        coefficients = as.numeric(coefficients),
                                        sigma = as.numeric(sigma),
                                        species = gsub("_"," ",pollinator_i))



# png("figures/gorbeia_conditions_diferences_pascuorum_loyal.png",
#     width = 11.69*.9, # The width of the plot in inches
#     height = 11.69*.5*600/450, units = "in", res=300*2)
# 
# ggplot(results_step_length_Pascuorum,aes(y = as.factor(comparisson)))+
#   geom_point(aes(x=coefficients),size=2)+
#   geom_errorbar(aes(xmin=coefficients-1.96*sigma, xmax=coefficients+1.96*sigma), width=.2)+
#   geom_vline(xintercept = 0,linetype = "dashed")+
#   facet_wrap(~species, ncol = 1, scales = "free_y")+
#   theme_bw()+
#   guides(size = "none")+
#   labs(title="Meadows (Gorbeia N.P.)", x=NULL, y = NULL)+
#   theme(axis.text=element_text(size=14),  plot.title=element_text(size=19))+
#   theme(strip.text = element_text(size=15,face = "italic"))
# 
# dev.off()




pollinator_i = "Apis_mellifera"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, Periodo!=3) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 50]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i,Bosque!=5, Periodo!=3, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary(amod_glht)
confint(amod_glht)
plot(confint(amod_glht))

pollinator_i = "Sphaerophoria_scripta" #There are differences
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 20]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
summary_report <- summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht))

results_fidelity_Scripta <- tibble(comparisson = names(coefficients),
                                         coefficients = as.numeric(coefficients),
                                         sigma = as.numeric(sigma),
                                         species = gsub("_"," ",pollinator_i))



# png("figures/gorbeia_conditions_diferences_scripta_loyal.png",
#     width = 11.69*.9, # The width of the plot in inches
#     height = 11.69*.5*600/450, units = "in", res=300*2)
# 
# ggplot(results_fidelity_Scripta,aes(y = as.factor(comparisson)))+
#   geom_point(aes(x=coefficients),size=2)+
#   geom_errorbar(aes(xmin=coefficients-1.96*sigma, xmax=coefficients+1.96*sigma), width=.2)+
#   geom_vline(xintercept = 0,linetype = "dashed")+
#   facet_wrap(~species, ncol = 1, scales = "free_y")+
#   theme_bw()+
#   guides(size = "none")+
#   labs(title="Meadows (Gorbeia N.P.)", x=NULL, y = NULL)+
#   theme(axis.text=element_text(size=14),  plot.title=element_text(size=19))+
#   theme(strip.text = element_text(size=15,face = "italic"))
# 
# dev.off()



pollinator_i = "Bombus_lapidarius" #There are differences
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 30]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=3, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)


pollinator_i = "Eristalis_sp" #There are differences
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 6]
amod <- aov(change_plant_sp ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht))

results_fidelity_Eristalis <- tibble(comparisson = names(coefficients),
                                          coefficients = as.numeric(coefficients),
                                          sigma = as.numeric(sigma),
                                          species = gsub("_"," ",pollinator_i))



# png("figures/gorbeia_conditions_diferences_Eristalis_loyal.png",
#     width = 11.69*.9, # The width of the plot in inches
#     height = 11.69*.5*600/450, units = "in", res=300*2)
# 
# ggplot(results_fidelity_Eristalis,aes(y = as.factor(comparisson)))+
#   geom_point(aes(x=coefficients),size=2)+
#   geom_errorbar(aes(xmin=coefficients-1.96*sigma, xmax=coefficients+1.96*sigma), width=.2)+
#   geom_vline(xintercept = 0,linetype = "dashed")+
#   facet_wrap(~species, ncol = 1, scales = "free_y")+
#   theme_bw()+
#   guides(size = "none")+
#   labs(title="Meadows (Gorbeia N.P.)", x=NULL, y = NULL)+
#   theme(axis.text=element_text(size=14),  plot.title=element_text(size=19))+
#   theme(strip.text = element_text(size=15,face = "italic"))
# 
# dev.off()

results_fidelity_df <- bind_rows(results_fidelity_Pascuorum,
                                 results_fidelity_Scripta,
                                 results_fidelity_Eristalis)
# write_csv(results_fidelity_df,"results/gorbea/significant_fidelity_differences_main_pollinator_conditions.csv") # Commented for security reasons

results_fidelity_df <- read_csv("results/gorbea/significant_fidelity_differences_main_pollinator_conditions.csv")
results_fidelity_df$comparisson %>% unique()


png("figures/gorbea_conditions_fidelity_differences_1.png",
    width = 11.69*.9, # The width of the plot in inches
    height = 11.69*0.7*600/450, units = "in", res=300*2)

ggplot(results_fidelity_df,aes(y = as.factor(comparisson)))+
  geom_point(aes(x=coefficients,size=2))+
  geom_errorbar(aes(xmin=coefficients-1.96*sigma, xmax=coefficients+1.96*sigma), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(~species, ncol = 1, scales = "free_y")+
  theme_bw()+
  guides(size = "none")+
  labs(title="Gorbeia (Floral fidelity)", x=NULL, y = NULL)+
  theme(axis.text=element_text(size=14),  plot.title=element_text(size=19))+
  theme(strip.text = element_text(size=15,face = "italic"))

dev.off()




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
removed_conditions <- observations_condition$condition[observations_condition$n < 12]
amod <- aov(delta_richness ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]

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
summary_report <- summary(amod_glht)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]


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
summary_report <- summary(amod_glht)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05] # No significant results

pollinator_i = "Apis_mellifera"
observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5) %>% group_by(condition) %>% count() %>% arrange(n)
removed_conditions <- observations_condition$condition[observations_condition$n < 10]
amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, Bosque!=5, !condition %in% removed_conditions ))
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05] # No significant results

# pollinator_i = "Sphaerophoria_scripta"
# observations_condition <- total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i) %>% group_by(condition) %>% count() %>% arrange(n)
# removed_conditions <- observations_condition$condition[observations_condition$n < 8]
# amod <- aov(delta_total_flowers ~ condition, total_pollinator_i_data_clogit %>% filter(Polinizador == pollinator_i, !condition %in% removed_conditions ))
# amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
# summary_report <- summary(amod_glht)
# # summary(amod_glht)
# # confint(amod_glht)
# # plot(confint(amod_glht))
# pvalues <- summary_report[["test"]][["pvalues"]]
# pvalues[pvalues<0.05]

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
summary_report <- summary(amod_glht)
# summary(amod_glht)
# confint(amod_glht)
# plot(confint(amod_glht))
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05] # No significant results


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
summary_report <- summary(amod_glht)
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht))

results_step_length_Pascuorum <- tibble(comparisson = names(coefficients),
                                       coefficients = as.numeric(coefficients),
                                       sigma = as.numeric(sigma),
                                       species = gsub("_"," ",pollinator_i))



png("figures/gorbeia_conditions_diferences.png",
    width = 11.69*.9, # The width of the plot in inches
    height = 11.69*.5*600/450, units = "in", res=300*2)

ggplot(results_step_length_Pascuorum,aes(y = as.factor(comparisson)))+
  geom_point(aes(x=coefficients),size=2)+
  geom_errorbar(aes(xmin=coefficients-1.96*sigma, xmax=coefficients+1.96*sigma), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(~species, ncol = 1, scales = "free_y")+
  theme_bw()+
  guides(size = "none")+
  labs(title="Gorbeia (Step length)", x=NULL, y = NULL)+
  theme(axis.text=element_text(size=14),  plot.title=element_text(size=19))+
  theme(strip.text = element_text(size=15,face = "italic"))

dev.off()


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
