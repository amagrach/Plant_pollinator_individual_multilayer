

# -------------------------------------------------------------------------

library(tidyverse)
library(MASS)
library(fitdistrplus)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(bbmle) ## for AICtab
library(performance)
library(visreg)
library(RColorBrewer)
library(nlme)
library(usdm)
library(see)
library(DHARMa)
library(fitdistrplus)
library(logspline)

#-------------------------------------------------------------------------
# Load data for models
number_random_steps <- 20
path_save_file <- paste0("results/donana/data for_fruitset_models_from_",
                         number_random_steps,"_rd_steps.csv")

# library(EnvStats)
# test <- EnvStats::rosnerTest(data_model$fruitset,
#                    k = 25
# )
# test
# https://statsandr.com/blog/outliers-detection-in-r/
# Rosner test suggest that fruitset < 0.4 are outliers


data_model <- read_csv(path_save_file) %>%
  mutate(prop_homo = homo_motif/(homo_motif+hete_motif)) %>%
  filter(!is.na(prop_homo),!is.na(prob_consp_step)) %>%
  mutate(Periodo = as.factor(Periodo),
         Planta = as.factor(Planta),
         Bosque = as.factor(Bosque),
         prop_homo = homo_motif/(homo_motif+hete_motif))


library(GGally)
# Pairwise visualization
ggpairs(data_model %>% dplyr::select("fruitset","homo_motif",
                                     "hete_motif","prop_homo",
                                             "prob_consp_step"), 
        title="correlogram") 

ggpairs(data_model %>% mutate(log_fruitset = log(fruitset)) %>% 
          dplyr::select("log_fruitset","homo_motif","hete_motif","prop_homo",
                                     "prob_consp_step"), 
        title="correlogram") 

# Plot response variable
ggplot(data_model)+
  geom_density(aes(x=fruitset))

ggplot(data_model)+
  geom_histogram(aes(x=fruitset))

ggplot(data_model)+
  geom_boxplot(aes(x=fruitset))

ggplot(data_model)+
  geom_boxplot(aes(x=fruitset))+
  facet_wrap(~Planta)

# library(EnvStats)
# test_cistus_crispus <- EnvStats::rosnerTest(data_model$fruitset[data_model$Planta=="Cistus_crispus"],
#                    k = 2
# )
# test_cistus_crispus
# 
# test_cistus_salviifolius <- EnvStats::rosnerTest(data_model$fruitset[data_model$Planta=="Cistus_salviifolius"],
#                                             k = 3
# )
# test_cistus_salviifolius # Remove 3-8 Cistus_salviifolius 6-2 Cistus_salviifolius

data_model_filtered <- data_model %>%
  filter(!row_number() %in% c(99, 62))

data_model_beta_full <- data_model_filtered
data_model_beta_full$fruitset[data_model_beta_full$fruitset==1] <- 1-1e-3
data_model_beta_full$fruitset[data_model_beta_full$fruitset==0] <- 1e-3

data_model_beta <- data_model_beta_full %>%
  filter(Planta != "Cistus_salviifolius")# White test suggest this species highly increase heterokedasticity

data_model_beta_salvii <- data_model_beta_full %>%
  filter(Planta == "Cistus_salviifolius")

# Pairwise visualization
ggpairs(data_model %>% dplyr::select("fruitset","homo_motif",
                                     "hete_motif","prop_homo",
                                     "prob_consp_step"), 
        title="correlogram") 

ggpairs(data_model %>% mutate(log_fruitset = log(fruitset)) %>% 
          dplyr::select("log_fruitset","homo_motif","hete_motif","prop_homo",
                        "prob_consp_step"), 
        title="correlogram") 

# fit a beta distr. to our fitness data
fitdistrplus::descdist(data_model$fruitset, discrete = FALSE)


# MODELS WITHOUT RANDOM EFFECTS---------------------------
only_visits_fruitset_GLM_beta <-  betareg::betareg((fruitset) ~ scale(Visits_tot), 
                                                   data_model_beta)
summary(only_visits_fruitset_GLM_beta)
#perform Breusch-Pagan Test
lmtest::bptest(only_visits_fruitset_GLM_beta)# OK

only_visits_fruitset_GLM_beta_periodo <-  betareg::betareg((fruitset) ~ scale(Visits_tot)+Periodo, 
                                                   data_model_beta)
summary(only_visits_fruitset_GLM_beta_periodo)
#perform Breusch-Pagan Test
lmtest::bptest(only_visits_fruitset_GLM_beta_periodo)# OK

only_visits_fruitset_GLM_beta_planta <-  betareg::betareg((fruitset) ~ scale(Visits_tot)+Planta, 
                                                           data_model_beta)
summary(only_visits_fruitset_GLM_beta_planta)
#perform Breusch-Pagan Test
lmtest::bptest(only_visits_fruitset_GLM_beta_planta)# FAILED

only_visits_fruitset_GLM_beta_bosque <-  betareg::betareg((fruitset) ~ scale(Visits_tot)+Bosque, 
                                                          data_model_beta)
summary(only_visits_fruitset_GLM_beta_bosque)
#perform Breusch-Pagan Test
lmtest::bptest(only_visits_fruitset_GLM_beta_bosque)# OK

only_visits_fruitset_GLM_beta_periodo_planta <-  betareg::betareg((fruitset) ~ scale(Visits_tot)+Periodo+Planta, 
                                                          data_model_beta)
summary(only_visits_fruitset_GLM_beta_periodo_planta)
#perform Breusch-Pagan Test
lmtest::bptest(only_visits_fruitset_GLM_beta_periodo_planta)# OK


alternative_variables_fruitset_GLM_beta_periodo_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                       scale(prop_homo) + 
                                                                       scale(prob_consp_step) + 
                                                                       Periodo, 
                                                                     data_model_beta)
summary(alternative_variables_fruitset_GLM_beta_periodo_quan) # prob_consp is significant
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_quan)# FAILED (p-value = 0.03066)


# Let us try a weighted fit
data_model_beta$resi <- residuals(alternative_variables_fruitset_GLM_beta_periodo_quan)

ggplot(data_model_beta, aes(y = resi, x = scale(Visits_tot), color = Planta)) + geom_point() + geom_abline(slope = 0)
ggplot(data_model_beta, aes(y = resi, x = scale(homo_motif), color = Planta)) + geom_point() + geom_abline(slope = 0)
ggplot(data_model_beta, aes(y = resi, x = scale(prob_consp_step), color = Planta)) + geom_point() + geom_abline(slope = 0)
ggplot(data_model_beta, aes(y = resi, x = scale(hete_motif), color = Planta)) + geom_point() + geom_abline(slope = 0)

var_alternative_variables_fruitset_GLM_beta_periodo_quan <- lm(log(resi^2) ~ scale(Visits_tot) + 
                                                                 scale(prop_homo) + 
                                                                 scale(prob_consp_step) + 
                                                                 Periodo,
                                                            data = data_model_beta)
data_model_beta$varfunc <- exp(var_alternative_variables_fruitset_GLM_beta_periodo_quan$fitted.values)
alternative_variables_fruitset_GLM_beta_periodo_quan_weighted <- betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                        scale(prop_homo) + 
                                                                                        scale(prob_consp_step) + 
                                                                                        Periodo,
                                                                                weights = 1/sqrt(varfunc), data_model_beta)

summary(alternative_variables_fruitset_GLM_beta_periodo_quan_weighted)
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_quan_weighted) # FAILED

# Lets explore where the heterokedasticity is
# Square the residuals
data_model_beta$residuos_cuadrados <- data_model_beta$resi^2
# Fit an auxiliary model with the square residuals
modelo_aux <- lm(residuos_cuadrados ~ scale(Visits_tot) + 
                   scale(prop_homo) + 
                   scale(prob_consp_step) + 
                   Periodo, 
                 data_model_beta)
# White test
white_test <- lmtest::coeftest(modelo_aux, vcov = sandwich::sandwich(modelo_aux))
print(white_test)




alternative_variables_fruitset_GLM_beta_planta_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                      scale(prop_homo) + 
                                                                      scale(prob_consp_step) + 
                                                                      Planta, 
                                                                    data_model_beta)
summary(alternative_variables_fruitset_GLM_beta_planta_quan)  # prop_homo is almost significant
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_beta_planta_quan)# FAILED (p-value = 0.01578)

# Let us try a weighted fit
data_model_beta$resi <- residuals(alternative_variables_fruitset_GLM_beta_planta_quan)
var_alternative_variables_fruitset_GLM_beta_planta_quan <- lm(log(resi^2) ~ scale(Visits_tot) + 
                                                                scale(prop_homo) + 
                                                                scale(prob_consp_step) + 
                                                                Planta,
                                                               data = data_model_beta)
data_model_beta$varfunc <- exp(var_alternative_variables_fruitset_GLM_beta_planta_quan$fitted.values)
alternative_variables_fruitset_GLM_beta_planta_quan_weighted <- betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                   scale(prop_homo) + 
                                                                                   scale(prob_consp_step) + 
                                                                                   Planta,
                                                                                  weights = 1/sqrt(varfunc), data_model_beta)

summary(alternative_variables_fruitset_GLM_beta_planta_quan_weighted)
lmtest::bptest(alternative_variables_fruitset_GLM_beta_planta_quan_weighted) # FAILED

# Lets explore where the heterokedasticity is
# Square the residuals
data_model_beta$residuos_cuadrados <- data_model_beta$resi^2
# Fit an auxiliary model with the square residuals
modelo_aux <- lm(residuos_cuadrados ~ scale(Visits_tot) + 
                   scale(prop_homo) + 
                   scale(prob_consp_step) + 
                   Planta, 
                 data_model_beta)
# White test
white_test <- lmtest::coeftest(modelo_aux, vcov = sandwich::sandwich(modelo_aux))
print(white_test)


alternative_variables_fruitset_GLM_beta_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                              scale(prop_homo) + 
                                                                              scale(prob_consp_step) + 
                                                                              Periodo+Planta, 
                                                                            data_model_beta)
summary(alternative_variables_fruitset_GLM_beta_periodo_planta_quan) # prop_homo is almost significant
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_planta_quan)# FAILED (p-value = 0.02715)

# Let us try a weighted fit
data_model_beta$resi <- residuals(alternative_variables_fruitset_GLM_beta_periodo_planta_quan)
var_alternative_variables_fruitset_GLM_beta_periodo_planta_quan <- lm(log(resi^2) ~ scale(Visits_tot) + 
                                                                scale(prop_homo) + 
                                                                scale(prob_consp_step) + 
                                                                Periodo+Planta,
                                                              data = data_model_beta)
data_model_beta$varfunc <- exp(var_alternative_variables_fruitset_GLM_beta_periodo_planta_quan$fitted.values)
alternative_variables_fruitset_GLM_beta_periodo_planta_quan_weighted <- betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                   scale(prop_homo) + 
                                                                                   scale(prob_consp_step) + 
                                                                                   Periodo+Planta,
                                                                                 weights = 1/sqrt(varfunc), data_model_beta)

summary(alternative_variables_fruitset_GLM_beta_periodo_planta_quan_weighted)
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_planta_quan_weighted) # FAILED

# Lets explore where the heterokedasticity is
# Square the residuals
data_model_beta$residuos_cuadrados <- data_model_beta$resi^2
# Fit an auxiliary model with the square residuals
modelo_aux <- lm(residuos_cuadrados ~ scale(Visits_tot) + 
                   scale(prop_homo) + 
                   scale(prob_consp_step) + 
                   Periodo+Planta, 
                 data_model_beta)
# White test
white_test <- lmtest::coeftest(modelo_aux, vcov = sandwich::sandwich(modelo_aux))
print(white_test)

alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                        scale(homo_motif) + scale(prob_consp_step) + 
                                                                                        scale(hete_motif) + Periodo + Planta, data_model_beta)
summary(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan) # OK(p-value = 0.1803)

# Let us try a weighted fit
# data_model_beta$resi <- residuals(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan)
# var_alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan <- lm(log(resi^2) ~ scale(Visits_tot) + 
#                                                                         scale(homo_motif) + scale(prob_consp_step) + 
#                                                                         scale(hete_motif) + Periodo + Planta,
#                                                                       data = data_model_beta)
# data_model_beta$varfunc <- exp(var_alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan$fitted.values)
# alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan_weighted <- betareg::betareg((fruitset) ~ scale(Visits_tot) + 
#                                                                                            scale(homo_motif) + scale(prob_consp_step) + 
#                                                                                            scale(hete_motif) + Periodo + Planta,
#                                                                                          weights = 1/sqrt(varfunc), data_model_beta)
# 
# summary(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan_weighted)
# lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan_weighted) # FAILED
# # Lets explore where the heterokedasticity is
# # Square the residuals
# data_model_beta$residuos_cuadrados <- data_model_beta$resi^2
# # Fit an auxiliary model with the square residuals
# modelo_aux <- lm(residuos_cuadrados ~ scale(Visits_tot) + 
#                    scale(homo_motif) + scale(prob_consp_step) + 
#                    scale(hete_motif) + Periodo + Planta, 
#                  data_model_beta)
# # White test
# white_test <- lmtest::coeftest(modelo_aux, vcov = sandwich::sandwich(modelo_aux))
# print(white_test)




alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                 scale(homo_motif) + scale(prob_consp_step) + 
                                                                                 scale(hete_motif) + Periodo, data_model_beta)
summary(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan) # prob_cons is significant
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan) # OK (p-value = 0.08405)

# Let us try a weighted fit
# data_model_beta$resi <- residuals(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan)
# var_alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan <- lm(log(resi^2) ~ scale(Visits_tot) + 
#                                                                                   scale(homo_motif) + scale(prob_consp_step) + 
#                                                                                   scale(hete_motif) + Periodo,
#                                                                                 data = data_model_beta)
# data_model_beta$varfunc <- exp(var_alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan$fitted.values)
# alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan_weighted <- betareg::betareg((fruitset) ~ scale(Visits_tot) + 
#                                                                                                      scale(homo_motif) + scale(prob_consp_step) + 
#                                                                                                      scale(hete_motif) + Periodo,
#                                                                                                    weights = 1/sqrt(varfunc), data_model_beta)
# 
# summary(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan_weighted)
# lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan_weighted) # FAILED
# # Lets explore where the heterokedasticity is
# # Square the residuals
# data_model_beta$residuos_cuadrados <- data_model_beta$resi^2
# # Fit an auxiliary model with the square residuals
# modelo_aux <- lm(residuos_cuadrados ~ scale(Visits_tot) + 
#                    scale(homo_motif) + scale(prob_consp_step) + 
#                    scale(hete_motif) + Periodo, 
#                  data_model_beta)
# # White test
# white_test <- lmtest::coeftest(modelo_aux, vcov = sandwich::sandwich(modelo_aux))
# print(white_test)




alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                scale(homo_motif) + scale(prob_consp_step) + 
                                                                                scale(hete_motif) + Planta, data_model_beta)
summary(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan) # OK (p-value = 0.1393)
# # Let us try a weighted fit
# data_model_beta$resi <- residuals(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)
# var_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan <- lm(log(resi^2) ~ scale(Visits_tot) + 
#                                                                            scale(homo_motif) + scale(prob_consp_step) + 
#                                                                            scale(hete_motif) + Planta,
#                                                                          data = data_model_beta)
# data_model_beta$varfunc <- exp(var_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan$fitted.values)
# alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan_weighted <- betareg::betareg((fruitset) ~ scale(Visits_tot) + 
#                                                                                               scale(homo_motif) + scale(prob_consp_step) + 
#                                                                                               scale(hete_motif) + Planta,
#                                                                                             weights = 1/sqrt(varfunc), data_model_beta)
# 
# summary(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan_weighted)
# lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan_weighted) # FAILED
# # Lets explore where the heterokedasticity is
# # Square the residuals
# data_model_beta$residuos_cuadrados <- data_model_beta$resi^2
# # Fit an auxiliary model with the square residuals
# modelo_aux <- lm(residuos_cuadrados ~ scale(Visits_tot) + 
#                    scale(homo_motif) + scale(prob_consp_step) + 
#                    scale(hete_motif) + Planta, 
#                  data_model_beta)
# # White test
# white_test <- lmtest::coeftest(modelo_aux, vcov = sandwich::sandwich(modelo_aux))
# print(white_test)








short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                                    scale(prob_consp_step) + 
                                                                                    Periodo + Planta, 
                                                                                  data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan)  # prop_homo is almost significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan) # FAILED (p-value = 0.02452)



short_alternative_variables_fruitset_GLM_beta_periodo_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                             scale(prob_consp_step) + 
                                                                             Periodo, 
                                                                           data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_periodo_quan) # prop_cons is significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_periodo_quan) # FAILED (p-value = 0.0165)


short_alternative_variables_fruitset_GLM_beta_planta_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                            scale(prob_consp_step) + 
                                                                            Planta, 
                                                                          data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_planta_quan) # prop_homo and prop_cons are almost significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_planta_quan) # FAILED (p-value = 0.01436)


short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(homo_motif) + scale(hete_motif) +
                                                                                              scale(prob_consp_step) + 
                                                                                              Periodo + Planta, 
                                                                                            data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan) # OK (p-value = 0.1153)



short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan <-  betareg::betareg((fruitset) ~ scale(homo_motif) + scale(hete_motif) + 
                                                                                       scale(prob_consp_step) + 
                                                                                       Periodo, 
                                                                                     data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan) #prob_consp_step is significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan) # OK (p-value = 0.08109)


short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan <-  betareg::betareg((fruitset) ~ scale(homo_motif) + scale(hete_motif) +
                                                                                      scale(prob_consp_step) + 
                                                                                      Planta, 
                                                                                    data_model_beta)
summary(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan) 
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan) # OK (p-value = 0.08246)





consp_prob_fruitset_GLM_beta_quan <-  betareg::betareg((fruitset) ~ scale(prob_consp_step), 
                                                                      data_model_beta)
summary(consp_prob_fruitset_GLM_beta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(consp_prob_fruitset_GLM_beta_quan) # FAILED (p-value = 0.03043)


consp_prob_fruitset_GLM_beta_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(prob_consp_step) + 
                                                                                                   Periodo + Planta, 
                                                                                                 data_model_beta)
summary(consp_prob_fruitset_GLM_beta_periodo_planta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(consp_prob_fruitset_GLM_beta_periodo_planta_quan) # OK (p-value = 0.06338)



consp_prob_fruitset_GLM_beta_periodo_quan <-  betareg::betareg((fruitset) ~ scale(prob_consp_step) + 
                                                                                            Periodo, 
                                                                                          data_model_beta)
summary(consp_prob_fruitset_GLM_beta_periodo_quan) #prob_consp_step is significant
#perform Breusch-Pagan Test
lmtest::bptest(consp_prob_fruitset_GLM_beta_periodo_quan) # OK (p-value = 0.09608)


consp_prob_fruitset_GLM_beta_planta_quan <-  betareg::betareg((fruitset) ~   scale(prob_consp_step) + 
                                                                                           Planta, 
                                                                                         data_model_beta)
summary(consp_prob_fruitset_GLM_beta_planta_quan) 
#perform Breusch-Pagan Test
lmtest::bptest(consp_prob_fruitset_GLM_beta_planta_quan) # OK (p-value = 0.05759)



prop_homo_fruitset_GLM_beta_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo), 
                                                       data_model_beta)
summary(prop_homo_fruitset_GLM_beta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(prop_homo_fruitset_GLM_beta_quan) # OK (p-value = 0.2036)


prop_homo_fruitset_GLM_beta_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                        Periodo + Planta, 
                                                                      data_model_beta)
summary(prop_homo_fruitset_GLM_beta_periodo_planta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(prop_homo_fruitset_GLM_beta_periodo_planta_quan) # OK (p-value = 0.4372)



prop_homo_fruitset_GLM_beta_periodo_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                 Periodo, 
                                                               data_model_beta)
summary(prop_homo_fruitset_GLM_beta_periodo_quan) #prop_homo is significant
#perform Breusch-Pagan Test
lmtest::bptest(prop_homo_fruitset_GLM_beta_periodo_quan) # OK (p-value = 0.2132)


prop_homo_fruitset_GLM_beta_planta_quan <-  betareg::betareg((fruitset) ~   scale(prop_homo) + 
                                                                Planta, 
                                                              data_model_beta)
summary(prop_homo_fruitset_GLM_beta_planta_quan) 
#perform Breusch-Pagan Test
lmtest::bptest(prop_homo_fruitset_GLM_beta_planta_quan) # OK (p-value = 0.2496)


# AIC comparisson

AIC(only_visits_fruitset_GLM_beta,
    only_visits_fruitset_GLM_beta_periodo,
    only_visits_fruitset_GLM_beta_planta,
    only_visits_fruitset_GLM_beta_bosque,
    only_visits_fruitset_GLM_beta_periodo_planta,
    alternative_variables_fruitset_GLM_beta_periodo_quan,
    alternative_variables_fruitset_GLM_beta_planta_quan,
    alternative_variables_fruitset_GLM_beta_periodo_planta_quan,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan,
    alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan,
    alternative_variables_fruitset_GLM_beta_periodo_quan_weighted,
    alternative_variables_fruitset_GLM_beta_planta_quan_weighted,
    alternative_variables_fruitset_GLM_beta_periodo_planta_quan_weighted,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan_weighted,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan_weighted,
    alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan_weighted,
    short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_periodo_quan,
    short_alternative_variables_fruitset_GLM_beta_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan,
    short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan,
    consp_prob_fruitset_GLM_beta_quan,
    consp_prob_fruitset_GLM_beta_periodo_quan,
    consp_prob_fruitset_GLM_beta_planta_quan,
    consp_prob_fruitset_GLM_beta_periodo_planta_quan,
    prop_homo_fruitset_GLM_beta_quan,
    prop_homo_fruitset_GLM_beta_periodo_quan,
    prop_homo_fruitset_GLM_beta_planta_quan,
    prop_homo_fruitset_GLM_beta_periodo_planta_quan)


# Check heterokedasticity
lmtest::bptest(only_visits_fruitset_GLM_beta) #OK
lmtest::bptest(only_visits_fruitset_GLM_beta_periodo)
lmtest::bptest(only_visits_fruitset_GLM_beta_planta)
lmtest::bptest(only_visits_fruitset_GLM_beta_bosque)
lmtest::bptest(only_visits_fruitset_GLM_beta_periodo_planta)
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_quan) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_beta_planta_quan) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_planta_quan) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan)
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan)
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_quan_weighted) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_beta_planta_quan_weighted) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_planta_quan_weighted) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan_weighted) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan_weighted) #FAILED
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan_weighted) #FAILED
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_periodo_quan) #FAILED
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_planta_quan) #FAILED
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan)
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan)
lmtest::bptest(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)
lmtest::bptest(consp_prob_fruitset_GLM_beta_quan) #FAILED
lmtest::bptest(consp_prob_fruitset_GLM_beta_periodo_quan)
lmtest::bptest(consp_prob_fruitset_GLM_beta_planta_quan)
lmtest::bptest(consp_prob_fruitset_GLM_beta_periodo_planta_quan)
lmtest::bptest(prop_homo_fruitset_GLM_beta_quan)
lmtest::bptest(prop_homo_fruitset_GLM_beta_periodo_quan)
lmtest::bptest(prop_homo_fruitset_GLM_beta_planta_quan)
lmtest::bptest(prop_homo_fruitset_GLM_beta_periodo_planta_quan)

# AIC comparisson

AIC(only_visits_fruitset_GLM_beta,
    only_visits_fruitset_GLM_beta_periodo,
    only_visits_fruitset_GLM_beta_planta,
    only_visits_fruitset_GLM_beta_bosque,
    only_visits_fruitset_GLM_beta_periodo_planta,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan,
    alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan,
    short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan,
    consp_prob_fruitset_GLM_beta_periodo_quan,
    consp_prob_fruitset_GLM_beta_planta_quan,
    consp_prob_fruitset_GLM_beta_periodo_planta_quan,
    prop_homo_fruitset_GLM_beta_quan,
    prop_homo_fruitset_GLM_beta_periodo_quan,
    prop_homo_fruitset_GLM_beta_planta_quan,
    prop_homo_fruitset_GLM_beta_periodo_planta_quan)

# Some AIC values are negative, but apparently it does not alter the AIC comparisson among several models.
# https://www.statology.org/negative-aic/

# BEST MODEL BY AIC: short_alternative_variables_fruitset_GLM_beta_planta_quan 

# Check model assumptions: Models with Bosque and Plantas do not work fine
performance::check_model(only_visits_fruitset_GLM_beta) # QQ not normal and VIF uncertainity intervals too wide
performance::check_model(only_visits_fruitset_GLM_beta_periodo) # QQ not normal and VIF uncertainity intervals too wide
performance::check_model(only_visits_fruitset_GLM_beta_planta)
performance::check_model(only_visits_fruitset_GLM_beta_bosque)
performance::check_model(only_visits_fruitset_GLM_beta_periodo_planta)
performance::check_model(alternative_variables_fruitset_GLM_beta_periodo_quan)
performance::check_model(alternative_variables_fruitset_GLM_beta_planta_quan)
performance::check_model(alternative_variables_fruitset_GLM_beta_periodo_planta_quan)
performance::check_model(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan)
performance::check_model(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan)
performance::check_model(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan)
performance::check_model(alternative_variables_fruitset_GLM_beta_periodo_quan_weighted)
performance::check_model(alternative_variables_fruitset_GLM_beta_planta_quan_weighted)
performance::check_model(alternative_variables_fruitset_GLM_beta_periodo_planta_quan_weighted)
performance::check_model(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan_weighted)
performance::check_model(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan_weighted)
performance::check_model(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan_weighted)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_periodo_quan)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_planta_quan)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan)
performance::check_model(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)


# TEST BEST AIC Models
performance::check_model(short_alternative_variables_fruitset_GLM_beta_planta_quan)
shapiro.test(residuals(short_alternative_variables_fruitset_GLM_beta_planta_quan))
summary(short_alternative_variables_fruitset_GLM_beta_planta_quan)
performance::r2(short_alternative_variables_fruitset_GLM_beta_planta_quan)

performance::check_model(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)
shapiro.test(residuals(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan))
summary(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)

performance::check_model(consp_prob_fruitset_GLM_beta_planta_quan)
performance::r2(consp_prob_fruitset_GLM_beta_planta_quan)
shapiro.test(residuals(consp_prob_fruitset_GLM_beta_planta_quan))
shapiro.test(residuals(consp_prob_fruitset_GLM_beta_planta_quan))

performance::check_model(prop_homo_fruitset_GLM_beta_planta_quan)
shapiro.test(residuals(prop_homo_fruitset_GLM_beta_planta_quan))


# Do the results change when using the full dataset??

short_alternative_variables_fruitset_GLM_beta_planta_quan_full <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                                 scale(prob_consp_step) + 
                                                                                 Planta, 
                                                                               data_model_beta_full)
summary(short_alternative_variables_fruitset_GLM_beta_planta_quan_full) # prop_homo and prop_cons are almost significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_planta_quan_full)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_planta_quan_full)

# Predictor trends look the same, but they are not significant at the 95% CI


# Do the results change when using the only C. salviifolius dataset??

short_alternative_variables_fruitset_GLM_beta_planta_quan_salvii <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                                      scale(prob_consp_step), 
                                                                                    data_model_beta_salvii)
summary(short_alternative_variables_fruitset_GLM_beta_planta_quan_salvii) # prop_homo and prop_cons trends are not significant, and opposite to those found before
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_planta_quan_salvii)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_planta_quan_salvii) # Hypotheses are not met.


write_csv(data_model_beta,"results/donana/beta_model_results.csv")