

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

library(EnvStats)
test_cistus_crispus <- EnvStats::rosnerTest(data_model$fruitset[data_model$Planta=="Cistus_crispus"],
                   k = 2
)
test_cistus_crispus

test_cistus_salviifolius <- EnvStats::rosnerTest(data_model$fruitset[data_model$Planta=="Cistus_salviifolius"],
                                            k = 3
)
test_cistus_salviifolius # Remove 3-8 Cistus_salviifolius 6-2 Cistus_salviifolius

data_model_filtered <- data_model %>%
  filter(!row_number() %in% c(99, 62)) # filter outlaiers

data_model_beta <- data_model_filtered
data_model_beta$fruitset[data_model_beta$fruitset==1] <- 1-1e-3
data_model_beta$fruitset[data_model_beta$fruitset==0] <- 1e-3



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
lmtest::bptest(only_visits_fruitset_GLM_beta_bosque)# FAILED

only_visits_fruitset_GLM_beta_periodo_planta <-  betareg::betareg((fruitset) ~ scale(Visits_tot)+Periodo+Planta, 
                                                          data_model_beta)
summary(only_visits_fruitset_GLM_beta_periodo_planta)
#perform Breusch-Pagan Test
lmtest::bptest(only_visits_fruitset_GLM_beta_periodo_planta)# FAILED


alternative_variables_fruitset_GLM_beta_periodo_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                       scale(prop_homo) + 
                                                                       scale(prob_consp_step) + 
                                                                       Periodo, 
                                                                     data_model_beta)
summary(alternative_variables_fruitset_GLM_beta_periodo_quan) # prob_consp is significant
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_quan)# FAILED (p-value = 0.02157)

alternative_variables_fruitset_GLM_beta_planta_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                      scale(prop_homo) + 
                                                                      scale(prob_consp_step) + 
                                                                      Planta, 
                                                                    data_model_beta)
summary(alternative_variables_fruitset_GLM_beta_planta_quan)  # prop_homo is almost significant
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_beta_planta_quan)# FAILED


alternative_variables_fruitset_GLM_beta_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                              scale(prop_homo) + 
                                                                              scale(prob_consp_step) + 
                                                                              Periodo+Planta, 
                                                                            data_model_beta)
summary(alternative_variables_fruitset_GLM_beta_periodo_planta_quan) # prop_homo is almost significant
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_beta_periodo_planta_quan)# FAILED

alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                        scale(homo_motif) + scale(prob_consp_step) + 
                                                                                        scale(hete_motif) + Periodo + Planta, data_model_beta)
summary(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan) # FAILED

alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                 scale(homo_motif) + scale(prob_consp_step) + 
                                                                                 scale(hete_motif) + Periodo, data_model_beta)
summary(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan) # prob_cons is significant
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan) # FAILED (p-value = 0.03953)

alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                                scale(homo_motif) + scale(prob_consp_step) + 
                                                                                scale(hete_motif) + Planta, data_model_beta)
summary(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan) # FAILED

short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                                    scale(prob_consp_step) + 
                                                                                    Periodo + Planta, 
                                                                                  data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan)  # prop_homo is almost significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan) # FAILED



short_alternative_variables_fruitset_GLM_beta_periodo_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                             scale(prob_consp_step) + 
                                                                             Periodo, 
                                                                           data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_periodo_quan) # prop_cons is significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_periodo_quan) # FAILED (p-value = 0.009001)


short_alternative_variables_fruitset_GLM_beta_planta_quan <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                            scale(prob_consp_step) + 
                                                                            Planta, 
                                                                          data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_planta_quan) # prop_homo and prop_cons are almost significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_planta_quan) # FAILED


short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan <-  betareg::betareg((fruitset) ~ scale(homo_motif) + scale(hete_motif) +
                                                                                              scale(prob_consp_step) + 
                                                                                              Periodo + Planta, 
                                                                                            data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan)
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan) # FAILED



short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan <-  betareg::betareg((fruitset) ~ scale(homo_motif) + scale(hete_motif) + 
                                                                                       scale(prob_consp_step) + 
                                                                                       Periodo, 
                                                                                     data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan) #prob_consp_step is significant
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan) # FAILED (p-value = 0.03148)


short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan <-  betareg::betareg((fruitset) ~ scale(homo_motif) + scale(hete_motif) +
                                                                                      scale(prob_consp_step) + 
                                                                                      Planta, 
                                                                                    data_model_beta)
summary(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan) 
#perform Breusch-Pagan Test
lmtest::bptest(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan) # FAILED









AIC(only_visits_fruitset_GLM_beta,
    only_visits_fruitset_GLM_beta_periodo,
    only_visits_fruitset_GLM_beta_planta,
    only_visits_fruitset_GLM_beta_bosque,
    only_visits_fruitset_GLM_beta_periodo_planta,
    alternative_variables_fruitset_GLM_beta_periodo,
    alternative_variables_fruitset_GLM_beta_planta,
    alternative_variables_fruitset_GLM_beta_periodo_planta,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo,
    alternative_variables_fruitset_GLM_homo_hete_beta_planta,
    short_alternative_variables_fruitset_GLM_beta_periodo_planta,
    short_alternative_variables_fruitset_GLM_beta_periodo,
    short_alternative_variables_fruitset_GLM_beta_planta,
    short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta,
    short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo,
    short_alternative_variables_fruitset_GLM_homo_hete_beta_planta,
    alternative_variables_fruitset_GLM_beta_periodo_quan,
    alternative_variables_fruitset_GLM_beta_planta_quan,
    alternative_variables_fruitset_GLM_beta_periodo_planta_quan,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan,
    alternative_variables_fruitset_GLM_homo_hete_beta_periodo_quan,
    alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_periodo_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_periodo_quan,
    short_alternative_variables_fruitset_GLM_beta_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan,
    short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan,
    short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)





# Some AIC values are negative, but apparently it does not alter the AIC comparisson among several models.
# https://www.statology.org/negative-aic/

# Check model assumptions: Models with Bosque and Plantas do not work fine
performance::check_model(only_visits_fruitset_GLM_beta) # QQ not normal
performance::check_model(only_visits_fruitset_GLM_beta_periodo) # QQ not normal
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
performance::check_model(short_alternative_variables_fruitset_GLM_beta_periodo_quan)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_planta_quan)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_planta_quan)
performance::check_model(short_alternative_variables_fruitset_GLM_beta_homo_hete_periodo_quan)  # QQ not normal
performance::check_model(short_alternative_variables_fruitset_GLM_homo_hete_beta_planta_quan)


performance::r2(alternative_variables_fruitset_GLM_homo_hete_beta_periodo_planta_quan)
