

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

data_model <- read_csv(path_save_file) %>%
  mutate(prop_homo = homo_motif/(homo_motif+hete_motif)) %>%
  filter(!is.na(prop_homo),!is.na(prob_consp_step)) %>%
  mutate(Periodo = as.factor(Periodo),
         Planta = as.factor(Planta),
         Bosque = as.factor(Bosque),
         prop_homo = homo_motif/(homo_motif+hete_motif))

data_model$cat_prob_consp_step <- "p<0.8"
data_model$cat_prob_consp_step[data_model$prob_consp_step > 0.8] <- "p>0.8"
data_model$cat_prob_consp_step <- as.factor(data_model$cat_prob_consp_step)

data_model_beta <- data_model
data_model_beta$fruitset[data_model_beta$fruitset==1] <- 1-1e-3
data_model_beta$fruitset[data_model_beta$fruitset==0] <- 1e-3

# Plot response variable
ggplot(data_model)+
  geom_density(aes(x=fruitset))

ggplot(data_model)+
  geom_histogram(aes(x=fruitset))




# fit a beta distr. to our fitness data
fitdistrplus::descdist(data_model$fruitset, discrete = FALSE)


# MODELS WITHOUT RANDOM EFFECTS---------------------------


only_intercept_LM <- lm(fruitset ~ 1, data_model)
summary(only_intercept_LM)

only_visits_fruitset_LM <- lm(fruitset ~ scale(Visits_tot) + Periodo, data_model)
summary(only_visits_fruitset_LM)

only_visits_fruitset_GLM <- lm(log(fruitset) ~ scale(Visits_tot) + Periodo, data_model %>% filter(fruitset>0))
summary(only_visits_fruitset_GLM)

only_visits_fruitset_GLM_binomial <- glm((fruitset) ~ scale(Visits_tot), data_model, family = quasibinomial)
summary(only_visits_fruitset_GLM_binomial)

alternative_variables_fruitset_LM <- lm(fruitset ~ scale(Visits_tot) + scale(prop_homo) + (cat_prob_consp_step) + Periodo, data_model)
summary(alternative_variables_fruitset_LM)

alternative_variables_fruitset_GLM <- lm(log(fruitset) ~ scale(Visits_tot) + scale(prop_homo) + (cat_prob_consp_step) + Periodo, data_model %>% filter(fruitset>0))
summary(alternative_variables_fruitset_GLM)

alternative_variables_fruitset_GLM_binomial <- glm((fruitset) ~ scale(Visits_tot) + scale(prop_homo) + (cat_prob_consp_step) + Periodo, data_model, family = quasibinomial)
summary(alternative_variables_fruitset_GLM_binomial)

alternative_variables_fruitset_LM_homo_hete <- lm(fruitset ~ scale(Visits_tot) + scale(homo_motif) + (cat_prob_consp_step) + scale(hete_motif) + Periodo, data_model)
summary(alternative_variables_fruitset_LM_homo_hete)

alternative_variables_fruitset_GLM_homo_hete_binomial <- glm((fruitset) ~ scale(Visits_tot) + scale(homo_motif) + (cat_prob_consp_step) + scale(hete_motif) + Periodo, data_model, family = quasibinomial)
summary(alternative_variables_fruitset_GLM_homo_hete_binomial)

short_alternative_variables_fruitset_LM <- lm(fruitset ~ scale(prop_homo) + (cat_prob_consp_step) + Periodo, data_model)
summary(short_alternative_variables_fruitset_LM)

short_alternative_variables_fruitset_GLM <- lm(log(fruitset) ~ scale(prop_homo) + (cat_prob_consp_step) + Periodo, data_model %>% filter(fruitset>0))
summary(short_alternative_variables_fruitset_GLM)

short_alternative_variables_fruitset_GLM_binomial <- glm((fruitset) ~ scale(prop_homo) + (cat_prob_consp_step) + Periodo, data_model, family = quasibinomial)
summary(short_alternative_variables_fruitset_GLM_binomial)

short_prop_homo_fruitset_LM <- lm(fruitset ~ scale(prop_homo) + Periodo, data_model)
summary(short_prop_homo_fruitset_LM)

short_prob_consp_fruitset_LM <- lm(fruitset ~ (cat_prob_consp_step) + Periodo, data_model)
summary(short_prob_consp_fruitset_LM)

short_homo_fruitset_LM <- lm(fruitset ~ scale(homo_motif) + Periodo, data_model)
summary(short_homo_fruitset_LM)

short_hete_fruitset_LM <- lm(fruitset ~ scale(hete_motif) + Periodo, data_model)
summary(short_hete_fruitset_LM)

short_prop_homo_fruitset_GLM <- lm(log(fruitset) ~ scale(prop_homo) + Periodo, data_model %>% filter(fruitset>0))
summary(short_prop_homo_fruitset_GLM)

short_prob_consp_fruitset_GLM <- lm(log(fruitset) ~ (cat_prob_consp_step) + Periodo, data_model %>% filter(fruitset>0))
summary(short_prob_consp_fruitset_GLM)

short_homo_fruitset_GLM <- lm(log(fruitset) ~ scale(homo_motif) + Periodo, data_model %>% filter(fruitset>0))
summary(short_homo_fruitset_GLM)

short_hete_fruitset_GLM <- lm(log(fruitset) ~ scale(hete_motif) + Periodo, data_model %>% filter(fruitset>0))
summary(short_hete_fruitset_GLM)


short_prop_homo_fruitset_GLM_binomial <- glm((fruitset) ~ scale(prop_homo) + Periodo, data_model, family = quasibinomial)
summary(short_prop_homo_fruitset_GLM_binomial)

short_prob_consp_fruitset_GLM_binomial <- glm((fruitset) ~ (cat_prob_consp_step) + Periodo, data_model, family = quasibinomial)
summary(short_prob_consp_fruitset_GLM_binomial)

short_homo_fruitset_GLM_binomial <- glm((fruitset) ~ scale(homo_motif) + Periodo, data_model, family = quasibinomial)
summary(short_homo_fruitset_GLM_binomial)

short_hete_fruitset_GLM_binomial <- glm((fruitset) ~ scale(hete_motif) + Periodo, data_model, family = quasibinomial)
summary(short_hete_fruitset_GLM_binomial)


# BETA distribution fit
only_visits_fruitset_GLM_beta <-  betareg::betareg((fruitset) ~ scale(Visits_tot), 
                                                   data_model_beta)
summary(only_visits_fruitset_GLM_beta)
AIC(only_visits_fruitset_GLM_beta)

alternative_variables_fruitset_GLM_beta <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                               scale(prop_homo) + 
                                                               (cat_prob_consp_step) + 
                                                               Periodo, 
                                                             data_model_beta)
summary(alternative_variables_fruitset_GLM_beta) # Select
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_beta) 

alternative_variables_fruitset_GLM_homo_hete_beta <-  betareg::betareg((fruitset) ~ scale(Visits_tot) + 
                                                                         scale(homo_motif) + (cat_prob_consp_step) + 
                                                                         scale(hete_motif) + Periodo, data_model_beta)
summary(alternative_variables_fruitset_GLM_homo_hete_beta)
#perform Breusch-Pagan Test
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta) 
# Since the p-value is more than 0.05, we can not reject the null hypothesis. We do not have sufficient evidence to say that heteroscedasticity is present in the regression model.

# data_model_beta$resi <- residuals(alternative_variables_fruitset_GLM_homo_hete_beta,
#                                   type = "response")
# 
# ggplot(data_model_beta, aes(y = resi, x = scale(Visits_tot), color = Planta)) + geom_point() + geom_abline(slope = 0)
# ggplot(data_model_beta, aes(y = resi, x = scale(homo_motif), color = Planta)) + geom_point() + geom_abline(slope = 0)
# ggplot(data_model_beta, aes(y = resi, x = (cat_prob_consp_step), color = Planta)) + geom_point() + geom_abline(slope = 0)
# ggplot(data_model_beta, aes(y = resi, x = scale(hete_motif), color = Planta)) + geom_point() + geom_abline(slope = 0)
# 
# 
# #alternative_variables_fruitset_GLM_homo_hete_beta$residuals
# varalternative_variables_fruitset_GLM_homo_hete_beta <- lm(log(resi^2) ~ scale(Visits_tot) + 
#                                                              scale(homo_motif) + (cat_prob_consp_step) + 
#                                                              scale(hete_motif) + Periodo, 
#                                                            data = data_model_beta)
# data_model_beta$varfunc <- exp(varalternative_variables_fruitset_GLM_homo_hete_beta$fitted.values)
# alternative_variables_fruitset_GLM_homo_hete_beta_weignted <- betareg::betareg((fruitset) ~ scale(Visits_tot) + 
#                                                                                  scale(homo_motif) + (cat_prob_consp_step) + 
#                                                                                  scale(hete_motif) + Periodo, 
#                                                                                weights = 1/sqrt(varfunc), data_model_beta)
# 
# summary(alternative_variables_fruitset_GLM_homo_hete_beta_weignted)
# plot(alternative_variables_fruitset_GLM_homo_hete_beta_weignted)
# lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta_weignted) 
# Since the p-value is less than 0.05, we can reject the null hypothesis. We do have sufficient evidence to say that heteroscedasticity is present in the regression model.



short_alternative_variables_fruitset_GLM_beta <-  betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                                     (cat_prob_consp_step) + 
                                                                     Periodo, 
                                                                   data_model_beta)
summary(short_alternative_variables_fruitset_GLM_beta)  # Select

short_prop_homo_fruitset_GLM_beta <- betareg::betareg((fruitset) ~ scale(prop_homo) + 
                                                        Periodo, data_model_beta)
summary(short_prop_homo_fruitset_GLM_beta)

short_prob_consp_fruitset_GLM_beta <- betareg::betareg((fruitset) ~ (cat_prob_consp_step) + 
                                                         Periodo, 
                                                       data_model_beta)
summary(short_prob_consp_fruitset_GLM_beta)

short_homo_fruitset_GLM_beta <- betareg::betareg((fruitset) ~ scale(homo_motif) + 
                                                   Periodo, 
                                                 data_model_beta)
summary(short_homo_fruitset_GLM_beta)

short_hete_fruitset_GLM_beta <- betareg::betareg((fruitset) ~ scale(hete_motif) + 
                                                   Periodo, data_model_beta)
summary(short_hete_fruitset_GLM_beta)

# MODELS WITH RANDOM EFFECTS---------------------------

only_intercept_LMM <- lmer(fruitset ~ 1 + (1 | Periodo), data_model)
summary(only_intercept_LMM)

only_visits_fruitset_LMM <- lmer(fruitset ~ scale(Visits_tot) + (1 |  Periodo), data_model)
summary(only_visits_fruitset_LMM)

alternative_variables_fruitset_LMM <- lmer(fruitset ~ scale(Visits_tot) + (cat_prob_consp_step) + (cat_prob_consp_step) + (1 |  Periodo), data_model)
summary(alternative_variables_fruitset_LMM)

short_alternative_variables_fruitset_LMM <- lmer(fruitset ~  (cat_prob_consp_step) + (1 |  Periodo), data_model)
summary(short_alternative_variables_fruitset_LMM)


only_visits_fruitset_GLMM <- lmer(log(fruitset) ~ scale(Visits_tot) + (1 |  Periodo), data_model %>% filter(fruitset>0))
summary(only_visits_fruitset_GLMM)

alternative_variables_fruitset_GLMM <- lmer(log(fruitset) ~ scale(Visits_tot) + scale(prop_homo) + (cat_prob_consp_step) + (1 |  Periodo), data_model %>% filter(fruitset>0))
summary(alternative_variables_fruitset_GLMM)

short_alternative_variables_fruitset_GLMM <- lmer(log(fruitset) ~ (cat_prob_consp_step) + (1 |  Periodo), data_model %>% filter(fruitset>0))
summary(short_alternative_variables_fruitset_GLMM)

only_visits_fruitset_GLMM_gamma <- glmmTMB(fruitset ~ scale(Visits_tot) + (1 |  Periodo), data_model%>% filter(fruitset>0), family=Gamma(link="log"))
summary(only_visits_fruitset_GLMM_gamma)

alternative_variables_fruitset_GLMM_gamma <- glmmTMB(fruitset ~ scale(Visits_tot) + scale(prop_homo) + (cat_prob_consp_step) + (1 |  Periodo), data_model %>% filter(fruitset>0), family=Gamma(link="log"))
summary(alternative_variables_fruitset_GLMM_gamma)

short_alternative_variables_fruitset_GLMM_gamma <- glmmTMB(fruitset ~   (cat_prob_consp_step) + (1 |  Periodo), data_model %>% filter(fruitset>0), family=Gamma(link="log"))
summary(short_alternative_variables_fruitset_GLMM_gamma)


only_visits_fruitset_GLMM_beta <- glmmTMB(fruitset ~ scale(Visits_tot) + (1 |  Periodo), data_model_beta, family=beta_family(link="logit"))
summary(only_visits_fruitset_GLMM_gamma_beta)

alternative_variables_fruitset_GLMM_beta <- glmmTMB(fruitset ~ scale(Visits_tot) + scale(prop_homo) + (cat_prob_consp_step) + (1 |  Periodo), data_model_beta, family=beta_family(link="logit"))
summary(alternative_variables_fruitset_GLMM_gamma_beta)

short_alternative_variables_fruitset_GLMM_beta <- glmmTMB(fruitset ~   scale(prop_homo) + (cat_prob_consp_step) + (1 |  Periodo), data_model_beta, family=beta_family(link="logit"))
summary(short_alternative_variables_fruitset_GLMM_beta)


AIC(only_intercept_LM,only_visits_fruitset_LM,alternative_variables_fruitset_LM,short_alternative_variables_fruitset_LM,
    only_visits_fruitset_GLM,alternative_variables_fruitset_GLM,short_alternative_variables_fruitset_GLM,
    only_visits_fruitset_GLM_beta,alternative_variables_fruitset_GLM_beta,
    alternative_variables_fruitset_GLM_homo_hete_beta,short_alternative_variables_fruitset_GLM_beta,
    short_prop_homo_fruitset_GLM_beta,short_prob_consp_fruitset_GLM_beta,short_homo_fruitset_GLM_beta,short_hete_fruitset_GLM_beta,
    only_intercept_LMM,only_visits_fruitset_LMM,alternative_variables_fruitset_LMM,short_alternative_variables_fruitset_LMM,
    only_visits_fruitset_GLMM,alternative_variables_fruitset_GLMM,short_alternative_variables_fruitset_GLMM,
    only_visits_fruitset_GLMM_gamma,alternative_variables_fruitset_GLMM_gamma,short_alternative_variables_fruitset_GLMM_gamma,
    only_visits_fruitset_GLMM_beta,alternative_variables_fruitset_GLMM_beta,short_alternative_variables_fruitset_GLMM_beta)

# Some AIC values are negative, but apparently it does not alter the AIC comparisson among several models.
# https://www.statology.org/negative-aic/

# Check model assumptions: Models with Bosque and Plantas do not work fine
performance::check_model(only_visits_fruitset_LM) #Heterokedast
performance::check_model(only_visits_fruitset_GLM) # OK

performance::check_model(alternative_variables_fruitset_LM) #Heterokedast
performance::check_model(alternative_variables_fruitset_GLM) #Heterokedast? QQ-problems

performance::check_model(alternative_variables_fruitset_LM_homo_hete) #Heterokedast? QQ-problems
performance::check_model(alternative_variables_fruitset_GLM_homo_hete)

performance::check_model(short_alternative_variables_fruitset_LM)#Heterokedast? QQ-problems
performance::check_model(short_alternative_variables_fruitset_GLM)#Heterokedast? QQ-problems

performance::check_model(short_prob_consp_fruitset_LM) #Heterokedast? QQ-problems
performance::check_model(short_prop_homo_fruitset_LM)#Heterokedast? QQ-problems
performance::check_model(short_homo_fruitset_LM)#Heterokedast? QQ-problems
performance::check_model(short_hete_fruitset_LM) #Not OK


performance::check_model(short_hete_fruitset_GLM)
performance::check_model(short_homo_fruitset_GLM)
performance::check_model(short_prob_consp_fruitset_GLM)
performance::check_model(short_prop_homo_fruitset_GLM)

performance::check_model(only_visits_fruitset_GLM_binomial)
performance::check_model(alternative_variables_fruitset_GLM_binomial)
performance::check_model(alternative_variables_fruitset_GLM_homo_hete_binomial) 
performance::check_model(short_alternative_variables_fruitset_GLM_binomial) # QQ-problems
performance::check_model(short_hete_fruitset_GLM_binomial)# QQ-problems
performance::check_model(short_homo_fruitset_GLM_binomial)# QQ-problems
performance::check_model(short_prob_consp_fruitset_GLM_binomial)# QQ-problems
performance::check_model(short_prop_homo_fruitset_GLM_binomial)# QQ-problems


performance::check_model(only_visits_fruitset_GLM_beta)# QQ-problems
performance::check_model(alternative_variables_fruitset_GLM_beta)
performance::check_model(alternative_variables_fruitset_GLM_homo_hete_beta)
performance::check_model(short_alternative_variables_fruitset_GLM_beta)
performance::check_model(short_prop_homo_fruitset_GLM_beta)
performance::check_model(short_prob_consp_fruitset_GLM_beta)
performance::check_model(short_homo_fruitset_GLM_beta)
performance::check_model(short_hete_fruitset_GLM_beta)

lmtest::bptest(alternative_variables_fruitset_GLM_beta) # hete
lmtest::bptest(alternative_variables_fruitset_GLM_homo_hete_beta) # homokedasticity OK
lmtest::bptest(short_alternative_variables_fruitset_GLM_beta) # HETEROkedasticity

car::vif(alternative_variables_fruitset_GLM_beta) #OK
car::vif(alternative_variables_fruitset_GLM_homo_hete_beta) #OK
car::vif(short_alternative_variables_fruitset_GLM_beta) #OK

plot(alternative_variables_fruitset_GLM_beta)
plot(alternative_variables_fruitset_GLM_homo_hete_beta)
plot(short_alternative_variables_fruitset_GLM_beta)


car::vif(alternative_variables_fruitset_GLM_homo_hete_binomial)
car::vif(short_alternative_variables_fruitset_LM)
car::vif(short_alternative_variables_fruitset_GLM)
car::vif(alternative_variables_fruitset_LM)
car::vif(alternative_variables_fruitset_GLM_homo_hete)

res_short_alternative_variables_fruitset_LM <- simulateResiduals(fittedModel = short_alternative_variables_fruitset_LM)
res_short_alternative_variables_fruitset_GLM <- simulateResiduals(fittedModel = short_alternative_variables_fruitset_GLM)
res_short_alternative_variables_fruitset_LMM <- simulateResiduals(fittedModel = short_alternative_variables_fruitset_LMM)
res_short_alternative_variables_fruitset_GLMM <- simulateResiduals(fittedModel = short_alternative_variables_fruitset_GLMM)
res_short_alternative_variables_fruitset_GLMM_gamma <- simulateResiduals(fittedModel = short_alternative_variables_fruitset_GLMM_gamma)

res_alternative_variables_fruitset_LM <- simulateResiduals(fittedModel = alternative_variables_fruitset_LM)
res_alternative_variables_fruitset_GLM <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLM)
res_alternative_variables_fruitset_LMM <- simulateResiduals(fittedModel = alternative_variables_fruitset_LMM)
res_alternative_variables_fruitset_GLMM <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLMM)
res_alternative_variables_fruitset_GLMM_gamma <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLMM_gamma)
res_alternative_variables_fruitset_GLMM_beta <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLMM_beta)
res_short_alternative_variables_fruitset_GLMM_beta <- simulateResiduals(fittedModel = short_alternative_variables_fruitset_GLMM_beta)

# res_only_visits_fruitset_GLM_beta <- simulateResiduals(fittedModel = only_visits_fruitset_GLM_beta)
# res_alternative_variables_fruitset_GLM_beta <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLM_beta)
# res_alternative_variables_fruitset_GLM_homo_hete_beta <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLM_homo_hete_beta)
# res_short_alternative_variables_fruitset_GLM_beta <- simulateResiduals(fittedModel = short_alternative_variables_fruitset_GLM_beta)
# res_short_prop_homo_fruitset_GLM_beta <- simulateResiduals(fittedModel = short_prop_homo_fruitset_GLM_beta)
# res_short_prob_consp_fruitset_GLM_beta <- simulateResiduals(fittedModel = short_prob_consp_fruitset_GLM_beta)
# res_short_homo_fruitset_GLM_beta <- simulateResiduals(fittedModel = short_homo_fruitset_GLM_beta)
# res_short_hete_fruitset_GLM_beta <- simulateResiduals(fittedModel = short_hete_fruitset_GLM_beta)


plot(res_short_alternative_variables_fruitset_LM)
plot(res_short_alternative_variables_fruitset_GLM)
plot(res_short_alternative_variables_fruitset_LMM)
plot(res_short_alternative_variables_fruitset_GLMM)
plot(res_short_alternative_variables_fruitset_GLMM_gamma)

plot(res_alternative_variables_fruitset_LM)
plot(res_alternative_variables_fruitset_GLM)
plot(res_alternative_variables_fruitset_LMM)
plot(res_alternative_variables_fruitset_GLMM)
plot(res_alternative_variables_fruitset_GLMM_gamma)
plot(res_alternative_variables_fruitset_GLMM_beta)
plot(res_short_alternative_variables_fruitset_GLMM_beta)

# plot(res_only_visits_fruitset_GLM_beta)
# plot(res_alternative_variables_fruitset_GLM_beta)
# plot(res_alternative_variables_fruitset_GLM_homo_hete_beta)
# plot(res_short_alternative_variables_fruitset_GLM_beta)
# plot(res_short_prop_homo_fruitset_GLM_beta)
# plot(res_short_prob_consp_fruitset_GLM_beta)
# plot(res_short_homo_fruitset_GLM_beta)
# plot(res_short_hete_fruitset_GLM_beta)


performance::r2(alternative_variables_fruitset_LM)
performance::r2(alternative_variables_fruitset_GLM)
performance::r2(alternative_variables_fruitset_LMM)
performance::r2(alternative_variables_fruitset_GLMM)
performance::r2(alternative_variables_fruitset_GLMM_gamma)

library(sjPlot)
library(sjmisc)

plot_model(alternative_variables_fruitset_LMM, type = "pred", terms = c("homo_motif", "hete_motif"))
