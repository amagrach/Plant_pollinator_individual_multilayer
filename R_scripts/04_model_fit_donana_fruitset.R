

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

#-------------------------------------------------------------------------
# Load data for models
number_random_steps <- 20
path_save_file <- paste0("results/donana/data for_fruitset_models_from_",
                         number_random_steps,"_rd_steps.csv")

data_model <- read_csv(path_save_file) %>%
  mutate(Periodo = as.factor(Periodo),
         Planta = as.factor(Planta),
         Bosque = as.factor(Bosque))


only_intercept_LM <- lm(fruitset ~ 1, data_model)
summary(only_intercept_LM)

only_visits_fruitset_LM <- lm(fruitset ~ scale(Visits_tot) + Planta, data_model)
summary(only_visits_fruitset_LM)

alternative_variables_fruitset_LM <- lm(fruitset ~ scale(Visits_tot) + scale(homo_motif) * scale(hete_motif) + scale(prob_consp_step) + Planta, data_model)
summary(alternative_variables_fruitset_LM)

only_visits_fruitset_GLM <- lm(log(fruitset) ~ scale(Visits_tot) + Planta, data_model %>% filter(fruitset>0))
summary(only_visits_fruitset_GLM)

alternative_variables_fruitset_GLM <- lm(log(fruitset) ~ scale(Visits_tot) +scale(homo_motif) * scale(hete_motif) + scale(prob_consp_step) + Planta, data_model %>% filter(fruitset>0))
summary(alternative_variables_fruitset_GLM)

only_intercept_LMM <- lmer(fruitset ~ 1 + (1 | Planta), data_model)
summary(only_intercept_LMM)

only_visits_fruitset_LMM <- lmer(fruitset ~ scale(Visits_tot) + (1 | Planta), data_model)
summary(only_visits_fruitset_LMM)

alternative_variables_fruitset_LMM <- lmer(fruitset ~ scale(Visits_tot) + scale(homo_motif) * scale(hete_motif) + scale(prob_consp_step) + (1 | Planta), data_model)
summary(alternative_variables_fruitset_LMM)


only_visits_fruitset_GLMM <- lmer(log(fruitset) ~ scale(Visits_tot) + (1 | Planta), data_model %>% filter(fruitset>0))
summary(only_visits_fruitset_GLMM)

alternative_variables_fruitset_GLMM <- lmer(log(fruitset) ~ scale(Visits_tot) + scale(homo_motif) * scale(hete_motif) + scale(prob_consp_step) + (1 | Planta), data_model %>% filter(fruitset>0))
summary(alternative_variables_fruitset_GLMM)


only_visits_fruitset_GLMM_gamma <- glmmTMB(fruitset ~ scale(Visits_tot) + (1 | Planta), data_model%>% filter(fruitset>0), family=Gamma(link="log"))
summary(only_visits_fruitset_GLMM_gamma)

alternative_variables_fruitset_GLMM_gamma <- glmmTMB(fruitset ~ scale(Visits_tot) + scale(homo_motif) * scale(hete_motif) + scale(prob_consp_step) + (1 | Planta), data_model %>% filter(fruitset>0), family=Gamma(link="log"))
summary(alternative_variables_fruitset_GLMM_gamma)


AIC(only_intercept_LM,only_visits_fruitset_LM,alternative_variables_fruitset_LM,
    only_visits_fruitset_GLM,alternative_variables_fruitset_GLM,
    only_intercept_LMM,only_visits_fruitset_LMM,alternative_variables_fruitset_LMM,
    only_visits_fruitset_GLMM,alternative_variables_fruitset_GLMM,
    only_visits_fruitset_GLMM_gamma,alternative_variables_fruitset_GLMM_gamma)


# Check model assumptions
check_model(alternative_variables_fruitset_LM)
check_model(alternative_variables_fruitset_GLM)
car::vif(alternative_variables_fruitset_LM)
car::vif(alternative_variables_fruitset_GLM)


res_alternative_variables_fruitset_LM <- simulateResiduals(fittedModel = alternative_variables_fruitset_LM)
res_alternative_variables_fruitset_GLM <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLM)
res_alternative_variables_fruitset_LMM <- simulateResiduals(fittedModel = alternative_variables_fruitset_LMM)
res_alternative_variables_fruitset_GLMM <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLMM)
res_alternative_variables_fruitset_GLMM_gamma <- simulateResiduals(fittedModel = alternative_variables_fruitset_GLMM_gamma)

plot(res_alternative_variables_fruitset_LM)
plot(res_alternative_variables_fruitset_GLM)
plot(res_alternative_variables_fruitset_LMM)
plot(res_alternative_variables_fruitset_GLMM)
plot(res_alternative_variables_fruitset_GLMM_gamma)


performance::r2(alternative_variables_fruitset_LM)
performance::r2(alternative_variables_fruitset_GLM)
performance::r2(alternative_variables_fruitset_LMM)
performance::r2(alternative_variables_fruitset_GLMM)
performance::r2(alternative_variables_fruitset_GLMM_gamma)

library(sjPlot)
library(sjmisc)

plot_model(alternative_variables_fruitset_LMM, type = "pred", terms = c("homo_motif", "hete_motif"))
