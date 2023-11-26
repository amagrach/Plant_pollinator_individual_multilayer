

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
path_save_file <- paste0("results/donana/NEW_data for_fruitset_models_from_",
                         number_random_steps,"_rd_steps.csv")


data_model_raw <- read_csv(path_save_file) %>% filter(!is.na(Planta)) 

data_model_raw$unloyal_steps[is.na(data_model_raw$unloyal_steps)] <- 0
data_model_raw$homo_motif[is.na(data_model_raw$homo_motif)] <- 0
data_model_raw$hete_motif[is.na(data_model_raw$hete_motif)] <- 0
data_model_raw$Visits_tot[is.na(data_model_raw$Visits_tot)] <- 0

data_model <- data_model_raw %>%
  filter(!is.na(prob_consp_step),!is.na(poll_abundance),
         !is.na(poll_richness),!is.na(plant_richness),!is.na( total_number_flowers)) %>%
  mutate(Periodo = as.factor(Periodo),
         Planta = as.factor(Planta),
         Bosque = as.factor(Bosque))

# data_model$total_steps[is.na(data_model$total_steps)] <- 0
# data_model$total_sequences[is.na(data_model$total_sequences)] <- 0
# data_model$prop_unloyal <- data_model$unloyal_steps/data_model$total_steps

# data_model <- data_model %>% filter(!is.na(prop_unloyal))

unique(data_model$Planta)


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

ggpairs(data_model %>% mutate(logit_fruitset = boot::logit(fruitset)) %>% 
          dplyr::select("logit_fruitset","homo_motif","hete_motif","prop_homo",
                        "prob_consp_step"), 
        title="correlogram") 

# Plot response variable
ggplot(data_model)+
  geom_density(aes(x=fruitset))


#########################################################################
# Check outlayers -----------------------------------------
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

data_salvii <- data_model %>% filter(Planta=="Cistus_salviifolius")

test_cistus_salviifolius <- EnvStats::rosnerTest(data_salvii$fruitset,
                                            k = 10
)
test_cistus_salviifolius

data_salvii_filtered <- data_salvii %>%
   filter(!row_number() %in% c(21,45,38,27))

data_other_species <- data_model %>% filter(Planta != "Cistus_salviifolius")
data_model_filtered <- bind_rows(data_salvii_filtered,data_other_species)


data_model_beta_full <- data_model_filtered
data_model_beta_full$fruitset[data_model_beta_full$fruitset==1] <- 1-1e-3
data_model_beta_full$fruitset[data_model_beta_full$fruitset==0] <- 1e-3

data_model_beta <- data_model_beta_full #%>%
  #filter(Planta != "Cistus_salviifolius")# White test suggest this species highly increase heterokedasticity

data_model_beta_salvii <- data_model_beta_full %>%
  filter(Planta == "Cistus_salviifolius")

# Pairwise visualization
ggpairs(data_model %>% dplyr::select("fruitset","homo_motif",
                                     "hete_motif","prop_homo",
                                     "prob_consp_step"), 
        title="correlogram") 

ggpairs(data_model_beta %>% mutate(log_fruitset = log(fruitset)) %>% 
          dplyr::select("log_fruitset","homo_motif","hete_motif","prop_homo",
                        "prob_consp_step","poll_richness","plant_richness",
                        "total_number_flowers","Bosque","Periodo"), 
        title="correlogram") 

ggpairs(data_model_beta %>% mutate(logit_fruitset = boot::logit(fruitset)) %>% 
          dplyr::select("logit_fruitset","homo_motif","hete_motif","prop_homo",
                        "prob_consp_step","poll_richness","plant_richness",
                        "total_number_flowers","Bosque","Periodo"), 
        title="correlogram") 

# fit a beta distr. to our fitness data
fitdistrplus::descdist(data_model$fruitset, discrete = FALSE)

library(MuMIn)

cor.test((data_model_beta$homo_motif),(data_model_beta$poll_richness))
cor.test(scale(data_model_beta$homo_motif),scale(data_model_beta$poll_richness))

cor.test((data_model_beta$homo_motif),(data_model_beta$Visits_tot))
cor.test(scale(data_model_beta$homo_motif),scale(data_model_beta$Visits_tot))

data_model_beta_full %>% group_by(Planta) %>% count()
plantas_selected <- c("Cistus_salviifolius", "Cistus_crispus", "Cistus_ladanifer")
data_model_beta_selection2 <- data_model_beta_full 
data_model_beta_selection2$log_prob_cons <- log10(data_model_beta_selection2$prob_consp_step)

# The best model we found
fullmodel4_RD2<- glmmTMB(fruitset^2 ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  + scale(poll_abundance)  +
                           scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(log_prob_cons)+(1|Bosque) + Planta + Periodo,
                         family=gaussian(link="log"),
                         data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # OK QQ, Residuals not OK
test_predictions <- tibble(predicted_y = exp(predict(fullmodel4_RD2)),
                           real_y = data_model_beta_selection2$fruitset,
                           Planta = data_model_beta_selection2$Planta)
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection2$X_rd,
                           y= data_model_beta_selection2$Y_rd)
ggplot(test_predictions,aes(predicted_y,real_y,color=Planta))+
  geom_point(size=2,alpha=0.5)+geom_smooth(method = "lm",se=F)+
  geom_abline(slope = 1,intercept = 0)
ggplot(test_predictions,aes(predicted_y,real_y))+
  geom_point(size=2,alpha=0.5)+geom_smooth(method = "lm",se=F)+
  geom_abline(slope = 1,intercept = 0)
ggplot(test_predictions %>% filter(real_y>1e-3),
       aes(log10(predicted_y),log10(real_y),color=Planta))+
  geom_point(size=2,alpha=0.5)+
  geom_abline(slope = 1,intercept = 0)
cor.test(data_model_beta_selection2$fruitset,exp(predict(fullmodel4_RD2)))

write_csv(data_model_beta_selection2,"results/donana/data_gaussian_log_fullmodel.csv")
