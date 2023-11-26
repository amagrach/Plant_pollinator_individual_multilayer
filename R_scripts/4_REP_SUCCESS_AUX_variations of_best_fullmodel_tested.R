
data_model_beta_full %>% group_by(Planta) %>% count()
plantas_selected <- c("Cistus_salviifolius", "Cistus_crispus", "Cistus_ladanifer")
data_model_beta_selection2 <- data_model_beta_full #%>% filter(fruitset>0) #%>% filter(Planta %in% plantas_selected)
data_model_beta_selection2$X_rd <- data_model_beta_selection2$X + rnorm(length(data_model_beta_selection2$X), mean = 0, sd = 0.01)
data_model_beta_selection2$Y_rd <- data_model_beta_selection2$Y + rnorm(length(data_model_beta_selection2$Y), mean = 0, sd = 0.01)
data_model_beta_selection2$log_prob_cons <- log10(data_model_beta_selection2$prob_consp_step)
data_model_beta_selection2$fruitset_sq <- (data_model_beta_selection2$prob_consp_step)^1

# ggplot(data_model_beta_selection2,aes(scale(prob_consp_step),fruitset,color=Planta))+
#   geom_point(size=2,alpha=0.5)+facet_grid(Periodo~Bosque)
# ggplot(data_model_beta_selection2,aes(scale(homo_motif),fruitset,color=Planta))+
#   geom_point(size=2,alpha=0.5)+facet_grid(Periodo~Bosque)
# ggplot(data_model_beta_selection2,aes(scale(hete_motif),fruitset,color=Planta))+
#   geom_point(size=2,alpha=0.5)+facet_grid(Periodo~Bosque)
# ggplot(data_model_beta_selection2,aes(scale(Visits_tot),fruitset,color=Planta))+
#   geom_point(size=2,alpha=0.5)+facet_grid(Periodo~Bosque)
# 
# ggplot(data_model_beta_selection2,aes(scale(poll_richness),fruitset,color=Planta))+
#   geom_point(size=2,alpha=0.5)+facet_grid(Periodo~Bosque)
# ggplot(data_model_beta_selection2,aes(scale(plant_richness),fruitset,color=Planta))+
#   geom_point(size=2,alpha=0.5)+facet_grid(Periodo~Bosque)
# ggplot(data_model_beta_selection2,aes(scale(total_number_flowers),fruitset,color=Planta))+
#   geom_point(size=2,alpha=0.5)+facet_grid(Periodo~Bosque)
# ggplot(data_model_beta_selection2,aes(scale(unloyal_steps),fruitset,color=Planta))+
#   geom_point(size=2,alpha=0.5)+facet_grid(Periodo~Bosque)

# El mejor de momento
fullmodel4_RD2<- glmmTMB((fruitset) ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  +
                           scale(poll_richness) + scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(prob_consp_step)+(1|Bosque/X)+(1|Bosque/Y) + Planta + Periodo,
                         family=gaussian(link="log"),
                         data_model_beta_selection)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # OK
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection$X_rd,
                           y= data_model_beta_selection$Y_rd)



fullmodel4_RD2<- glmmTMB((fruitset) ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  +
                           scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(prob_consp_step)+(1|Bosque/X)+(1|Bosque/Y) + Planta,
                         family=gaussian(link="log"),
                         data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # Bad
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection2$X_rd,
                           y= data_model_beta_selection2$Y_rd)

# El mejor
fullmodel4_RD2<- glmmTMB((fruitset)^2 ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  +
                           scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(prob_consp_step)+(1|Bosque/X)+(1|Bosque/Y) + Planta + Periodo,
                         family=gaussian(link="log"),
                         data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # OK

test_predictions <- tibble(predicted_y = exp(predict(fullmodel4_RD2)),
                           real_y = data_model_beta_selection2$fruitset,
                           Planta = data_model_beta_selection2$Planta)
ggplot(test_predictions,aes(predicted_y,real_y,color=Planta))+geom_point(size=2,alpha=0.5)+geom_abline(slope = 1,intercept = 0)
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection2$X_rd,
                           y= data_model_beta_selection2$Y_rd)

plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$Bosque, xlab = "Bosque", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$Periodo, xlab = "Periodo", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$poll_richness, xlab = "poll_richness", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$plant_richness, xlab = "plant_richness", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$homo_motif, xlab = "prop_hete", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$hete_motif, xlab = "prop_hete", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$prob_consp_step, xlab = "f(prob_consp_step)", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$total_number_flowers, xlab = "f(total_number_flowers)", main=NULL)


fullmodel4_RD2<- glmmTMB((fruitset)^2 ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  +
                           scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(log_prob_cons)+(1|Bosque/X)+(1|Bosque/Y) + Planta + Periodo,
                         family=gaussian(link="log"),
                         data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # OK
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection2$X_rd,
                           y= data_model_beta_selection2$Y_rd)


plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$Bosque, xlab = "Bosque", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$Periodo, xlab = "Periodo", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$poll_richness, xlab = "poll_richness", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$plant_richness, xlab = "plant_richness", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$homo_motif, xlab = "prop_hete", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$hete_motif, xlab = "prop_hete", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$log_prob_cons, xlab = "f(prob_consp_step)", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$total_number_flowers, xlab = "f(total_number_flowers)", main=NULL)



# El mejor
fullmodel4_RD2<- glmmTMB((fruitset)^2 ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  +
                           scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(log_prob_cons)+(1|Bosque) + Planta + Periodo,
                         family=gaussian(link="log"),
                         data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
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
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # OK
test_predictions <- tibble(predicted_y = exp(predict(fullmodel4_RD2)),
                           real_y = data_model_beta_selection2$fruitset,
                           Planta = data_model_beta_selection2$Planta)


testDispersion(res_fullmodel4_RD2) # OK


plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$Bosque, xlab = "Bosque", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$Periodo, xlab = "Periodo", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$poll_richness, xlab = "poll_richness", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$plant_richness, xlab = "plant_richness", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$homo_motif, xlab = "prop_hete", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$hete_motif, xlab = "prop_hete", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$log_prob_cons, xlab = "f(prob_consp_step)", main=NULL)
plotResiduals(res_fullmodel4_RD2, data_model_beta_selection2$total_number_flowers, xlab = "f(total_number_flowers)", main=NULL)



fullmodel4_RD2<- glmer((fruitset) ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  +
                           scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(log_prob_cons)+(1|Bosque) + Planta + Periodo,
                         family=gaussian(link="log"),
                       control = glmerControl(optimizer = "bobyqa"),
                         data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # Bad
test_predictions <- tibble(predicted_y = exp(predict(fullmodel4_RD2)),
                           real_y = data_model_beta_selection2$fruitset,
                           Planta = data_model_beta_selection2$Planta)
ggplot(test_predictions,aes(predicted_y,real_y,color=Planta))+geom_point(size=2,alpha=0.5)+geom_abline(slope = 1,intercept = 0)

testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection2$X_rd,
                           y= data_model_beta_selection2$Y_rd)


fullmodel4_RD2<- lmer((fruitset)^2 ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  +
                         scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                         scale(log_prob_cons)+(1|Bosque) + Planta + Periodo,
                       data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # Bad
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection2$X_rd,
                           y= data_model_beta_selection2$Y_rd)


fullmodel4_RD2<- lmer((fruitset)^2 ~  scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                        scale(log_prob_cons)+(1|Bosque) + Planta + Periodo,
                      data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # Bad
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection2$X_rd,
                           y= data_model_beta_selection2$Y_rd)



fullmodel4_RD2<- lmer((fruitset) ~  scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                        scale(log_prob_cons)+ 
                        I((scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                             scale(log_prob_cons))^2)+ 
                        (1|Bosque) + Planta + Periodo,
                      data_model_beta_selection2)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # Bad
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection2$X_rd,
                           y= data_model_beta_selection2$Y_rd)

