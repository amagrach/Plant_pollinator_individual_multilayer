
data_model_beta_full %>% group_by(Planta) %>% count()
plantas_selected <- c("Cistus_salviifolius", "Cistus_crispus", "Cistus_ladanifer")
data_model_beta_selection <- data_model_beta_full #%>% filter(Planta %in% plantas_selected)


fullmodel <- betareg::betareg((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                                scale(plant_richness) * scale(total_number_flowers) +
                                scale(X)+scale(Y)+
                                scale(prob_consp_step)+Bosque + Periodo+Planta, 
                              data_model_beta_selection) # 184 observations
summary(fullmodel) # Convergence problem 
performance::check_collinearity(fullmodel)

fullmodel2 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                       scale(plant_richness) * scale(total_number_flowers) +
                        scale(X)+scale(Y)+
                       scale(prob_consp_step)+Bosque + Periodo+Planta,
                     family=beta_family(link="logit"),
                     data_model_beta_selection)  # 184 observations
summary(fullmodel2)# Convergence problem 
performance::check_collinearity(fullmodel2)
res_fullmodel2 <- simulateResiduals(fittedModel = fullmodel2)
plot(res_fullmodel2) # Bad
testDispersion(res_fullmodel2) # Bad

fullmodel3 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                        scale(plant_richness) * scale(total_number_flowers) +
                        scale(X)+scale(Y)+
                        scale(prob_consp_step)+Bosque + Periodo + Planta,
                      family=gaussian(link="logit"),
                      data_model_beta_selection)  # 184 observations
summary(fullmodel3)
performance::check_collinearity(fullmodel3) # High correlation
res_fullmodel3 <- simulateResiduals(fittedModel = fullmodel3)
plot(res_fullmodel3) # Bad
testDispersion(res_fullmodel3) # OK

fullmodel4 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                        scale(plant_richness) * scale(total_number_flowers) +
                        scale(X)+scale(Y)+
                        scale(prob_consp_step)+Bosque+Periodo+Planta,
                      family=gaussian(link="log"),
                      data_model_beta_selection)  # 184 observations
summary(fullmodel4) # Convergence problem 
performance::check_collinearity(fullmodel4)
res_fullmodel4 <- simulateResiduals(fittedModel = fullmodel4)
plot(res_fullmodel4) # Bad
testDispersion(res_fullmodel4) # OK

fullmodel5 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                        scale(plant_richness) * scale(total_number_flowers) +
                        scale(X)+scale(Y)+
                        scale(prob_consp_step)+Bosque+Periodo+Planta,
                      family=gaussian(),
                      data_model_beta_selection)  # 184 observations
summary(fullmodel5)
performance::check_collinearity(fullmodel5)
res_fullmodel5 <- simulateResiduals(fittedModel = fullmodel5)
plot(res_fullmodel5) # Bad
testDispersion(res_fullmodel5) # OK


########################
# Random factors (1|Bosque:Periodo)

fullmodel2_RD1 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                        scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                        scale(prob_consp_step)+(1|Bosque:Periodo/X)+(1|Bosque:Periodo/Y)+Planta,
                      family=beta_family(link="logit"),
                      data_model_beta_selection)  # 184 observations
summary(fullmodel2_RD1)
performance::check_collinearity(fullmodel2_RD1)
performance::r2(fullmodel2_RD1) # Makes no sense
res_fullmodel2_RD1 <- simulateResiduals(fittedModel = fullmodel2_RD1)
plot(res_fullmodel2_RD1) # Bad
testDispersion(res_fullmodel2_RD1) # Bad

fullmodel3_RD1 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+ #scale(Visits_tot)  +
                        scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                        scale(prob_consp_step)+(1|Bosque:Periodo/X)+(1|Bosque:Periodo/Y) + Planta,
                      family=gaussian(link="logit"),
                      data_model_beta_selection)  # 184 observations
summary(fullmodel3_RD1) # Makes no sense: unloyal, prob consp
performance::check_collinearity(fullmodel3_RD1)
performance::r2(fullmodel3_RD1) # Variance Bosque-periodo too small
res_fullmodel3_RD1 <- simulateResiduals(fittedModel = fullmodel3_RD1)
plot(res_fullmodel3_RD1) # Bad
testDispersion(res_fullmodel3_RD1) # OK

fullmodel4_RD1 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+ #scale(Visits_tot)  +
                        scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                        scale(prob_consp_step)+(1|Bosque:Periodo/X)+(1|Bosque:Periodo/Y)+Planta,
                      family=gaussian(link="log"),
                      data_model_beta_selection)  # 184 observations
summary(fullmodel4_RD1)
performance::check_collinearity(fullmodel4_RD1)
performance::r2(fullmodel4_RD1)
res_fullmodel4_RD1 <- simulateResiduals(fittedModel = fullmodel4_RD1)
plot(res_fullmodel4_RD1) # Bad
testDispersion(res_fullmodel4_RD1) # OK


fullmodel5_RD1 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+ #scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Bosque:Periodo/X)+(1|Bosque:Periodo/Y)+Planta,
                          family=gaussian(),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel5_RD1)
performance::check_collinearity(fullmodel5_RD1)
performance::r2(fullmodel5_RD1)
res_fullmodel5_RD1 <- simulateResiduals(fittedModel = fullmodel5_RD1)
plot(res_fullmodel5_RD1) # Bad
testDispersion(res_fullmodel5_RD1) # OK

########################
# Random factors (1|Bosque)

fullmodel2_RD2<- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Bosque/X)+(1|Bosque/Y)+Planta,
                          family=beta_family(link="logit"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel2_RD2)
performance::check_collinearity(fullmodel2_RD2)
performance::r2(fullmodel2_RD2) # Makes no sense
res_fullmodel2_RD2<- simulateResiduals(fittedModel = fullmodel2_RD2)
plot(res_fullmodel2_RD2) # Bad
testDispersion(res_fullmodel2_RD2) # Bad

fullmodel3_RD2<- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) + scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Bosque/X)+(1|Bosque/Y) + Planta ,
                          family=gaussian(link="logit"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel3_RD2)
performance::check_collinearity(fullmodel3_RD2) # High correlation
performance::r2(fullmodel3_RD2) # Variance Bosque-periodo too small
res_fullmodel3_RD2<- simulateResiduals(fittedModel = fullmodel3_RD2)
plot(res_fullmodel3_RD2) # Bad
testDispersion(res_fullmodel3_RD2) # OK


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
plot(res_fullmodel4_RD2) # Bad
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection$X_rd,
                           y= data_model_beta_selection$Y_rd)

fullmodel4_RD2<- glmmTMB((fruitset) ~  scale(homo_motif) * scale(hete_motif) + scale(Visits_tot)  +
                           scale(poll_richness) + scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Bosque/X)+(1|Bosque/Y) + Planta + Periodo,
                          family=gaussian(link="log"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # Bad
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection$X_rd,
                           y= data_model_beta_selection$Y_rd)


fullmodel4_RD2<- glmmTMB((fruitset) ~  scale(homo_motif) * scale(hete_motif) + scale(Visits_tot)  +
                           scale(poll_richness) + scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(prob_consp_step)+(X|Bosque)+(Y|Bosque) + Planta + Periodo,
                         family=gaussian(link="log"),
                         data_model_beta_selection)  # 184 observations
summary(fullmodel4_RD2)
performance::check_collinearity(fullmodel4_RD2)
performance::r2(fullmodel4_RD2)
res_fullmodel4_RD2<- simulateResiduals(fittedModel = fullmodel4_RD2)
plot(res_fullmodel4_RD2) # Bad
testDispersion(res_fullmodel4_RD2) # OK
testSpatialAutocorrelation(simulationOutput =res_fullmodel4_RD2,
                           x = data_model_beta_selection$X_rd,
                           y= data_model_beta_selection$Y_rd)


fullmodel5_RD2<- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+ (1|Bosque/X)+(1|Bosque/Y)+Planta + Periodo,
                          family=gaussian(),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel5_RD2)
performance::check_collinearity(fullmodel5_RD2)
performance::r2(fullmodel5_RD2)
res_fullmodel5_RD2 <- simulateResiduals(fittedModel = fullmodel5_RD2)
plot(res_fullmodel5_RD2) # Bad
testDispersion(res_fullmodel5_RD2) # OK


########################
# Random factors (1|Periodo)

fullmodel2_RD3 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Periodo)+(1|Planta/Bosque/X)+(1|Planta/Bosque/Y),
                          family=beta_family(link="logit"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel2_RD3)
performance::check_collinearity(fullmodel2_RD3)
performance::r2(fullmodel2_RD3) # Makes no sense
res_fullmodel2_RD3 <- simulateResiduals(fittedModel = fullmodel2_RD3)
plot(res_fullmodel2_RD3) # Bad
testDispersion(res_fullmodel2_RD3) # Bad

fullmodel3_RD3 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Periodo) + (1|Planta/Bosque/X)+(1|Planta/Bosque/Y),
                          family=gaussian(link="logit"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel3_RD3)
performance::check_collinearity(fullmodel3_RD3)
performance::r2(fullmodel3_RD3) # Variance Bosque-periodo too small
res_fullmodel3_RD3 <- simulateResiduals(fittedModel = fullmodel3_RD3)
plot(res_fullmodel3_RD3) # Bad
testDispersion(res_fullmodel3_RD3) # OK

fullmodel4_RD3 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Periodo)+(1|Planta/Bosque/X)+(1|Planta/Bosque/Y),
                          family=gaussian(link="log"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel4_RD3)
performance::check_collinearity(fullmodel4_RD3)
performance::r2(fullmodel4_RD3)
res_fullmodel4_RD3 <- simulateResiduals(fittedModel = fullmodel4_RD3)
plot(res_fullmodel4_RD3) # Bad
testDispersion(res_fullmodel4_RD3) # OK


fullmodel5_RD3 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Periodo)+(1|Planta/Bosque/X)+(1|Planta/Bosque/Y),
                          family=gaussian(),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel5_RD3)
performance::check_collinearity(fullmodel5_RD3)
performance::r2(fullmodel5_RD3)
res_fullmodel5_RD3 <- simulateResiduals(fittedModel = fullmodel5_RD3)
plot(res_fullmodel5_RD3) # Bad
testDispersion(res_fullmodel5_RD3) # OK

########################
# Random factors (1|Planta/Bosque:Periodo)

fullmodel2_RD4 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Planta/Bosque:Periodo),
                          family=beta_family(link="logit"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel2_RD4)
performance::check_collinearity(fullmodel2_RD4)
performance::r2(fullmodel2_RD4) # Makes no sense
res_fullmodel2_RD4 <- simulateResiduals(fittedModel = fullmodel2_RD4)
plot(res_fullmodel2_RD4) # Bad
testDispersion(res_fullmodel2_RD4) # OK

fullmodel3_RD4 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Planta/Bosque:Periodo),
                          family=gaussian(link="logit"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel3_RD4) # Not converged
performance::check_collinearity(fullmodel3_RD4) # high correlation
performance::r2(fullmodel3_RD4) # Variance Bosque-periodo too small
res_fullmodel3_RD4 <- simulateResiduals(fittedModel = fullmodel3_RD4)
plot(res_fullmodel3_RD4) # Bad
testDispersion(res_fullmodel3_RD4) # OK

fullmodel4_RD4 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Planta/Bosque:Periodo),
                          family=gaussian(link="log"),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel4_RD4)
performance::check_collinearity(fullmodel4_RD4)
performance::r2(fullmodel4_RD4)
res_fullmodel4_RD4 <- simulateResiduals(fittedModel = fullmodel4_RD4)
plot(res_fullmodel4_RD4) # Bad
testDispersion(res_fullmodel4_RD4) # OK


fullmodel5_RD4 <- glmmTMB((fruitset) ~ scale(poll_richness) + scale(homo_motif) * scale(hete_motif)+  scale(Visits_tot)  +
                            scale(plant_richness) * scale(total_number_flowers) + scale(unloyal_steps) +
                            scale(prob_consp_step)+(1|Planta/Bosque:Periodo),
                          family=gaussian(),
                          data_model_beta_selection)  # 184 observations
summary(fullmodel5_RD4)
performance::check_collinearity(fullmodel5_RD4)
performance::r2(fullmodel5_RD4)
res_fullmodel5_RD4 <- simulateResiduals(fittedModel = fullmodel5_RD4)
plot(res_fullmodel5_RD4) # Bad
testDispersion(res_fullmodel5_RD4) # OK

