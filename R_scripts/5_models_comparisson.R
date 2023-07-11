
# This code run conditional logistic regression models by using the observed 
# steps and the corresponding rd steps

# IMPORTANT: Since some XY combinations apparently do not have the species
# obsered in a given period (floral census). For that reason, the rd step info
# on the following variables is not updated: flowers_sp1_XY2,flowers_sp2_XY1,
# flowers_sp2_XY2, deltaflowers_sp1, deltaflowers_sp2. So do not use it to
# model

library(tidyverse)
library(survival)
library(ggallin) # to use pseudolog10_trans in plots
library(car)

source("R/pollinator_model_exp_coef.R")
source("R/set_same_factor_levels_no_flower_data.R")

number_random_steps <- 20

data_path_file <- paste0("Processed_files/total_pollinator_i_data_clogit_observations_",
                         number_random_steps,"_rd_steps_NEW.csv")

total_pollinator_i_data_clogit <- read_csv(data_path_file) # %>% filter(Periodo<3)
ranking_pollinators <- total_pollinator_i_data_clogit %>% group_by(Polinizador) %>% count() %>% arrange(desc(n))

# Estimation of coeff.

main_pollinator_coef <- NULL
set.seed(1234)

for(pollinator_i in ranking_pollinators$Polinizador[1:5]) {
  
  print(pollinator_i)
  
  pollinator_i_data_clogit <- total_pollinator_i_data_clogit %>% 
    filter(Polinizador == pollinator_i)
  
  pollinator_i_data_clogit_within_field <- pollinator_i_data_clogit
  
  pollinator_i_data_clogit_mod <- set_same_factor_levels_no_flower_data(pollinator_i_data_clogit_within_field) %>%
    mutate(cosine_turning = cos(turning_angle*pi/180))
  
  pollinator_i_data_clogit_mod$Periodo <- as.factor(pollinator_i_data_clogit_mod$Periodo)
  
  pollinator_i_data_clogit_mod$step_length <- scale(pollinator_i_data_clogit_mod$step_length)
  pollinator_i_data_clogit_mod$delta_richness <- scale(pollinator_i_data_clogit_mod$delta_richness)
  pollinator_i_data_clogit_mod$delta_total_flowers <- scale(pollinator_i_data_clogit_mod$delta_total_flowers)
  pollinator_i_data_clogit_mod$cosine_turning <- scale(pollinator_i_data_clogit_mod$cosine_turning)
  
  model_pollinator_i_mean <- clogit(control ~ step_length + 
                                 delta_richness +
                                 delta_total_flowers +
                                 cosine_turning +
                                 strata(step_ID),
                               method = "exact",
                               pollinator_i_data_clogit_mod)
  
  
  model_pollinator_i_period_1 <- clogit(control ~ step_length + 
                                      delta_richness +
                                      delta_total_flowers +
                                      cosine_turning +
                                      strata(step_ID),
                                    method = "exact",
                                    pollinator_i_data_clogit_mod %>%
                                      filter(Periodo==1))
  
  model_pollinator_i_period_1_time_day_1 <- clogit(control ~ step_length + 
                                          delta_richness +
                                          delta_total_flowers +
                                          cosine_turning +
                                          strata(step_ID),
                                        method = "exact",
                                        pollinator_i_data_clogit_mod %>%
                                          filter(Periodo==1,Periodo_hora==1))
  
  
  model_pollinator_i_period_1_time_day_2 <- clogit(control ~ step_length + 
                                          delta_richness +
                                          delta_total_flowers +
                                          cosine_turning +
                                          strata(step_ID),
                                        method = "exact",
                                        pollinator_i_data_clogit_mod %>%
                                          filter(Periodo==1,Periodo_hora==2))
  
  model_pollinator_i_period_1_time_day_3 <- clogit(control ~ step_length + 
                                          delta_richness +
                                          delta_total_flowers +
                                          cosine_turning +
                                          strata(step_ID),
                                        method = "exact",
                                        pollinator_i_data_clogit_mod %>%
                                          filter(Periodo==1,Periodo_hora==3))
  
  model_pollinator_i_period_2 <- clogit(control ~ step_length + 
                                          delta_richness +
                                          delta_total_flowers +
                                          cosine_turning +
                                          strata(step_ID),
                                        method = "exact",
                                        pollinator_i_data_clogit_mod %>%
                                          filter(Periodo==2))
  
  model_pollinator_i_period_2_time_day_1 <- clogit(control ~ step_length + 
                                                     delta_richness +
                                                     delta_total_flowers +
                                                     cosine_turning +
                                                     strata(step_ID),
                                                   method = "exact",
                                                   pollinator_i_data_clogit_mod %>%
                                                     filter(Periodo==2,Periodo_hora==1))
  
  
  model_pollinator_i_period_2_time_day_2 <- clogit(control ~ step_length + 
                                                     delta_richness +
                                                     delta_total_flowers +
                                                     cosine_turning +
                                                     strata(step_ID),
                                                   method = "exact",
                                                   pollinator_i_data_clogit_mod %>%
                                                     filter(Periodo==2,Periodo_hora==2))
  
  model_pollinator_i_period_2_time_day_3 <- clogit(control ~ step_length + 
                                                     delta_richness +
                                                     delta_total_flowers +
                                                     cosine_turning +
                                                     strata(step_ID),
                                                   method = "exact",
                                                   pollinator_i_data_clogit_mod %>%
                                                     filter(Periodo==2,Periodo_hora==3))
  
  model_pollinator_i_period_3 <- clogit(control ~ step_length + 
                                          delta_richness +
                                          delta_total_flowers +
                                          cosine_turning +
                                          strata(step_ID),
                                        method = "exact",
                                        pollinator_i_data_clogit_mod %>%
                                          filter(Periodo==3))
  
  model_pollinator_i_period_3_time_day_1 <- clogit(control ~ step_length + 
                                                     delta_richness +
                                                     delta_total_flowers +
                                                     cosine_turning +
                                                     strata(step_ID),
                                                   method = "exact",
                                                   pollinator_i_data_clogit_mod %>%
                                                     filter(Periodo==3,Periodo_hora==1))
  
  
  model_pollinator_i_period_3_time_day_2 <- clogit(control ~ step_length + 
                                                     delta_richness +
                                                     delta_total_flowers +
                                                     cosine_turning +
                                                     strata(step_ID),
                                                   method = "exact",
                                                   pollinator_i_data_clogit_mod %>%
                                                     filter(Periodo==3,Periodo_hora==2))
  
  if(nrow(pollinator_i_data_clogit_mod %>%
          filter(Periodo==3,Periodo_hora==3))>0){
    
    model_pollinator_i_period_3_time_day_3 <- clogit(control ~ step_length + 
                                                       delta_richness +
                                                       delta_total_flowers +
                                                       cosine_turning +
                                                       strata(step_ID),
                                                     method = "exact",
                                                     pollinator_i_data_clogit_mod %>%
                                                       filter(Periodo==3,Periodo_hora==3))
    pollinator_i_coef_period_3_time_day_3 <- broom::tidy(model_pollinator_i_period_2_time_day_3) %>% 
      mutate(pollinator = pollinator_i, model = "Period 3, tod 3")
    
    
  }
  
  
  
  
  model_pollinator_i_time_day_1 <- clogit(control ~ step_length + 
                                          delta_richness +
                                          delta_total_flowers +
                                          cosine_turning +
                                          strata(step_ID),
                                        method = "exact",
                                        pollinator_i_data_clogit_mod %>%
                                          filter(Periodo_hora==1))
  
  model_pollinator_i_time_day_2 <- clogit(control ~ step_length + 
                                          delta_richness +
                                          delta_total_flowers +
                                          cosine_turning +
                                          strata(step_ID),
                                        method = "exact",
                                        pollinator_i_data_clogit_mod %>%
                                          filter(Periodo_hora==2))
  
  model_pollinator_i_time_day_3 <- clogit(control ~ step_length + 
                                          delta_richness +
                                          delta_total_flowers +
                                          cosine_turning +
                                          strata(step_ID),
                                        method = "exact",
                                        pollinator_i_data_clogit_mod %>%
                                          filter(Periodo_hora==3))
  
  pollinator_i_coef_mean <- broom::tidy(model_pollinator_i_mean) %>% mutate(pollinator = pollinator_i,
                                                                            model = "All data")
  
  pollinator_i_coef_period_1 <- broom::tidy(model_pollinator_i_period_1) %>% 
    mutate(pollinator = pollinator_i, model = "Period 1")
  pollinator_i_coef_period_2 <- broom::tidy(model_pollinator_i_period_2) %>%
    mutate(pollinator = pollinator_i, model = "Period 2")
  pollinator_i_coef_period_3 <- broom::tidy(model_pollinator_i_period_3) %>% 
    mutate(pollinator = pollinator_i, model = "Period 3")
  
  pollinator_i_coef_period_1_time_day_1 <- broom::tidy(model_pollinator_i_period_1_time_day_1) %>% 
    mutate(pollinator = pollinator_i, model = "Period 1, tod 1")
  pollinator_i_coef_period_1_time_day_2 <- broom::tidy(model_pollinator_i_period_1_time_day_2) %>% 
    mutate(pollinator = pollinator_i, model = "Period 1, tod 2")
  pollinator_i_coef_period_1_time_day_3 <- broom::tidy(model_pollinator_i_period_1_time_day_3) %>% 
    mutate(pollinator = pollinator_i, model = "Period 1, tod 3")
  
  pollinator_i_coef_period_2_time_day_1 <- broom::tidy(model_pollinator_i_period_2_time_day_1) %>% 
    mutate(pollinator = pollinator_i, model = "Period 2, tod 1")
  pollinator_i_coef_period_2_time_day_2 <- broom::tidy(model_pollinator_i_period_2_time_day_2) %>% 
    mutate(pollinator = pollinator_i, model = "Period 2, tod 2")
  pollinator_i_coef_period_2_time_day_3 <- broom::tidy(model_pollinator_i_period_2_time_day_3) %>% 
    mutate(pollinator = pollinator_i, model = "Period 2, tod 3")
  
  
  pollinator_i_coef_period_3_time_day_1 <- broom::tidy(model_pollinator_i_period_3_time_day_1) %>% 
    mutate(pollinator = pollinator_i, model = "Period 3, tod 1")
  pollinator_i_coef_period_3_time_day_2 <- broom::tidy(model_pollinator_i_period_3_time_day_2) %>% 
    mutate(pollinator = pollinator_i, model = "Period 3, tod 2")
  

  
  pollinator_i_coef_time_day_1 <- broom::tidy(model_pollinator_i_time_day_1) %>% 
    mutate(pollinator = pollinator_i, model = "Time of day 1")
  pollinator_i_coef_time_day_2 <- broom::tidy(model_pollinator_i_time_day_2) %>% 
    mutate(pollinator = pollinator_i, model = "Time of day 2")
  pollinator_i_coef_time_day_3 <- broom::tidy(model_pollinator_i_time_day_3) %>% 
    mutate(pollinator = pollinator_i, model = "Time of day 3")
  
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_mean)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_1)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_2)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_3)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_time_day_1)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_time_day_2)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_time_day_3)
  
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_1_time_day_1)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_1_time_day_2)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_1_time_day_3)
  
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_2_time_day_1)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_2_time_day_2)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_2_time_day_3)
  
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_3_time_day_1)
  main_pollinator_coef <- bind_rows(main_pollinator_coef, pollinator_i_coef_period_3_time_day_2)
  
                                    
  
}



main_pollinator_coef$term %>% unique()


coefficient_names <- c(
  `step_length` = "step length",
  `cosine_turning` = "cosine of\nturning angle",
  `step_length:time_of_day12:40 - 15:19` = "step length:12:40 - 15:19",
  `step_length:time_of_day15:20 - 18:05` = "step length:15:20 - 18:05",
  `step_length:plotPlot 2` = "step length:Plot 2",
  `step_length:plotPlot 3` = "step length:Plot 3",
  `step_length:plotPlot 4` = "step length:Plot 4",
  `step_length:plotPlot 5` = "step length:Plot 5",
  `step_length:Year2021` = "step length:2021",
  `step_length:change_plant_spTRUE` = "step length:unloyal\n(floral fidelity)",
  `step_length:cosine_turning` = "step length:cosine of\nturning angle",
  `step_length:Periodo2` = "step length:May-June",
  `step_length:Periodo3` = "step length:July",
  `cosine_turning:time_of_day12:40 - 15:19` = "cosine of\nturning angle:12:40 - 15:19",
  `cosine_turning:time_of_day15:20 - 18:05` = "cosine of\nturning angle:15:20 - 18:05",
  `cosine_turning:plotPlot 2` = "cosine of\nturning angle:Plot 2",
  `cosine_turning:plotPlot 3` = "cosine of\nturning angle:Plot 3",
  `cosine_turning:plotPlot 4` = "cosine of\nturning angle:Plot 4",
  `cosine_turning:plotPlot 5` = "cosine of\nturning angle:Plot 5",
  `cosine_turning:Year2021` = "cosine of\nturning angle:2021",
  `cosine_turning:change_plant_spTRUE` = "cosine of\nturning angle:change plant sp.",
  `cosine_turning:Periodo2` = "cosine of\nturning angle:May-June",
  `cosine_turning:Periodo3` = "cosine of\nturning angle:July",
  `delta_richness` = "change in sp. richness",
  `delta_total_flowers` = "change in the total\n# flowers",
  `step_length:delta_richness` = "step length:change\nin sp. richness",
  `step_length:delta_total_flowers` = "step length:change in\nthe total # flowers",
  `delta_richness:delta_total_flowers` = "change in sp. richness:\nchange in the\ntotal # flowers"
)

main_pollinator_coef$pollinator <- gsub("_", " ", main_pollinator_coef$pollinator)

main_pollinator_coef$shapes <- "0.1 < p-value"
main_pollinator_coef$shapes[main_pollinator_coef$p.value <= 0.1] <- "0.05 < p-value < 0.1"
main_pollinator_coef$shapes[main_pollinator_coef$p.value <= 0.05] <- "p-value < 0.05"

main_pollinator_coef$shapes <- factor(main_pollinator_coef$shapes, levels=c("p-value < 0.05","0.05 < p-value < 0.1","0.1 < p-value"), ordered = TRUE)

bombus_pasc_plot <- ggplot(main_pollinator_coef %>% filter(pollinator == "Bombus pascuorum"), aes(y=model))+
  geom_point(aes(x = estimate, color = as.factor(model), shape = shapes),size=2)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error,color = as.factor(model)), width=.2)+
  geom_vline(xintercept = 0)+
  facet_wrap(vars(term), ncol = 5, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans)+
  theme_bw()+theme(plot.title = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none")+
  labs(title="Bombus pascuorum", x = "Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)


apis_mel_plot <- ggplot(main_pollinator_coef %>% filter(pollinator == "Apis mellifera"), aes(y=model))+
  geom_point(aes(x = estimate, color = as.factor(model), shape = shapes),size=2)+
  scale_shape_manual(values = c(16,17,15),
                     labels = c("p-value < 0.05", " 0.05 < p-value < 0.1", "0.1 < p-value"),
                     drop = FALSE)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error,color = as.factor(model)), width=.2)+
  geom_vline(xintercept = 0)+
  facet_wrap(vars(term), ncol = 5, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans)+
  theme_bw()+theme(plot.title = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none", shape = "none")+
  labs(title="Apis mellifera", x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)


sphaero_scri_plot <- ggplot(main_pollinator_coef %>% filter(pollinator == "Sphaerophoria scripta"), aes(y=model))+
  geom_point(aes(x = estimate, color = as.factor(model), shape = shapes),size=2)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error,color = as.factor(model)), width=.2)+
  geom_vline(xintercept = 0)+
  facet_wrap(vars(term), ncol = 5, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans)+
  theme_bw()+theme(plot.title = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none")+
  labs(title="Sphaerophoria scripta", x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)

bombus_lapi_plot <- ggplot(main_pollinator_coef %>% filter(pollinator == "Bombus lapidarius"), aes(y=model))+
  geom_point(aes(x = estimate, color = as.factor(model), shape = shapes),size=2)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error,color = as.factor(model)), width=.2)+
  geom_vline(xintercept = 0)+
  facet_wrap(vars(term), ncol = 5, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans)+
  theme_bw()+theme(plot.title = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none")+
  labs(title="Bombus lapidarius", x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)

eristalis_plot <- ggplot(main_pollinator_coef %>% filter(pollinator == "Eristalis sp"), aes(y=model))+
  geom_point(aes(x = estimate, color = as.factor(model), shape = shapes),size=2)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error,color = as.factor(model)), width=.2)+
  geom_vline(xintercept = 0)+
  facet_wrap(vars(term), ncol = 5, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans)+
  theme_bw()+theme(plot.title = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none")+
  labs(title="Eristalis sp.", x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)

library(patchwork)
(bombus_pasc_plot / apis_mel_plot / sphaero_scri_plot / bombus_lapi_plot / eristalis_plot) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

png("Images/clogit_model_comparison.png",
    width = 11.69*0.6, # The width of the plot in inches
    height = 11.69, units = "in", res=300*2)

(bombus_pasc_plot / apis_mel_plot / sphaero_scri_plot / bombus_lapi_plot / eristalis_plot) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

dev.off()
