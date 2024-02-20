

library(tidyverse)
library(ggallin) # to use pseudolog10_trans in plots
library(scales)

################################################################################
# Gorbeia plots

number_random_steps <- 20

# Plot results
path_observations_gorbeia <- paste0("results/gorbea/pollinator_floral_coef_observations_",
                            number_random_steps,"_rd_steps_UPDATED_RANDOM_FIDELITY.csv")
coef_observations_gorbeia <- read_csv(path_observations_gorbeia)

coef_observations_gorbeia$term %>% unique()


coefficient_names <- c(
  `step_length` = "Step length",
  `cosine_turning` = "cosine of\nturning angle",
  `step_length:time_of_day12:00 - 13:59` = "step length:12:00 - 13:59",
  `step_length:time_of_day14:00 - 16:05` = "step length:14:00 - 16:05",
  `step_length:plotPlot 2` = "step length:Plot 2",
  `step_length:plotPlot 3` = "step length:Plot 3",
  `step_length:plotPlot 4` = "step length:Plot 4",
  `step_length:plotPlot 5` = "step length:Plot 5",
  `step_length:Year2021` = "step length:2021",
  `step_length:change_plant_spTRUE` = "step length:unloyal\n(floral fidelity)",
  `step_length:cosine_turning` = "step length:cosine of\nturning angle",
  `step_length:Periodo2` = "step length:May-June",
  `step_length:Periodo3` = "step length:July",
  `delta_richness` = "Change in sp. richness",
  `delta_total_flowers` = "Change in the total\nnumber of flowers",
  `step_length:delta_richness` = "step length:change\nin sp. richness",
  `step_length:delta_total_flowers` = "step length:change in\nthe total number of flowers",
  `delta_richness:delta_total_flowers` = "change in sp. richness:\nchange in the\ntotal number of flowers",
  `step_length:time_of_day12:00 - 13:59` = "time of day\n12:00 - 13:59",
  `step_length:time_of_day14:00 - 16:05` = "time of day\n14:00 - 16:05",
  `time_of_day15:20 - 18:05` = "time of day\n15:20 - 18:05",
  `BosquePinar Hinojos` = "Hinojos",
  `BosquePinar Puebla` = "Puebla",
  `BosquePinar Villamanrique Este (Chaparral)` = "Villamanrique Este",
  `BosquePinar Villamanrique Sur` = "Villamanrique Sur",
  `step_length:BosquePinar Hinojos` = "step length:Hinojos",
  `step_length:BosquePinar Puebla` = "step length:Puebla",
  `step_length:BosquePinar Villamanrique Este (Chaparral)` = "step length:Villamanrique Este",
  `step_length:BosquePinar Villamanrique Sur` = "step length:Villamanrique Sur",
  change_plant_spTRUE = "Unloyal movement",
  `delta_richness:change_plant_spTRUE` = "change in sp. richness:unloyal\n(floral fidelity)",
  `delta_total_flowers:change_plant_spTRUE` = "change in the total\nnumber of flowers:unloyal (floral fidelity)",
  `log_sl` = "Ln(step length)"
  
)

coef_observations_gorbeia$pollinator <- gsub("_", " ", coef_observations_gorbeia$pollinator)

coef_observations_gorbeia$shapes <- "0.1 < p-value"
coef_observations_gorbeia$shapes[coef_observations_gorbeia$p.value <= 0.1] <- "0.05 < p-value < 0.1"
coef_observations_gorbeia$shapes[coef_observations_gorbeia$p.value <= 0.05] <- "p-value < 0.05"

coef_observations_gorbeia$shapes_form <- 0
coef_observations_gorbeia$shapes_form[coef_observations_gorbeia$p.value <= 0.1] <- 2
coef_observations_gorbeia$shapes_form[coef_observations_gorbeia$p.value <= 0.05] <- 1

coef_observations_gorbeia$shapes <- factor(coef_observations_gorbeia$shapes, levels=c("p-value < 0.05","0.05 < p-value < 0.1","0.1 < p-value"), ordered = TRUE)

# coef_observations_gorbeia$pollinator <- factor(coef_observations_gorbeia$pollinator, levels=c("Apis mellifera","Bombus terrestris","Anthophora dispar",
#                                                                               "Dasypoda cingulata","Xylocopa cantabrita"), ordered = TRUE)


################################################
# DOÑANA

number_random_steps <- 20

# Plot results
path_observations <- paste0("results/donana/pollinator_floral_coef_observations_",
                            number_random_steps,"_rd_steps_UPDATED_RANDOM_FIDELITY.csv")
coef_observations <- read_csv(path_observations)

coef_observations$term %>% unique()


coefficient_names <- c(
  `step_length` = "Step length",
  `cosine_turning` = "cosine of\nturning angle",
  `step_length:time_of_day12:00 - 13:59` = "step length:12:00 - 13:59",
  `step_length:time_of_day14:00 - 16:05` = "step length:14:00 - 16:05",
  `step_length:plotPlot 2` = "step length:Plot 2",
  `step_length:plotPlot 3` = "step length:Plot 3",
  `step_length:plotPlot 4` = "step length:Plot 4",
  `step_length:plotPlot 5` = "step length:Plot 5",
  `step_length:Year2021` = "step length:2021",
  `step_length:change_plant_spTRUE` = "step length:unloyal\n(floral fidelity)",
  `step_length:cosine_turning` = "step length:cosine of\nturning angle",
  `step_length:Periodo2` = "step length:May-June",
  `step_length:Periodo3` = "step length:July",
  `delta_richness` = "Change in sp. richness",
  `delta_total_flowers` = "Change in the total\nnumber of flowers",
  `step_length:delta_richness` = "step length:change\nin sp. richness",
  `step_length:delta_total_flowers` = "step length:change in\nthe total number of flowers",
  `delta_richness:delta_total_flowers` = "change in sp. richness:\nchange in the\ntotal # flowers",
  `step_length:time_of_day12:00 - 13:59` = "time of day\n12:00 - 13:59",
  `step_length:time_of_day14:00 - 16:05` = "time of day\n14:00 - 16:05",
  `time_of_day15:20 - 18:05` = "time of day\n15:20 - 18:05",
  `BosquePinar Hinojos` = "Hinojos",
  `BosquePinar Puebla` = "Puebla",
  `BosquePinar Villamanrique Este (Chaparral)` = "Villamanrique Este",
  `BosquePinar Villamanrique Sur` = "Villamanrique Sur",
  `step_length:BosquePinar Hinojos` = "step length:Hinojos",
  `step_length:BosquePinar Puebla` = "step length:Puebla",
  `step_length:BosquePinar Villamanrique Este (Chaparral)` = "step length:Villamanrique Este",
  `step_length:BosquePinar Villamanrique Sur` = "step length:Villamanrique Sur",
  change_plant_spTRUE = "Unloyal movement",
  `delta_richness:change_plant_spTRUE` = "change in sp. richness:unloyal\n(floral fidelity)",
  `delta_total_flowers:change_plant_spTRUE` = "change in the total\nnumber of flowers:unloyal (floral fidelity)",
  `log_sl` = "Ln(step length)"
  
)

coef_observations$pollinator <- gsub("_", " ", coef_observations$pollinator)

coef_observations$shapes <- "0.1 < p-value"
coef_observations$shapes[coef_observations$p.value <= 0.1] <- "0.05 < p-value < 0.1"
coef_observations$shapes[coef_observations$p.value <= 0.05] <- "p-value < 0.05"

coef_observations$shapes_form <- 0
coef_observations$shapes_form[coef_observations$p.value <= 0.1] <- 2
coef_observations$shapes_form[coef_observations$p.value <= 0.05] <- 1

coef_observations$shapes <- factor(coef_observations$shapes, levels=c("p-value < 0.05","0.05 < p-value < 0.1","0.1 < p-value"), ordered = TRUE)

coef_observations$pollinator <- factor(coef_observations$pollinator, levels=c("Apis mellifera","Bombus terrestris","Anthophora dispar",
                                                                              "Dasypoda cingulata","Xylocopa cantabrita"), ordered = TRUE)


ggplot(coef_observations, aes(y=pollinator))+
  geom_point(aes(x = estimate, shape = shapes),size=2)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 5, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans)+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none")+
  labs(title=NULL, x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)

coef_observations$plot_order <- 1
coef_observations$plot_order[coef_observations$pollinator=="Bombus terrestris"] <- 2
coef_observations$plot_order[coef_observations$pollinator=="Anthophora dispar"] <- 3
coef_observations$plot_order[coef_observations$pollinator=="Dasypoda cingulata"] <- 4
coef_observations$plot_order[coef_observations$pollinator=="Xylocopa cantabrita"] <- 5


################################################
################################################
################################################


gorbeia_loyal <- ggplot(coef_observations_gorbeia %>% filter(term=="change_plant_spTRUE"), aes(y=pollinator))+
  geom_point(aes(x = estimate, y = pollinator, shape = shapes),size=5)+
  scale_shape(solid = FALSE)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 1, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans,breaks = breaks_pretty(3))+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none", shape="none")+
  labs(title="Meadows (Gorbeia N.P.)", x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)+
  theme(legend.text = element_text(size=18))+
  theme(axis.text=element_text(size=18),  axis.title=element_text(size=19,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 19))+
  theme(plot.title = element_text(size = 20))


donana_loyal <- ggplot(coef_observations %>% filter(term=="change_plant_spTRUE"), aes(y=pollinator))+
  geom_point(aes(x = estimate, y = pollinator, shape = shapes),size=5)+
  scale_shape(solid = FALSE)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 1, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans,breaks = breaks_pretty(3))+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none")+
  labs(title="Pine Woodlands (Doñana N.P.)", x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)+
  theme(legend.text = element_text(size=18))+
  theme(axis.text=element_text(size=18),  axis.title=element_text(size=19,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 19))+
  theme(plot.title = element_text(size = 20))

# Species richness

gorbeia_richness <- ggplot(coef_observations_gorbeia %>% filter(term=="delta_richness"), aes(y=pollinator))+
  geom_point(aes(x = estimate, y = pollinator, shape = shapes),size=5)+
  scale_shape_manual(values = c(1,0,0))+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 1, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans,breaks = breaks_pretty(3))+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none", shape="none")+
  labs(x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)+
  theme(legend.text = element_text(size=18))+
  theme(axis.text=element_text(size=18),  axis.title=element_text(size=19,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 19))+
  theme(plot.title = element_text(size = 20))



donana_richness <- ggplot(coef_observations %>% filter(term=="delta_richness"), aes(y=pollinator))+
  geom_point(aes(x = estimate, y = pollinator, shape = shapes),size=5)+
  scale_shape(solid = FALSE)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 1, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans,breaks = breaks_pretty(3))+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none", shape="none")+
  labs(x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)+
  theme(legend.text = element_text(size=18))+
  theme(axis.text=element_text(size=18),  axis.title=element_text(size=19,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 19))+
  theme(plot.title = element_text(size = 20))

# Abundance

gorbeia_abundance <- ggplot(coef_observations_gorbeia %>% filter(term=="delta_total_flowers"), aes(y=pollinator))+
  geom_point(aes(x = estimate, y = pollinator, shape = shapes),size=5)+
  scale_shape_manual(values = c(1,2,0))+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 1, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans,breaks = breaks_pretty(3))+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none", shape="none")+
  labs(x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)+
  theme(legend.text = element_text(size=18))+
  theme(axis.text=element_text(size=18),  axis.title=element_text(size=19,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 19))+
  theme(plot.title = element_text(size = 20))


donana_abundance <- ggplot(coef_observations %>% filter(term=="delta_total_flowers"), aes(y=pollinator))+
  geom_point(aes(x = estimate, y = pollinator, shape = shapes),size=5)+
  scale_shape_manual(values = c(1,0,0))+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 1, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans,breaks = breaks_pretty(3))+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none", shape="none")+
  labs(x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)+
  theme(legend.text = element_text(size=18))+
  theme(axis.text=element_text(size=18),  axis.title=element_text(size=19,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 19))+
  theme(plot.title = element_text(size = 20))

# step-length

gorbeia_step <- ggplot(coef_observations_gorbeia %>% filter(term=="step_length"), aes(y=pollinator))+
  geom_point(aes(x = estimate, y = pollinator, shape = shapes),size=5)+
  scale_shape_manual(values = c(1,2,0))+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 1, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans,breaks = breaks_pretty(3))+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none", shape="none")+
  labs(x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)+
  theme(legend.text = element_text(size=18))+
  theme(axis.text=element_text(size=18),  axis.title=element_text(size=19,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 19))+
  theme(plot.title = element_text(size = 20))


donana_step <- ggplot(coef_observations %>% filter(term=="step_length"), aes(y=pollinator))+
  geom_point(aes(x = estimate, y = pollinator, shape = shapes),size=5)+
  scale_shape(solid = FALSE)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 1, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans,breaks = breaks_pretty(3))+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none", shape="none")+
  labs(x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)+
  theme(legend.text = element_text(size=18))+
  theme(axis.text=element_text(size=18),  axis.title=element_text(size=19,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 19))+
  theme(plot.title = element_text(size = 20))


library(patchwork)

png("figures/TOTAL_clogit_floral_coef_observed_distributions_UPDATED_RANDOM_FIDELITY.png",
    width = 11.69*.55*2, # The width of the plot in inches
    height = 11.69*600/450, units = "in", res=300*2)

(((gorbeia_loyal+donana_loyal)/(gorbeia_richness+donana_richness)/(gorbeia_abundance+donana_abundance)/(gorbeia_step+ donana_step)) & theme(legend.position = "bottom",plot.tag = element_text(face = 'bold', size=20), plot.tag.position  = c(.3, .96))) + plot_layout(guides = "collect")+ plot_annotation(tag_levels = "a")

dev.off()
