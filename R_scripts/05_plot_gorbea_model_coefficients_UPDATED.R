
library(tidyverse)
library(ggallin) # to use pseudolog10_trans in plots



number_random_steps <- 20

# Plot results
path_observations <- paste0("results/gorbea/pollinator_floral_coef_observations_",
                            number_random_steps,"_rd_steps_UPDATED.csv")
coef_observations <- read_csv(path_observations)

coef_observations$term %>% unique()


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
  `delta_richness:delta_total_flowers` = "change in sp. richness:\nchange in the\ntotal # flowers",
  `time_of_day12:40 - 15:19` = "time of day\n12:40 - 15:19",
  `time_of_day15:20 - 18:05` = "time of day\n15:20 - 18:05",
  change_plant_spTRUE = "change plant sp.",
  `delta_richness:change_plant_spTRUE` = "change in sp. richness:unloyal\n(floral fidelity)",
  `delta_total_flowers:change_plant_spTRUE` = "change in the total\n# flowers:unloyal (floral fidelity)",
  `log_sl` = "Ln(step length)"
)

coef_observations$pollinator <- gsub("_", " ", coef_observations$pollinator)

coef_observations$shapes <- "0.1 < p-value"
coef_observations$shapes[coef_observations$p.value <= 0.1] <- "0.05 < p-value < 0.1"
coef_observations$shapes[coef_observations$p.value <= 0.05] <- "p-value < 0.05"

coef_observations$shapes <- factor(coef_observations$shapes, levels=c("p-value < 0.05","0.05 < p-value < 0.1","0.1 < p-value"), ordered = TRUE)

ggplot(coef_observations, aes(y=pollinator))+
  geom_point(aes(x = estimate, color = as.factor(pollinator), shape = shapes),size=2)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error,color = as.factor(pollinator)), width=.2)+
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

png("figures/gorbea_clogit_floral_coef_observed_distributions_UPDATED.png",
    width = 11.69*1.2, # The width of the plot in inches
    height = 11.69*0.6, units = "in", res=300*2)

ggplot(coef_observations, aes(y=pollinator))+
  geom_point(aes(x = estimate, color = as.factor(pollinator), shape = shapes),size=2)+
  geom_errorbar(aes(xmin=estimate-1.96*std.error, xmax=estimate+1.96*std.error,color = as.factor(pollinator)), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  facet_wrap(vars(term), ncol = 5, labeller = as_labeller(coefficient_names), scales = "free_x")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(trans = pseudolog10_trans)+
  theme_bw()+theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="bottom")+
  guides(color = "none")+
  labs(title="Gorbea", x="Coef. estimate", y = NULL,
       color = "Floral\nvisitor", shape = NULL)

dev.off()
