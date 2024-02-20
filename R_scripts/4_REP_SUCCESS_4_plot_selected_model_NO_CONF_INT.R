
library(tidyverse)
library(glmmTMB)
library(RColorBrewer)
library(scales)

data_log_fullmodel <- read_csv("results/donana/data_gaussian_log_fullmodel.csv")
unique(data_log_fullmodel$Planta)

fullmodel <- glmmTMB(fruitset^2 ~  scale(homo_motif) + scale(hete_motif) + scale(Visits_tot)  + scale(poll_abundance)  +
                           scale(poll_richness) + scale(plant_richness) + scale(total_number_flowers) + scale(unloyal_steps) +
                           scale(log_prob_cons)+(1|Bosque) + Planta + Periodo,
                         family=gaussian(link="log"),
                         data_log_fullmodel)

################################################################################
# PLOTS
################################################################################

#-------------------------------------------------------------------------------------------
# Plot 1
dat1 <- ggeffects::ggpredict(fullmodel, terms = c("log_prob_cons","Planta"),type = "random")
plot_data1 <- plot(dat1)
data_plot_1 <- ggplot_build(plot_data1)$data

data_plot_1[[1]]$colour[data_plot_1[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_1[[2]]$fill[data_plot_1[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p1 <- ggplot(data_plot_1[[1]])+
  # geom_ribbon(data = data_plot_1[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(log_prob_cons,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "log(C.-L. effectiveness)",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                  expression(italic("Cistus ladanifer")),
                                                  expression(italic("Cistus libanotis")),
                                                  expression(italic("Cistus salviifolius")),
                                                  expression(italic("Halimium calycinum")),
                                                  expression(italic("Halimium halimifolium"))
                                                  
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(size = FALSE, fill = FALSE)



#-------------------------------------------------------------------------------------------
# Plot 2
dat2 <- ggeffects::ggpredict(fullmodel, terms = c("unloyal_steps","Planta"),type = "random")
plot_data2 <- plot(dat2)
data_plot_2 <- ggplot_build(plot_data2)$data

data_plot_2[[1]]$colour[data_plot_2[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_2[[1]]$colour[data_plot_2[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_2[[1]]$colour[data_plot_2[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_2[[1]]$colour[data_plot_2[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_2[[1]]$colour[data_plot_2[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_2[[1]]$colour[data_plot_2[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_2[[2]]$fill[data_plot_2[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_2[[2]]$fill[data_plot_2[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_2[[2]]$fill[data_plot_2[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_2[[2]]$fill[data_plot_2[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_2[[2]]$fill[data_plot_2[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_2[[2]]$fill[data_plot_2[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p2 <- ggplot(data_plot_2[[1]])+
  # geom_ribbon(data = data_plot_2[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(unloyal_steps,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "Unloyal steps recorded",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                expression(italic("Cistus ladanifer")),
                                                expression(italic("Cistus libanotis")),
                                                expression(italic("Cistus salviifolius")),
                                                expression(italic("Halimium calycinum")),
                                                expression(italic("Halimium halimifolium"))
                                                
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(color = FALSE, size = FALSE, fill = FALSE)


#-------------------------------------------------------------------------------------------
# Plot 3
dat3 <- ggeffects::ggpredict(fullmodel, terms = c("homo_motif","Planta"),type = "random")
plot_data3 <- plot(dat3)
data_plot_3 <- ggplot_build(plot_data3)$data

data_plot_3[[1]]$colour[data_plot_3[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_3[[1]]$colour[data_plot_3[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_3[[1]]$colour[data_plot_3[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_3[[1]]$colour[data_plot_3[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_3[[1]]$colour[data_plot_3[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_3[[1]]$colour[data_plot_3[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_3[[2]]$fill[data_plot_3[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_3[[2]]$fill[data_plot_3[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_3[[2]]$fill[data_plot_3[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_3[[2]]$fill[data_plot_3[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_3[[2]]$fill[data_plot_3[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_3[[2]]$fill[data_plot_3[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p3 <- ggplot(data_plot_3[[1]])+
  # geom_ribbon(data = data_plot_3[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(homo_motif,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "Homospecfic triplets",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                 expression(italic("Cistus ladanifer")),
                                                 expression(italic("Cistus libanotis")),
                                                 expression(italic("Cistus salviifolius")),
                                                 expression(italic("Halimium calycinum")),
                                                 expression(italic("Halimium halimifolium"))
                                                 
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(color = FALSE, size = FALSE, fill = FALSE)


#-------------------------------------------------------------------------------------------
# Plot 4
dat4 <- ggeffects::ggpredict(fullmodel, terms = c("hete_motif","Planta"),type = "random")
plot_data4 <- plot(dat4)
data_plot_4 <- ggplot_build(plot_data4)$data

data_plot_4[[1]]$colour[data_plot_4[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_4[[1]]$colour[data_plot_4[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_4[[1]]$colour[data_plot_4[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_4[[1]]$colour[data_plot_4[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_4[[1]]$colour[data_plot_4[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_4[[1]]$colour[data_plot_4[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_4[[2]]$fill[data_plot_4[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_4[[2]]$fill[data_plot_4[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_4[[2]]$fill[data_plot_4[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_4[[2]]$fill[data_plot_4[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_4[[2]]$fill[data_plot_4[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_4[[2]]$fill[data_plot_4[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p4 <- ggplot(data_plot_4[[1]])+
  # geom_ribbon(data = data_plot_4[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(hete_motif,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "Heterospecfic triplets",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                 expression(italic("Cistus ladanifer")),
                                                 expression(italic("Cistus libanotis")),
                                                 expression(italic("Cistus salviifolius")),
                                                 expression(italic("Halimium calycinum")),
                                                 expression(italic("Halimium halimifolium"))
                                                 
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(color = FALSE, size = FALSE, fill = FALSE)


#-------------------------------------------------------------------------------------------
# Plot 5
dat5 <- ggeffects::ggpredict(fullmodel, terms = c("plant_richness","Planta"),type = "random")
plot_data5 <- plot(dat5)
data_plot_5 <- ggplot_build(plot_data5)$data

data_plot_5[[1]]$colour[data_plot_5[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_5[[1]]$colour[data_plot_5[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_5[[1]]$colour[data_plot_5[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_5[[1]]$colour[data_plot_5[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_5[[1]]$colour[data_plot_5[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_5[[1]]$colour[data_plot_5[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_5[[2]]$fill[data_plot_5[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_5[[2]]$fill[data_plot_5[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_5[[2]]$fill[data_plot_5[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_5[[2]]$fill[data_plot_5[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_5[[2]]$fill[data_plot_5[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_5[[2]]$fill[data_plot_5[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p5 <- ggplot(data_plot_5[[1]])+
  # geom_ribbon(data = data_plot_5[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(plant_richness,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "Plant richness",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                 expression(italic("Cistus ladanifer")),
                                                 expression(italic("Cistus libanotis")),
                                                 expression(italic("Cistus salviifolius")),
                                                 expression(italic("Halimium calycinum")),
                                                 expression(italic("Halimium halimifolium"))
                                                 
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(color = FALSE, size = FALSE, fill = FALSE)

#-------------------------------------------------------------------------------------------
# Plot 6
dat6 <- ggeffects::ggpredict(fullmodel, terms = c("poll_richness","Planta"),type = "random")
plot_data6 <- plot(dat6)
data_plot_6 <- ggplot_build(plot_data6)$data

data_plot_6[[1]]$colour[data_plot_6[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_6[[1]]$colour[data_plot_6[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_6[[1]]$colour[data_plot_6[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_6[[1]]$colour[data_plot_6[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_6[[1]]$colour[data_plot_6[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_6[[1]]$colour[data_plot_6[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_6[[2]]$fill[data_plot_6[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_6[[2]]$fill[data_plot_6[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_6[[2]]$fill[data_plot_6[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_6[[2]]$fill[data_plot_6[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_6[[2]]$fill[data_plot_6[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_6[[2]]$fill[data_plot_6[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p6 <- ggplot(data_plot_6[[1]])+
  # geom_ribbon(data = data_plot_6[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(poll_richness,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "Pollinator richness",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                 expression(italic("Cistus ladanifer")),
                                                 expression(italic("Cistus libanotis")),
                                                 expression(italic("Cistus salviifolius")),
                                                 expression(italic("Halimium calycinum")),
                                                 expression(italic("Halimium halimifolium"))
                                                 
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(color = FALSE, size = FALSE, fill = FALSE)



#-------------------------------------------------------------------------------------------
# Plot 7
dat7 <- ggeffects::ggpredict(fullmodel, terms = c("total_number_flowers","Planta"),type = "random")
plot_data7 <- plot(dat7)
data_plot_7 <- ggplot_build(plot_data7)$data

data_plot_7[[1]]$colour[data_plot_7[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_7[[1]]$colour[data_plot_7[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_7[[1]]$colour[data_plot_7[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_7[[1]]$colour[data_plot_7[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_7[[1]]$colour[data_plot_7[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_7[[1]]$colour[data_plot_7[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_7[[2]]$fill[data_plot_7[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_7[[2]]$fill[data_plot_7[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_7[[2]]$fill[data_plot_7[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_7[[2]]$fill[data_plot_7[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_7[[2]]$fill[data_plot_7[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_7[[2]]$fill[data_plot_7[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p7 <- ggplot(data_plot_7[[1]])+
  # geom_ribbon(data = data_plot_7[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(total_number_flowers,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "Total number of flowers",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                 expression(italic("Cistus ladanifer")),
                                                 expression(italic("Cistus libanotis")),
                                                 expression(italic("Cistus salviifolius")),
                                                 expression(italic("Halimium calycinum")),
                                                 expression(italic("Halimium halimifolium"))
                                                 
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(color = FALSE, size = FALSE, fill = FALSE)


#-------------------------------------------------------------------------------------------
# Plot 8
dat8 <- ggeffects::ggpredict(fullmodel, terms = c("Visits_tot","Planta"),type = "random")
plot_data8 <- plot(dat8)
data_plot_8 <- ggplot_build(plot_data8)$data

data_plot_8[[1]]$colour[data_plot_8[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_8[[1]]$colour[data_plot_8[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_8[[1]]$colour[data_plot_8[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_8[[1]]$colour[data_plot_8[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_8[[1]]$colour[data_plot_8[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_8[[1]]$colour[data_plot_8[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_8[[2]]$fill[data_plot_8[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_8[[2]]$fill[data_plot_8[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_8[[2]]$fill[data_plot_8[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_8[[2]]$fill[data_plot_8[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_8[[2]]$fill[data_plot_8[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_8[[2]]$fill[data_plot_8[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p8 <- ggplot(data_plot_8[[1]])+
  # geom_ribbon(data = data_plot_8[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(Visits_tot,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "Total number of visits",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                 expression(italic("Cistus ladanifer")),
                                                 expression(italic("Cistus libanotis")),
                                                 expression(italic("Cistus salviifolius")),
                                                 expression(italic("Halimium calycinum")),
                                                 expression(italic("Halimium halimifolium"))
                                                 
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(color = FALSE, size = FALSE, fill = FALSE)

#-------------------------------------------------------------------------------------------
# Plot 9
dat9 <- ggeffects::ggpredict(fullmodel, terms = c("poll_abundance","Planta"),type = "random")
plot_data9 <- plot(dat9)
data_plot_9 <- ggplot_build(plot_data9)$data

data_plot_9[[1]]$colour[data_plot_9[[1]]$colour=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_9[[1]]$colour[data_plot_9[[1]]$colour=="#377EB8"] <- "Cistus_crispus"
data_plot_9[[1]]$colour[data_plot_9[[1]]$colour=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_9[[1]]$colour[data_plot_9[[1]]$colour=="#984EA3"] <- "Halimium_calycinum"
data_plot_9[[1]]$colour[data_plot_9[[1]]$colour=="#FF7F00"] <- "Cistus_libanotis"
data_plot_9[[1]]$colour[data_plot_9[[1]]$colour=="#FFFF33"] <- "Cistus_ladanifer"

data_plot_9[[2]]$fill[data_plot_9[[2]]$fill=="#E41A1C"] <- "Cistus_salviifolius"
data_plot_9[[2]]$fill[data_plot_9[[2]]$fill=="#377EB8"] <- "Cistus_crispus"
data_plot_9[[2]]$fill[data_plot_9[[2]]$fill=="#4DAF4A"] <- "Halimium_halimifolium"
data_plot_9[[2]]$fill[data_plot_9[[2]]$fill=="#984EA3"] <- "Halimium_calycinum"
data_plot_9[[2]]$fill[data_plot_9[[2]]$fill=="#FF7F00"] <- "Cistus_libanotis"
data_plot_9[[2]]$fill[data_plot_9[[2]]$fill=="#FFFF33"] <- "Cistus_ladanifer"

p9 <- ggplot(data_plot_9[[1]])+
  # geom_ribbon(data = data_plot_9[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_log_fullmodel, aes(poll_abundance,fruitset,color=Planta), size = 3.5, alpha=0.25)+
  labs(
    x = "Total number of pollinators",
    y = "Fruits per flower",
    colour = NULL,
    title =NULL
  )+scale_color_brewer(palette = "Set1",labels=c(expression(italic("Cistus crispus")),
                                                 expression(italic("Cistus ladanifer")),
                                                 expression(italic("Cistus libanotis")),
                                                 expression(italic("Cistus salviifolius")),
                                                 expression(italic("Halimium calycinum")),
                                                 expression(italic("Halimium halimifolium"))
                                                 
  ))+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14))+ 
  guides(color = FALSE, size = FALSE, fill = FALSE)


#--------------------------------
# Save image
library(patchwork)
png("figures/fig5_NEW.png", width=2000*4.2*0.8, height = 2000*2*1.8, res=300*2)
((p1+p2+p3)/(p4+p5+p6)/(p7+ p9 + p8) & theme(legend.position = "bottom", 
                                                              plot.tag = element_text(face = 'bold', size=20), 
                                                              plot.tag.position  = c(0.03, 0.95))) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")
dev.off()


#--------------------------------
# # Save image
# library(patchwork)
# png("figures/fig5_NEW_Ainhoa.png", width=2000*4.2*0.7, height = 2000*2*3, res=300*2)
# ((p1+p2)/(p3+p4)/(p5+p6)/(p7+ p9)/(p8+ plot_spacer()) & theme(legend.position = "bottom")) + plot_layout(guides = "collect")
# dev.off()
