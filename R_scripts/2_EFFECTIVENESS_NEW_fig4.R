
library(tidyverse)
library(viridis)
library(wesanderson)
# Load data--------------------------------------------------------------------
number_random_steps <- 20
path_load_file <- paste0("results/gorbea/NEW_consp_step_probability_from_",
                         number_random_steps,"_rd_steps_2021.csv")

probability_info <- read_csv(path_load_file)


# Prepare data for plots-------------------------------------------------------

probability_info$Bosque <- paste0("Plot ",probability_info$Bosque)

probability_info$Periodo <- paste0("Period ",probability_info$Periodo)

census_plantas <- probability_info %>% group_by(Planta) %>% count()

library(latex2exp)
library(scales)


plant_sp_selected = "Polygala_vulgaris"

panel_a <- ggplot(probability_info %>% filter(Bosque %in% c("Plot 1","Plot 3","Plot 4"),
                                              Planta == plant_sp_selected),
                  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = paste0("   a    ",gsub("_"," ",plant_sp_selected)),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(legend.text = element_text(size = 16),
        axis.text = element_text( size = 16 ),
        axis.text.x = element_text( size = 16 ),
        axis.text.y = element_text( size = 16 ),
        axis.title = element_text( size = 18, face = "bold" ),
        strip.text = element_text(size = 15),
        plot.title=element_text(size=20,face="bold"),legend.title = element_text(size = 17))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()
  

panel_a 


##################################################
###################################################

library(glmmTMB)
library(viridis) 
library(RColorBrewer)
library(scales)

data_model_eff_rich <- read_csv("results/gorbea/comm_eff_VS_poll_rich_data_model.csv") %>%
  mutate(Year = as.factor(Year))

plant_sp <- unique(data_model_eff_rich$Planta) %>% sort()
plant_sp_name <- sub("_"," ",plant_sp)

model_av_eff_av_richness <- glmmTMB(prob_consp_step ~ scale(poll_richness)+Planta+Periodo+(1|Bosque)+Year,
                                    family = beta_family(link="logit"),
                                    data = data_model_eff_rich)



colors_brewer <- brewer.pal(12, "Set3")
colors_brewer_extra <- brewer.pal(7, "Dark2")

colores <- c(colors_brewer, colors_brewer_extra)


################################################################################
# PLOTS
################################################################################

#-------------------------------------------------------------------------------------------
# Plot 1
dat1 <- ggeffects::ggpredict(model_av_eff_av_richness, terms = c("poll_richness","Planta","Year"))
plot_data1 <- plot(dat1) + scale_colour_manual(values = colores)
data_plot_1 <- ggplot_build(plot_data1)$data

data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[1]] <- plant_sp[1]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[2]] <- plant_sp[2]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[3]] <- plant_sp[3]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[4]] <- plant_sp[4]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[5]] <- plant_sp[5]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[6]] <- plant_sp[6]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[7]] <- plant_sp[7]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[8]] <- plant_sp[8]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[9]] <- plant_sp[9]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[10]] <- plant_sp[10]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[11]] <- plant_sp[11]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[12]] <- plant_sp[12]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[13]] <- plant_sp[13]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[14]] <- plant_sp[14]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[15]] <- plant_sp[15]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[16]] <- plant_sp[16]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[17]] <- plant_sp[17]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[18]] <- plant_sp[18]
data_plot_1[[1]]$colour[data_plot_1[[1]]$colour==colores[19]] <- plant_sp[19]

data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[1]] <- plant_sp[1]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[2]] <- plant_sp[2]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[3]] <- plant_sp[3]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[4]] <- plant_sp[4]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[5]] <- plant_sp[5]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[6]] <- plant_sp[6]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[7]] <- plant_sp[7]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[8]] <- plant_sp[8]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[9]] <- plant_sp[9]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[10]] <- plant_sp[10]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[11]] <- plant_sp[11]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[12]] <- plant_sp[12]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[13]] <- plant_sp[13]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[14]] <- plant_sp[14]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[15]] <- plant_sp[15]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[16]] <- plant_sp[16]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[17]] <- plant_sp[17]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[18]] <- plant_sp[18]
data_plot_1[[2]]$fill[data_plot_1[[2]]$fill==colores[18]] <- plant_sp[19]



panel_b <- ggplot(data_plot_1[[1]])+#data_plot_1_1_NEW)+
  # geom_ribbon(data = data_plot_1[[2]], aes(x = x, ymin = ymin, ymax = ymax, fill = fill),alpha=0.2)+
  geom_line(aes(x,y,color=colour),linewidth=1.1)+
  geom_point(data = data_model_eff_rich, aes(poll_richness,prob_consp_step,color=Planta), size = 3.5, alpha=0.25)+
  facet_wrap(~Year)+
  labs(
    x = "Pollinator richness",
    y = "Community-level effectiveness",
    title="b Gorbeia N.P.",
    colour = NULL
  )+
  scale_color_manual(values = colores,labels=c(expression(italic("Ajuga reptans")),
                                               expression(italic("Bellis perennis")),
                                               expression(italic("Helleborus viridis")),
                                               expression(italic("Hippocrepis comosa")),
                                               expression(italic("Hutchinsia alpina")),
                                               expression(italic("Lathyrus linifolius")),
                                               expression(italic("Lotus corniculatus")),
                                               expression(italic("Myosotis lamottiana")),
                                               expression(italic("Pedicularis sylvatica")),
                                               expression(italic("Pilosella officinarum")),
                                               expression(italic("Pilosella officinarum")),
                                               expression(italic("Polygala_vulgaris")),
                                               expression(italic("Potentilla sterilis")),
                                               expression(italic("Prunella grandiflora")),
                                               expression(italic("Ranunculus repens")),
                                               expression(italic("Ranunculus sp.")),
                                               expression(italic("Taraxacum sp.")),
                                               expression(italic("Vicia pyrenaica")),
                                               expression(italic("Viola riviniana"))))+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 18, face = "bold"))+ 
  guides(size = FALSE, fill = FALSE, color=guide_legend(nrow=4,byrow=TRUE))


panel_b

library(patchwork)
png("figures/fig4_NEW2.png",
    width = 800*4.4, # The width of the plot in inches
    height = 520*9, res=300)

(panel_a/panel_b) + plot_layout(heights = c(2, 1)) #+ theme(legend.position = "bottom", 
                          #plot.tag = element_text(face = 'bold', size=20), 
                          #plot.tag.position  = c(0.03, 0.95)) #+ plot_annotation(tag_levels = "a")

dev.off()
