
library(tidyverse)
library(lme4)
library(lmerTest)
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


# Average values analysis

model_gorbea_av_efficiency <- lmer(n ~ Planta + Periodo+(1|Bosque), data = probability_info %>%
                                mutate(Periodo =as.factor(Periodo),
                                       Bosque = as.factor(Bosque),Year = as.factor(Year),
                                       n=log(prob_consp_step)))
summary(model_gorbea_av_efficiency)

census_plantas <- probability_info %>% group_by(Planta) %>% count()

library(latex2exp)
library(scales)

plant_sp_selected = "Bellis_perennis"

png("figures/gorbea_prob_map_Bellis_perennis.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.55, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()

plant_sp_selected = "Potentilla_erecta"

png("figures/gorbea_prob_map_Potentilla_erecta.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.7, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()

plant_sp_selected = "Ranunculus_sp"

png("figures/gorbea_prob_map_Ranunculus_sp.png",
    width = 11.69*.6, # The width of the plot in inches
    height = 11.69*0.65, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = "Ranunculus sp.",x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()

plant_sp_selected = "Taraxacum_sp"

png("figures/gorbea_prob_map_Taraxacum_sp.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.85, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()

plant_sp_selected = "Lotus_corniculatus"

png("figures/gorbea_prob_map_Lotus_corniculatus.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.6, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()

plant_sp_selected = "Polygala_vulgaris"

png("figures/gorbea_prob_map_Polygala_vulgaris.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.85, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()

plant_sp_selected = "Polygala_vulgaris"

png("figures/fig4_NEW.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.6, units = "in", res=300*2)

ggplot(probability_info %>% filter(Bosque %in% c("Plot 1","Plot 3","Plot 4"),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 13 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 14, face = "bold" ),
        strip.text = element_text(size = 13))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()


plant_sp_selected = "Ranunculus_repens"

png("figures/gorbea_prob_map_Ranunculus_repens.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.6, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()


dev.off()

plant_sp_selected = "Scilla_verna"

png("figures/gorbea_prob_map_Scilla_verna.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.85, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()

plant_sp_selected = "Vicia_pyrenaica"

png("figures/gorbea_prob_map_Vicia_pyrenaica.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.6, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()

plant_sp_selected = "Hutchinsia_alpina"

png("figures/gorbea_prob_map_Hutchinsia_alpina.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.6, units = "in", res=300*2)

ggplot(probability_info %>% filter(#!is.na(prob_consp_step),
  Planta == plant_sp_selected),
  aes(X, Y, fill = log10(prob_consp_step))) + 
  geom_tile()+
  facet_grid(Bosque~Periodo)+
  labs(title = gsub("_"," ",plant_sp_selected),x="X(m)",y="Y(m)",
       fill= TeX('$log_{10}(C.-L. eff.)$'))+
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", type = "continuous"))) +theme_bw()+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="bottom")+ 
  theme(legend.key.width=unit(2,"cm"))+ 
  theme(title = element_text( size = 15 ) ,
        axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 14 ),
        axis.text.y = element_text( size = 14 ),
        axis.title = element_text( size = 16, face = "bold" ),
        strip.text = element_text(size = 15))+ 
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())+
  coord_fixed()

dev.off()


