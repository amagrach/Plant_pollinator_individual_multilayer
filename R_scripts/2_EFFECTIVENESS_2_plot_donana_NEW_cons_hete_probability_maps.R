
library(tidyverse)
library(lme4)
library(lmerTest)
library(wesanderson)

# Load data--------------------------------------------------------------------
number_random_steps <- 20
path_load_file <- paste0("results/donana/NEW_consp_step_probability_from_",
                         number_random_steps,"_rd_steps.csv")

probability_info <- read_csv(path_load_file)


# Prepare data for plots-------------------------------------------------------

probability_info$Bosque[probability_info$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Pinar Villamanrique Este"

probability_info$Bosque <- gsub("Pinar ","",probability_info$Bosque)
probability_info$Bosque <- gsub("Villamanrique ","Villam. ",probability_info$Bosque)

probability_info$Periodo <- paste0("Period ",probability_info$Periodo)


# Average values analysis

model_donana_av_efficiency <- lmer(n ~ Planta + Periodo+(1|Bosque), data = probability_info %>%
                                mutate(Periodo =as.factor(Periodo),
                                       Bosque = as.factor(Bosque),Year = as.factor(Year),
                                       n=log(prob_consp_step)))
summary(model_donana_av_efficiency)



library(latex2exp)
library(scales)
plant_sp_selected = "Cistus_salviifolius"

png("figures/donana_prob_map_Cistus_salviifolius.png",
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
  scale_y_continuous(breaks= pretty_breaks())

dev.off()

plant_sp_selected = "Leontodon_longirostris"
png("figures/donana_prob_map_Leontodon_longirostris.png",
    width = 11.69*.55, # The width of the plot in inches
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()

plant_sp_selected = "Cistus_crispus"
png("figures/donana_prob_map_Cistus_crispus.png",
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()

plant_sp_selected = "Galactites_tomentosus"
png("figures/donana_prob_map_Galactites_tomentosus.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.5, units = "in", res=300*2)
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()

plant_sp_selected = "Lavandula_stoechas"
png("figures/donana_prob_map_Lavandula_stoechas.png",
    width = 11.69*.7, # The width of the plot in inches
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()

plant_sp_selected = "Aetheorhiza_bulbosa"
png("figures/donana_prob_map_Aetheorhiza_bulbosa.png",
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()

plant_sp_selected = "Cistus_ladanifer"
png("figures/donana_prob_map_Cistus_ladanifer.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.35, units = "in", res=300*2)
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()

plant_sp_selected = "Rosmarinus_officinalis"
png("figures/donana_prob_map_Rosmarinus_officinalis.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.5, units = "in", res=300*2)
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()

plant_sp_selected = "Halimium_calycinum"
png("figures/donana_prob_map_Halimium_calycinum.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.35, units = "in", res=300*2)
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()



plant_sp_selected = "Ulex_australis"
png("figures/donana_prob_map_Ulex_australis.png",
    width = 11.69*.5, # The width of the plot in inches
    height = 11.69*0.5, units = "in", res=300*2)
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
  scale_y_continuous(breaks= pretty_breaks())
dev.off()

