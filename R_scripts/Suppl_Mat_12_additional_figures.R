
library(tidyverse)


flora_census <- read_csv("results/donana/flora_census_21.csv")

unique(flora_census$Bosque)

flora_census$Bosque[flora_census$Bosque=="Pinar Aznalcazar"] <- "Aznalcazar"
flora_census$Bosque[flora_census$Bosque=="Pinar Hinojos"] <- "Hinojos"
flora_census$Bosque[flora_census$Bosque=="Pinar Puebla"] <- "Puebla"
flora_census$Bosque[flora_census$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Vill. Este"
flora_census$Bosque[flora_census$Bosque=="Pinar Villamanrique Sur"] <- "Vill. Sur"
flora_census$Planta <- sub("_", " ", flora_census$Planta)

flowers_subplot_plant <- flora_census %>% 
  group_by(Year,Bosque,Periodo,Planta,X,Y) %>% count(wt=Flores) %>%
  mutate(subplot_label = paste0(Year," ",Bosque," ",Periodo," ",X," ",Y))  %>%
  rename(number_flowers = n) %>% ungroup() %>% 
  dplyr::select(subplot_label,Planta,number_flowers)

#sanity check
any(duplicated(flowers_subplot_plant))


flights_raw <- read_csv("results/donana/foraging_Donana_2021.csv") %>%
  rename(node = Codigo_within_sequence)

flights_raw$Bosque[flights_raw$Bosque=="Pinar Aznalcazar"] <- "Aznalcazar"
flights_raw$Bosque[flights_raw$Bosque=="Pinar Hinojos"] <- "Hinojos"
flights_raw$Bosque[flights_raw$Bosque=="Pinar Puebla"] <- "Puebla"
flights_raw$Bosque[flights_raw$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Vill. Este"
flights_raw$Bosque[flights_raw$Bosque=="Pinar Villamanrique Sur"] <- "Vill. Sur"
flights_raw$Planta <- sub("_", " ", flights_raw$Planta)

flights_raw$Periodo <- 1
flights_raw$Periodo[flights_raw$Day_ISO > 90] <- 2
flights_raw$Periodo[flights_raw$Day_ISO > 123] <- 3

visits_subplot_plant <- flights_raw %>% 
  group_by(Year,Bosque,Periodo,Planta,X,Y) %>% count() %>%
  mutate(subplot_label = paste0(Year," ",Bosque," ",Periodo," ",X," ",Y))  %>%
  rename(number_visits = n) %>% ungroup() %>% 
  dplyr::select(subplot_label,Planta,number_visits)

#sanity check
any(duplicated(visits_subplot_plant))


# Motifs-----------------------------------------

motifs_raw <- 
  read_csv("results/donana/triplets_week/2021_donana_WEEK_SPECIES.csv") %>%
  separate(Subplot_Plant_Label,sep =" " , names =,c("X_Y","Planta"))%>%
  separate(X_Y,sep ="-" , names =,c("X","Y"))

unique(flights_raw$Week[flights_raw$Periodo==1]) %>% sort()
unique(flights_raw$Week[flights_raw$Periodo==2]) %>% sort()
unique(flights_raw$Week[flights_raw$Periodo==3]) %>% sort()

motifs_raw$Periodo <- 1
motifs_raw$Periodo[motifs_raw$Week > 4] <- 2
motifs_raw$Periodo[motifs_raw$Week > 9] <- 3

motifs_raw$Bosque[motifs_raw$Bosque=="Pinar Aznalcazar"] <- "Aznalcazar"
motifs_raw$Bosque[motifs_raw$Bosque=="Pinar Hinojos"] <- "Hinojos"
motifs_raw$Bosque[motifs_raw$Bosque=="Pinar Puebla"] <- "Puebla"
motifs_raw$Bosque[motifs_raw$Bosque=="Pinar Villamanrique Este (Chaparral)"] <- "Vill. Este"
motifs_raw$Bosque[motifs_raw$Bosque=="Pinar Villamanrique Sur"] <- "Vill. Sur"
motifs_raw$Planta <- sub("_", " ", motifs_raw$Planta)

motifs_subplot_plant <- motifs_raw %>% 
  group_by(Year,Bosque,Periodo,Planta,X,Y) %>% summarise(number_homo = sum(homo_motif), number_hete = sum(hete_motif)) %>%
  mutate(subplot_label = paste0(Year," ",Bosque," ",Periodo," ",X," ",Y))  %>%
  ungroup() %>% 
  dplyr::select(subplot_label,Planta,number_homo,number_hete)

#sanity check
any(duplicated(motifs_subplot_plant))


#-----------------------------------------------------------
motifs_subplot_plant$subplot_label[!unique(motifs_subplot_plant$subplot_label) %in% 
                                     unique(visits_subplot_plant$subplot_label)] %>%
  unique()

motifs_subplot_plant$subplot_label[!unique(motifs_subplot_plant$subplot_label) %in% 
                                     unique(flowers_subplot_plant$subplot_label)] %>%
  unique()

visits_subplot_plant$subplot_label[!unique(visits_subplot_plant$subplot_label) %in% 
                                     unique(flowers_subplot_plant$subplot_label)] %>%
  unique()

#-----------------------------------------------------------

total_number_plant_sp <- flowers_subplot_plant %>% 
  group_by(subplot_label) %>% count() %>%
  rename(plant_sp_in_subplot_flwr_census = n) %>% ungroup()

total_number_plant_sp2 <- visits_subplot_plant %>% 
  group_by(subplot_label) %>% count() %>%
  rename(plant_sp_in_subplot_seq_census = n) %>% ungroup()

total_number_flowers <- flowers_subplot_plant %>% 
  group_by(subplot_label) %>% count(wt=number_flowers) %>%
  rename(flowers_in_subplot = n) %>% ungroup()

total_number_visits <- visits_subplot_plant %>% 
  group_by(subplot_label) %>% count(wt=number_visits) %>%
  rename(visits_in_subplot = n) %>% ungroup()

visits_subplot_plant %>% filter(subplot_label=="2021 Aznalcazar 1 3 9")
total_number_visits %>% filter(subplot_label=="2021 Aznalcazar 1 3 9")

subplot_data <- flowers_subplot_plant %>% 
  full_join(visits_subplot_plant, by = c("subplot_label","Planta")) %>%
  full_join(motifs_subplot_plant, by = c("subplot_label","Planta")) %>%
  left_join(total_number_plant_sp, by = c("subplot_label")) %>%
  left_join(total_number_plant_sp2, by = c("subplot_label")) %>%
  left_join(total_number_flowers, by = c("subplot_label")) %>%
  left_join(total_number_visits, by = c("subplot_label")) %>%
  filter(!is.na(visits_in_subplot)) %>%
  arrange(desc(visits_in_subplot)) %>% 
  mutate_all(~ replace_na(., 0))


library(patchwork)
library(RColorBrewer)

colors_set1 <- brewer.pal(9, "Set1")
colors_set2 <- brewer.pal(8, "Set2")
colors_set3 <- brewer.pal(8, "Set3")

combined_colors <- c(colors_set1, colors_set2[1:3], colors_set3[1:4])

Plant_cat <- unique(c(subplot_data$Planta[subplot_data$flowers_in_subplot < 10],
                      subplot_data$Planta[subplot_data$flowers_in_subplot > 100])
                    ) %>% sort()
colors4plots <- setNames(combined_colors, Plant_cat)

plot_flowers <- ggplot(subplot_data %>% filter(plant_sp_in_subplot_flwr_census >1, visits_in_subplot>10), aes(x = number_flowers, y = subplot_label, fill = Planta)) + 
  geom_bar(stat = "identity")+labs(x="Number of flowers",y="Subplot ID",fill=NULL)+
  scale_fill_manual(values = colors4plots)+
  theme_bw()+
  theme(text = element_text(face = "italic"), axis.text.y = element_blank(),
        legend.text = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15))

plot_visits <- ggplot(subplot_data %>% filter(plant_sp_in_subplot_flwr_census >1, visits_in_subplot>10), aes(x = number_visits, y = subplot_label, fill = Planta)) + 
  geom_bar(stat = "identity")+labs(x="Number of visits",y="Subplot ID",fill=NULL)+
  scale_fill_manual(values = colors4plots)+
  theme_bw()+
  theme(text = element_text(face = "italic"), axis.text.y = element_blank(),
        legend.text = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15))

plot_homo <- ggplot(subplot_data %>% filter(plant_sp_in_subplot_flwr_census >1, visits_in_subplot>10), aes(x = number_homo, y = subplot_label, fill = Planta)) + 
  geom_bar(stat = "identity")+labs(x="Number of homosp. triplets",y="Subplot ID",fill=NULL)+
  scale_fill_manual(values = colors4plots)+
  theme_bw()+
  theme(text = element_text(face = "italic"), axis.text.y = element_blank(),
        legend.text = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15))

plot_hete <- ggplot(subplot_data %>% filter(plant_sp_in_subplot_flwr_census >1, visits_in_subplot>10), aes(x = number_hete, y = subplot_label, fill = Planta)) + 
  geom_bar(stat = "identity")+labs(x="Number of heterosp. triplets",y="Subplot ID",fill=NULL)+
  scale_fill_manual(values = colors4plots)+
  theme_bw()+
  theme(text = element_text(face = "italic"), axis.text.y = element_blank(),
        legend.text = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15))

png("figures/NEW_motif.png", width=2000*4.2, height = 2000*2*1.7, res=300*2)
((plot_visits+plot_flowers)/(plot_homo + plot_hete) & 
    theme(legend.position = "bottom", plot.tag = element_text(face = 'bold', size=20), plot.tag.position  = c(0.01, .96))) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")
dev.off()



length(unique(subplot_data$subplot_label[(subplot_data$plant_sp_in_subplot_flwr_census >1) & (subplot_data$visits_in_subplot>10)]))


#######################################



# plot_flowers2 <- ggplot(subplot_data %>% filter(plant_sp_in_subplot_seq_census >1), aes(x = number_flowers, y = subplot_label, fill = Planta)) + 
#   geom_bar(stat = "identity")+labs(x="Number of flowers",y="Subplot ID",fill=NULL)+
#   scale_fill_manual(values = colors4plots)+
#   theme_bw()+
#   theme(text = element_text(face = "italic"), axis.text.y = element_blank())
# 
# plot_visits2 <- ggplot(subplot_data %>% filter(plant_sp_in_subplot_seq_census >1), aes(x = number_visits, y = subplot_label, fill = Planta)) + 
#   geom_bar(stat = "identity")+labs(x="Number of visits",y="Subplot ID",fill=NULL)+
#   scale_fill_manual(values = colors4plots)+
#   theme_bw()+
#   theme(text = element_text(face = "italic"), axis.text.y = element_blank())
# 
# plot_homo2 <- ggplot(subplot_data %>% filter(plant_sp_in_subplot_seq_census >1), aes(x = number_homo, y = subplot_label, fill = Planta)) + 
#   geom_bar(stat = "identity")+labs(x="Number of homosp. triplets",y="Subplot ID",fill=NULL)+
#   scale_fill_manual(values = colors4plots)+
#   theme_bw()+
#   theme(text = element_text(face = "italic"), axis.text.y = element_blank())
# 
# plot_hete2 <- ggplot(subplot_data %>% filter(plant_sp_in_subplot_seq_census >1), aes(x = number_hete, y = subplot_label, fill = Planta)) + 
#   geom_bar(stat = "identity")+labs(x="Number of heterosp. triplets",y="Subplot ID",fill=NULL)+
#   scale_fill_manual(values = colors4plots)+
#   theme_bw()+
#   theme(text = element_text(face = "italic"), axis.text.y = element_blank())
# 
# png("figures/NEW_motif2.png", width=2000*4.2*0.7, height = 2000*2*1.7, res=300*2)
# ((plot_flowers2 + plot_visits2)/(plot_homo2 + plot_hete2) & 
#     theme(legend.position = "bottom", plot.tag = element_text(face = 'bold', size=20), plot.tag.position  = c(0, .96))) +
#   plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")
# dev.off()



##############################################

# cor.test(subplot_data$number_flowers[subplot_data$visits_in_subplot>10],
#          subplot_data$number_visits[subplot_data$visits_in_subplot>10], method = "spearman")
# 
# cor.test(subplot_data$number_flowers[subplot_data$visits_in_subplot>10],
#          subplot_data$number_homo[subplot_data$visits_in_subplot>10], method = "spearman")
# 
# cor.test(subplot_data$number_flowers[subplot_data$visits_in_subplot>10],
#          subplot_data$number_hete[subplot_data$visits_in_subplot>10], method = "spearman")
# 
# cor.test(subplot_data$number_visits[subplot_data$visits_in_subplot>10],
#          subplot_data$number_homo[subplot_data$visits_in_subplot>10], method = "spearman")
# 
# cor.test(subplot_data$number_visits[subplot_data$visits_in_subplot>10],
#          subplot_data$number_hete[subplot_data$visits_in_subplot>10], method = "spearman")


# library(scales)
# ggplot(subplot_data, aes(x = number_homo, y = number_visits)) + 
#   geom_point(size=3,alpha=0.3)+
#   facet_wrap(~Planta,)+
#   scale_x_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   scale_y_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   labs(x="Number of homospecific triplets",y="Number of visits")+
#   theme_bw()+
#   theme(strip.text.x = element_text(face = "italic"))
# 
# ggplot(subplot_data, aes(x = number_hete, y = number_visits)) + 
#   geom_point(size=3,alpha=0.3)+
#   facet_wrap(~Planta,)+
#   scale_x_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   scale_y_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   labs(x="Number of heterospecific triplets",y="Number of visits")+
#   theme_bw()+
#   theme(strip.text.x = element_text(face = "italic"))
# 
# 
# ggplot(subplot_data, aes(x = number_homo, y = number_hete)) + 
#   geom_point(size=3,alpha=0.3)+
#   facet_wrap(~Planta,)+
#   scale_x_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   scale_y_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   labs(x="Number of homospecific triplets",y="Number of heterospecific triplets")+
#   theme_bw()+
#   theme(strip.text.x = element_text(face = "italic"))
# 
# ggplot(subplot_data, aes(x = number_homo, y = number_flowers)) + 
#   geom_point(size=3,alpha=0.3)+
#   facet_wrap(~Planta,)+
#   scale_x_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   scale_y_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   labs(x="Number of homospecific triplets",y="Number of flowers")+
#   theme_bw()+
#   theme(strip.text.x = element_text(face = "italic"))
# 
# ggplot(subplot_data, aes(x = number_hete, y = number_flowers)) + 
#   geom_point(size=3,alpha=0.3)+
#   facet_wrap(~Planta,)+
#   scale_x_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   scale_y_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   labs(x="Number of heterospecific triplets",y="Number of flowers")+
#   theme_bw()+
#   theme(strip.text.x = element_text(face = "italic"))
# 
# 
# ggplot(subplot_data, aes(x = number_flowers, y = number_visits)) + 
#   geom_point(size=3,alpha=0.3)+
#   facet_wrap(~Planta,)+
#   scale_x_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   scale_y_continuous(trans = log10_trans(),
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)))+
#   labs(x="Number of flowers",y="Number of visits")+
#   theme_bw()+
#   theme(strip.text.x = element_text(face = "italic"))
