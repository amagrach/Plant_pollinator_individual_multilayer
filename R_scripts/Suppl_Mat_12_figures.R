
library(tidyverse)
library(scales)

motif_info <- read_csv("results/donana/triplets_week/2021_donana_WEEK_SPECIES.csv")

total_motifs_per_plant <- motif_info %>% group_by(Bosque, Subplot_Plant_Label) %>%
  summarise(total_visits = sum(Visits_tot), total_homo = sum(homo_motif),
            total_hete = sum(hete_motif)) %>%
  separate(Subplot_Plant_Label,c("Subplot","Plant")," ") %>%
  mutate(Plant = gsub("_", " ", Plant))


# Correlations

cor.test(total_motifs_per_plant$total_visits,total_motifs_per_plant$total_homo,method = "spearman")
cor.test(total_motifs_per_plant$total_visits,total_motifs_per_plant$total_hete,method = "spearman")


wilcox.test(total_motifs_per_plant$total_homo,
            total_motifs_per_plant$total_hete,
            paired = TRUE)

plant_sp = "Cistus salviifolius"
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Leontodon longirostris"
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Cistus crispus"
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Galactites tomentosus"
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Lavandula stoechas" 
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Aetheorhiza bulbosa"
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Cistus ladanifer" 
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Rosmarinus officinalis"
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Halimium calycinum"
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)

plant_sp = "Ulex australis"  
wilcox.test(total_motifs_per_plant$total_homo[total_motifs_per_plant$Plant==plant_sp],
            total_motifs_per_plant$total_hete[total_motifs_per_plant$Plant==plant_sp],
            paired = TRUE)



total_number_plants_observed <- total_motifs_per_plant %>% ungroup() %>%
  group_by(Plant) %>% count() %>% arrange(desc(n))

plant_species_selected <- total_number_plants_observed$Plant[1:10]




png("figures/plant_visits_per_week.png",
    width = 800*2, # The width of the plot in inches
    height = 520*2,
    res=300)
ggplot(motif_info %>% separate(Subplot_Plant_Label,c("Subplot","Plant")," ") %>% 
         group_by(Plant,Week) %>%
         summarise(total_visits = sum(Visits_tot))  %>%
         mutate(Plant = gsub("_", " ", Plant))%>% filter(Plant %in% plant_species_selected))+
  geom_point(aes(x=Week,y=Plant,size=total_visits,color=total_visits))+
  scale_color_distiller(palette = "Spectral")+ 
  scale_x_continuous(breaks= pretty_breaks())+
  ylab(NULL) +
  theme_bw()+
  labs(color = "# Visits",size = "# Visits")+
  theme(axis.text.y = element_text(face = "italic"))+ guides(size = "none")
dev.off()


png("figures/plant_homo_motifs_per_week.png",
    width = 800*2, # The width of the plot in inches
    height = 520*2,
    res=300)
ggplot(motif_info %>% separate(Subplot_Plant_Label,c("Subplot","Plant")," ") %>% 
         group_by(Plant,Week) %>%
         summarise(total_homo = sum(homo_motif))  %>%
         mutate(Plant = gsub("_", " ", Plant)) %>%
         filter(Plant %in% plant_species_selected))+
  geom_point(aes(x=Week,y=Plant,size=total_homo,color=total_homo))+
  scale_color_distiller(palette = "Spectral")+ 
  scale_x_continuous(breaks= pretty_breaks())+
  ylab(NULL) +
  theme_bw()+
  labs(color = "# Hom. subgr.",size = "# Hom. subgr.")+
  theme(axis.text.y = element_text(face = "italic"))+ guides(size = "none")
dev.off()

png("figures/plant_hete_motifs_per_week.png",
    width = 800*2, # The width of the plot in inches
    height = 520*2,
    res=300)
ggplot(motif_info %>% separate(Subplot_Plant_Label,c("Subplot","Plant")," ") %>% 
         group_by(Plant,Week) %>%
         summarise(total_hete = sum(hete_motif))  %>%
         mutate(Plant = gsub("_", " ", Plant)) %>% 
         filter(Plant %in% plant_species_selected))+
  geom_point(aes(x=Week,y=Plant,size=total_hete,color=total_hete))+
  scale_color_distiller(palette = "Spectral")+ 
  scale_x_continuous(breaks= pretty_breaks())+
  ylab(NULL) +
  theme_bw()+
  labs(color = "# Het. subgr.",size = "# Het. subgr.")+
  theme(axis.text.y = element_text(face = "italic"))+ guides(size = "none")
dev.off()

png("figures/plant_total_hete_homo_motifs.png",
    width = 360*3, # The width of the plot in inches
    height = 660*3,
    res=300)
ggplot(total_motifs_per_plant %>% filter(Plant %in% plant_species_selected), aes(x=total_homo, y = total_hete))+
  geom_point(size=3,alpha=0.25)+
  facet_wrap(~Plant,ncol =2)+
  geom_abline(slope = 1, intercept = 0, linetype="dashed", linewidth=1.2, color="Deep Sky Blue")+
  labs(x="Total number of homospecific subgraphs",y="Total number of heterospecific subgraphs")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"))
dev.off()

ggplot(total_motifs_per_plant %>% filter(Plant %in% plant_species_selected), aes(x=total_homo, y = total_hete))+
  geom_point(size=3,alpha=0.25)+
  facet_grid(Bosque~Plant)+
  geom_abline(slope = 1, intercept = 0, linetype="dashed", linewidth=1.2, color="Deep Sky Blue")+
  labs(x="Total number of homospecific subgraphs",y="Total number of heterospecific subgraphs")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"))
