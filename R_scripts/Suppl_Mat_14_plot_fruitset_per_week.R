
library(tidyverse)

#---------------------------------------
# Load fruitset data for Doñana 2021

fruitset_census <- read_csv("results/donana/fruitset_census_21.csv")  %>% filter(!is.na(Planta)) %>%
  mutate(fruitset = number_fruits/labelled_flowers)

fruitset_census$Planta  <-  gsub("_"," ",fruitset_census$Planta)


fruitset_census %>% group_by(Planta) %>% count() %>% arrange(desc(n))

png("figures/fruitset_per_week.png", width=1000*7, height = 1000*5, res=300*2)
ggplot(fruitset_census,aes(x=as.factor(Week_ISO), y = fruitset))+
  geom_boxplot()+
  geom_smooth(aes(x=Week_ISO),method = "lm", formula = y ~ I(x^3))+
  facet_wrap(~Planta)+
  labs(x="Sampling week", y = "Fruitset")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"))+ 
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 14 ),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.position="none",
         # The new stuff
         strip.text = element_text(size = 16))
dev.off()

floral_census <- read_csv("results/donana/flora_census_21.csv")
floral_census$Planta  <-  gsub("_"," ",floral_census$Planta)

png("figures/abundance_C_ladanifer_Hali_calycinum.png", width=1000*7*.8, height = 1000*4*.8, res=300*2)
ggplot(floral_census %>% filter(Planta %in% c("Halimium calycinum","Cistus ladanifer")),aes(x=as.factor(Week_ISO), y = Flores))+
  geom_boxplot()+
  geom_smooth(aes(x=Week_ISO),method = "lm", formula = y ~ I(x^3))+
  facet_wrap(~Planta)+
  labs(x="Sampling week", y = "Abundance")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"))+ 
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 14 ),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.position="none",
         # The new stuff
         strip.text = element_text(size = 16))
dev.off()

flora_census_filtered <- floral_census %>% filter(Planta %in% c("Halimium calycinum","Cistus ladanifer"))

flora_census_filtered %>% group_by(Planta,Week_ISO) %>% count() %>% arrange(Week_ISO)

# Realizar un análisis de varianza de dos factores
modelo <- aov(Flores ~ Week_ISO * Planta, data =flora_census_filtered)

# Mostrar un resumen del análisis
summary(modelo)


conditions_flora_census_filtered <- flora_census_filtered %>% mutate(condition=paste0(Planta,Week_ISO),
                                                                     condition=as.factor(condition))
  
library(sandwich)
library(multcomp)

amod <- aov(Flores ~ condition, conditions_flora_census_filtered)
amod_glht <- glht(amod, mcp(condition="Tukey"), vcov=vcovHC)
summary_report <- summary(amod_glht)
plot(summary_report)
pvalues <- summary_report[["test"]][["pvalues"]]
pvalues[pvalues<0.05]
which(pvalues<0.05)
coefficients <- summary_report[["test"]][["coefficients"]][which(pvalues<0.05)]
sigma <- summary_report[["test"]][["sigma"]][which(pvalues<0.05)]
# confint(amod_glht)
# plot(confint(amod_glht))

results_observations_condition <- tibble(comparisson = names(coefficients),
                                     coefficients = as.numeric(coefficients),
                                     sigma = as.numeric(sigma))


png("figures/donana_condition_comparisson_fl_abundance_Cist_ladanifer_Hali_calycinum.png",
    width = 11.69*.9*0.8, # The width of the plot in inches
    height = 11.69*.5*600/450*.35, units = "in", res=300*2)

ggplot(results_observations_condition,aes(y = as.factor(comparisson)))+
  geom_point(aes(x=coefficients),size=2)+
  geom_errorbar(aes(xmin=coefficients-1.96*sigma, xmax=coefficients+1.96*sigma), width=.2)+
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw()+
  guides(size = "none")+
  labs(title="Doñana (Flower abundance)", x=NULL, y = NULL)+
  theme(axis.text=element_text(size=16),  plot.title=element_text(size=19))+
  theme(strip.text = element_text(size=15,face = "italic"))

dev.off()
