
library(tidyverse)


data_model_beta <- read_csv("results/donana/beta_model_results.csv")

data_model_beta$species_name[data_model_beta$Planta == "Cistus_crispus"] <- " Cistus crispus"
data_model_beta$species_name[data_model_beta$Planta == "Cistus_ladanifer"] <- "Cistus ladanifer"
data_model_beta$species_name[data_model_beta$Planta == "Cistus_libanotis"] <- "Cistus libanotis"
data_model_beta$species_name[data_model_beta$Planta == "Halimium_calycinum"] <- "Halimium calycinum"

data_model_beta$predictions <- predict(short_alternative_variables_fruitset_GLM_beta_planta_quan) 
ggplot(data_model_beta, aes(x=fruitset,y=predictions, color = species_name))+
  geom_abline(slope = 1,  color="deepskyblue1",
              linetype="dashed", linewidth=1.5)+geom_point(alpha=0.5,size=3)+
  labs(x="Observed fruitset", y="Predicted fruitset", color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(face = "italic"))+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(nrow=2,byrow=TRUE,override.aes = list(size=5)))


png("figures/fruitset_beta_fit.png", width=500, height = 500, res=300)

ggplot(data_model_beta, aes(x=fruitset,y=predictions, color = species_name))+
  geom_abline(slope = 1,  color="deepskyblue1",
              linetype="dashed", linewidth=1.5)+geom_point(alpha=0.5,size=3)+
  labs(x="Observed fruitset", y="Predicted fruitset", color=NULL)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(face = "italic"))+
  theme(legend.text = element_text(size=15))+
  theme(axis.text=element_text(size=15),  axis.title=element_text(size=17,face="bold"))+                                                                # Change font size
  theme(strip.text.x = element_text(size = 18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour = guide_legend(nrow=2,byrow=TRUE,override.aes = list(size=5)))


dev.off()
