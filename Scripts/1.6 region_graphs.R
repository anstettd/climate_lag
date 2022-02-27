#### Run 1.5 first 

########################################################################################################## 
#################### Graphs North ###################################

############ SPEI ############
# SPEI SLA lag0 = n_sla_lag0
n_vis_sla<-visreg(n_sla_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
SLA_plot<-ggplot(n_Res_sla, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (lag 0)") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
SLA_plot
ggsave("Figures/N.SPEI_lag0_SLA.pdf", width = 6, height = 6, units = "in")


# SPEI Date of Flowering Lag1
n_vis_ft<-visreg(n_fl_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
n_ft_plot<-ggplot(n_Res_ft, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
n_ft_plot <-n_ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
n_ft_plot <-n_ft_plot  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
n_ft_plot
ggsave("Figures/N.SPEI_lag1_ft.pdf", width = 6, height = 6, units = "in")



############ MATA ############
# MATA SLA lag0
n_vis_sla_lag1<-visreg(n_MATA_sla_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla_lag1<-n_vis_sla_lag1$res # Extract residuals

#Reorder Treatments
n_Res_sla_lag1$Drought <- as.factor(n_Res_sla_lag1$Drought)
n_Res_sla_lag1$Drought <- factor(n_Res_sla_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
n_SLA_lag1_plot<-ggplot(n_Res_sla_lag1, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA (lag 1)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
n_SLA_lag1_plot <-n_SLA_lag1_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
n_SLA_lag1_plot <-n_SLA_lag1_plot + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
n_SLA_lag1_plot
ggsave("Figures/N.MATA_lag1_SLA.pdf", width = 6, height = 6, units = "in")


# MATA SLA lag2
n_vis_sla_lag2<-visreg(n_MATA_sla_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla_lag2<-n_vis_sla_lag2$res  # Extract residuals

#Reorder Treatments
n_Res_sla_lag2$Drought <- as.factor(n_Res_sla_lag2$Drought)
n_Res_sla_lag2$Drought <- factor(n_Res_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
n_SLA_lag2_plot<-ggplot(n_Res_sla_lag2, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA lag 2") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
n_SLA_lag2_plot <-n_SLA_lag2_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
n_SLA_lag2_plot <-n_SLA_lag2_plot + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
n_SLA_lag2_plot
ggsave("Figures/N.MATA_lag2_SLA.pdf", width = 6, height = 6, units = "in")



# MATA Date of Flowering Lag1
n_vis_ft_lag1<-visreg(n_MATA_fl_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_ft_lag1<-n_vis_ft_lag1$res # Extract residuals

#Reorder Treatments
n_Res_ft_lag1$Drought <- as.factor(n_Res_ft_lag1$Drought)
n_Res_ft_lag1$Drought <- factor(n_Res_ft_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
n_ft_lag1_plot<-ggplot(n_Res_ft_lag1, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
n_ft_lag1_plot <-n_ft_lag1_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
n_ft_lag1_plot <-n_ft_lag1_plot +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
n_ft_lag1_plot
ggsave("Figures/N.MATA_lag1_ft.pdf", width = 6, height = 6, units = "in")


############ MAPA ############

# MAPA SLA lag2
n_vis_sla_lag2<-visreg(n_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla_lag2<-n_vis_sla_lag2$res # Extract residuals

#Reorder Treatments
n_Res_sla_lag2$Drought <- as.factor(n_Res_sla_lag2$Drought)
n_Res_sla_lag2$Drought <- factor(n_Res_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
n_SLA_lag2_plot<-ggplot(n_Res_sla_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA (lag2))") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
n_SLA_lag2_plot <-n_SLA_lag2_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
n_SLA_lag2_plot <-n_SLA_lag2_plot +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
n_SLA_lag2_plot
ggsave("Figures/N.MAPA_lag2_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA SLA lag012
n_vis_sla_lag012<-visreg(n_MAPA_sla_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla_lag012<-n_vis_sla_lag012$res  # Extract residuals

#Reorder Treatments
n_Res_sla_lag012$Drought <- as.factor(n_Res_sla_lag012$Drought)
n_Res_sla_lag012$Drought <- factor(n_Res_sla_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
n_SLA_lag012_plot<-ggplot(n_Res_sla_lag012, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA 3-Year Average)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
n_SLA_lag012_plot <-n_SLA_lag012_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
n_SLA_lag012_plot <-n_SLA_lag012_plot +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
n_SLA_lag012_plot
ggsave("Figures/N.MAPA_lag012_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag012
vis_ft_D<-visreg(MAPA_fl_lag012, xvar="MAPA_lag012", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(MAPA_fl_lag012, xvar="MAPA_lag012", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_ft_D<-vis_ft_D$res ; Res_ft_W<-vis_ft_W$res # Extract residuals
Res_ft_all<-rbind(Res_ft_D, Res_ft_W) #Row bind wet and dry residuals into one data frame
Res_ft_all$Region<-as.factor(Res_ft_all$Region)
Res_ft_all$Region<-factor(Res_ft_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_ft_all$Drought <- as.factor(Res_ft_all$Drought)
Res_ft_all$Drought <- factor(Res_ft_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
ft_plot<-ggplot(Res_ft_all, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("log(MAPA 3-Year Average)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
ft_plot <-ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
ft_plot <-ft_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
ft_plot
ggsave("Figures/3B.MAPA_lag012_ft.pdf", width = 8, height = 6, units = "in")



############ CMDA ##  Drought*CMDA_lag2
# CMDA SLA lag2
vis_sla_D<-visreg(CMDA_sla_lag2, xvar="CMDA_lag2", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(CMDA_sla_lag2, xvar="CMDA_lag2", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
SLA_plot<-ggplot(Res_sla_all, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("CMDA (lag 2)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
SLA_plot
ggsave("Figures/4A.CMDA_lag2_SLA.pdf", width = 8, height = 6, units = "in")


# CMDA Date of Flowering Lag1
vis_ft_D<-visreg(CMDA_fl_lag1, xvar="CMDA_lag1", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(CMDA_fl_lag1, xvar="CMDA_lag1", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_ft_D<-vis_ft_D$res ; Res_ft_W<-vis_ft_W$res # Extract residuals
Res_ft_all<-rbind(Res_ft_D, Res_ft_W) #Row bind wet and dry residuals into one data frame
Res_ft_all$Region<-as.factor(Res_ft_all$Region)
Res_ft_all$Region<-factor(Res_ft_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_ft_all$Drought <- as.factor(Res_ft_all$Drought)
Res_ft_all$Drought <- factor(Res_ft_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
ft_plot<-ggplot(Res_ft_all, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("CMDA (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
ft_plot <-ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
ft_plot <-ft_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
ft_plot
ggsave("Figures/4B.CMDA_lag1_ft.pdf", width = 8, height = 6, units = "in")



############ CMDA  ##  Drought+CMDA_lag2
# CMDA SLA lag2
vis_sla_D<-visreg(CMDA_sla_lag2a, xvar="CMDA_lag2", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(CMDA_sla_lag2a, xvar="CMDA_lag2", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
SLA_plot<-ggplot(Res_sla_all, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("CMDA (lag 2)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
SLA_plot
ggsave("Figures/5A.CMDA_lag2_SLA.pdf", width = 8, height = 6, units = "in")


# CMDA Date of Flowering Lag1
vis_ft_D<-visreg(CMDA_fl_lag1a, xvar="CMDA_lag1", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(CMDA_fl_lag1a, xvar="CMDA_lag1", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_ft_D<-vis_ft_D$res ; Res_ft_W<-vis_ft_W$res # Extract residuals
Res_ft_all<-rbind(Res_ft_D, Res_ft_W) #Row bind wet and dry residuals into one data frame
Res_ft_all$Region<-as.factor(Res_ft_all$Region)
Res_ft_all$Region<-factor(Res_ft_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_ft_all$Drought <- as.factor(Res_ft_all$Drought)
Res_ft_all$Drought <- factor(Res_ft_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
ft_plot<-ggplot(Res_ft_all, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("CMDA (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
ft_plot <-ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
ft_plot <-ft_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
ft_plot
ggsave("Figures/5B.CMDA_lag1_ft.pdf", width = 8, height = 6, units = "in")








