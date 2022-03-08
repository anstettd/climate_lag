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
N1<-ggplot(n_Res_sla, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (lag 0)") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N1 <-N1 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N1 <-N1 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N1
ggsave("Figures/N.SPEI_lag0_SLA.pdf", width = 6, height = 6, units = "in")


# SPEI Date of Flowering Lag1
n_vis_ft<-visreg(n_fl_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_ft, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2
ggsave("Figures/N.SPEI_lag1_ft.pdf", width = 6, height = 6, units = "in")



############ MATA ############
# MATA SLA lag1
n_vis_sla_lag1<-visreg(n_MATA_sla_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla_lag1<-n_vis_sla_lag1$res # Extract residuals

#Reorder Treatments
n_Res_sla_lag1$Drought <- as.factor(n_Res_sla_lag1$Drought)
n_Res_sla_lag1$Drought <- factor(n_Res_sla_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla_lag1, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA (lag 1)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N3 <-N3 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N3 <-N3 + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N3
ggsave("Figures/N.MATA_lag1_SLA.pdf", width = 6, height = 6, units = "in")


# MATA SLA lag2
n_vis_sla_lag2<-visreg(n_MATA_sla_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla_lag2<-n_vis_sla_lag2$res  # Extract residuals

#Reorder Treatments
n_Res_sla_lag2$Drought <- as.factor(n_Res_sla_lag2$Drought)
n_Res_sla_lag2$Drought <- factor(n_Res_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla_lag2, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA lag 2") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N4 <-N4 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N4 <-N4 + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N4
ggsave("Figures/N.MATA_lag2_SLA.pdf", width = 6, height = 6, units = "in")



# MATA Date of Flowering Lag1
n_vis_ft_lag1<-visreg(n_MATA_fl_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_ft_lag1<-n_vis_ft_lag1$res # Extract residuals

#Reorder Treatments
n_Res_ft_lag1$Drought <- as.factor(n_Res_ft_lag1$Drought)
n_Res_ft_lag1$Drought <- factor(n_Res_ft_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N5<-ggplot(n_Res_ft_lag1, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N5 <-N5 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N5 <-N5 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N5
ggsave("Figures/N.MATA_lag1_ft.pdf", width = 6, height = 6, units = "in")


############ MAPA ############

# MAPA SLA lag2
n_vis_sla_lag2<-visreg(n_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla_lag2<-n_vis_sla_lag2$res # Extract residuals

#Reorder Treatments
n_Res_sla_lag2$Drought <- as.factor(n_Res_sla_lag2$Drought)
n_Res_sla_lag2$Drought <- factor(n_Res_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N6<-ggplot(n_Res_sla_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA (lag2))") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N6 <-N6 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N6 <-N6 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N6
ggsave("Figures/N.MAPA_lag2_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA SLA lag012
n_vis_sla_lag012<-visreg(n_MAPA_sla_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla_lag012<-n_vis_sla_lag012$res  # Extract residuals

#Reorder Treatments
n_Res_sla_lag012$Drought <- as.factor(n_Res_sla_lag012$Drought)
n_Res_sla_lag012$Drought <- factor(n_Res_sla_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N7<-ggplot(n_Res_sla_lag012, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA 3-Year Average)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N7 <-N7 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N7 <-N7 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N7
ggsave("Figures/N.MAPA_lag012_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag1
n_vis_ft_lag1<-visreg(n_MAPA_fl_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
n_Res_ft_lag1<-n_vis_ft_lag1$res  # Extract residuals
#Reorder Treatments
n_Res_ft_lag1$Drought <- as.factor(n_Res_ft_lag1$Drought)
n_Res_ft_lag1$Drought <- factor(n_Res_ft_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N8<-ggplot(n_Res_ft_lag1, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA lag1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N8 <-N8 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N8 <-N8  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N8
ggsave("Figures/N.MAPA_lag1_ft.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag2
n_vis_ft_lag2<-visreg(n_MAPA_fl_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_ft_lag2<-n_vis_ft_lag2$res  # Extract residuals
#Reorder Treatments
n_Res_ft_lag2$Drought <- as.factor(n_Res_ft_lag2$Drought)
n_Res_ft_lag2$Drought <- factor(n_Res_ft_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N9<-ggplot(n_Res_ft_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (lag2)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N9 <-N9 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N9 <-N9  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N9
ggsave("Figures/N.MAPA_lag2_ft.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag01
n_vis_ft_lag01<-visreg(n_MAPA_fl_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
n_Res_ft_lag01<-n_vis_ft_lag01$res  # Extract residuals
#Reorder Treatments
n_Res_ft_lag01$Drought <- as.factor(n_Res_ft_lag01$Drought)
n_Res_ft_lag01$Drought <- factor(n_Res_ft_lag01$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N10<-ggplot(n_Res_ft_lag01, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (lag01)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N10 <-N10 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N10 <-N10  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N10
ggsave("Figures/N.MAPA_lag01_ft.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag012
n_vis_ft_lag012<-visreg(n_MAPA_fl_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_ft_lag012<-n_vis_ft_lag012$res  # Extract residuals
#Reorder Treatments
n_Res_ft_lag012$Drought <- as.factor(n_Res_ft_lag012$Drought)
n_Res_ft_lag012$Drought <- factor(n_Res_ft_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N11<-ggplot(n_Res_ft_lag012, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (lag012)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N11 <-N11 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N11 <-N11  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N11
ggsave("Figures/N.MAPA_lag012_ft.pdf", width = 6, height = 6, units = "in")


############ CMDA ##  Drought*CMDA_lag1
#n_CMDA_fl_lag1_no_drought 
# CMDA Date of Flowering Lag1
n_cmda_vis_ft_lag1<-visreg(n_CMDA_fl_lag1_no_drought, xvar="CMDA_lag1") #set up visreg for Drought
n_cmda_Res_ft_lag1<-n_cmda_vis_ft_lag1$res # Extract residuals

#Use ggplot to generate plot with all required formating
N12<-ggplot(n_cmda_Res_ft_lag1, aes(CMDA_lag1, y=visregRes))+
  geom_jitter( size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  theme_classic()
N12 <-N12 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N12 <-N12 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N12
ggsave("Figures/N.CMDA_lag1_ft.pdf", width = 6, height = 6, units = "in")

#Cowplot
plot_grid(N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,ncol = 4)



###############################################################################
###############################################################################
### Centre####

############ SPEI ############
# SPEI SLA lag0 = c_sla_lag0
c_vis_sla<-visreg(c_sla_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
c_Res_sla<-c_vis_sla$res # Extract residuals

#Reorder Treatments
c_Res_sla$Drought <- as.factor(c_Res_sla$Drought)
c_Res_sla$Drought <- factor(c_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C1<-ggplot(c_Res_sla, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (lag 0)") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C1 <-C1 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C1 <-C1 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C1
ggsave("Figures/C.SPEI_lag0_SLA.pdf", width = 6, height = 6, units = "in")


# SPEI Date of SLA Lag1
c_vis_sla<-visreg(c_sla_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
c_Res_sla<-c_vis_sla$res  # Extract residuals

#Reorder Treatments
c_Res_sla$Drought <- as.factor(c_Res_sla$Drought)
c_Res_sla$Drought <- factor(c_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C2<-ggplot(c_Res_sla, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (lag 1)") +
  scale_y_continuous(name="Specific Leaf Area", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C2 <-C2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C2 <-C2  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C2
ggsave("Figures/C.SPEI_lag1_SLA.pdf", width = 6, height = 6, units = "in")

# SPEI Date of SLA Lag2
c_vis_lag2_sla<-visreg(c_sla_lag2, xvar="lag2", by="Drought") #set up visreg for Drought
c_Res_lag2_sla<-c_vis_lag2_sla$res  # Extract residuals

#Reorder Treatments
c_Res_lag2_sla$Drought <- as.factor(c_Res_lag2_sla$Drought)
c_Res_lag2_sla$Drought <- factor(c_Res_lag2_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C3<-ggplot(c_Res_lag2_sla, aes(lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (lag 2)") +
  scale_y_continuous(name="Specific Leaf Area", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C3 <-C3 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C3 <-C3  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C3
ggsave("Figures/C.SPEI_lag2_SLA.pdf", width = 6, height = 6, units = "in")

############ MATA ############
# MATA SLA lag0
c_vis_MATA_lag0<-visreg(c_MATA_sla_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
c_Res_MATA_lag0<-c_vis_MATA_lag0$res # Extract residuals

#Reorder Treatments
c_Res_MATA_lag0$Drought <- as.factor(c_Res_MATA_lag0$Drought)
c_Res_MATA_lag0$Drought <- factor(c_Res_MATA_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C4<-ggplot(c_Res_MATA_lag0, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA (lag 0)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C4 <-C4 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C4 <-C4 + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C4
ggsave("Figures/C.MATA_lag0_SLA.pdf", width = 6, height = 6, units = "in")

# MATA SLA lag1
c_vis_MATA_sla_lag1<-visreg(c_MATA_sla_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
c_Res_MATA_sla_lag1<-c_vis_MATA_sla_lag1$res  # Extract residuals

#Reorder Treatments
c_Res_MATA_sla_lag1$Drought <- as.factor(c_Res_MATA_sla_lag1$Drought)
c_Res_MATA_sla_lag1$Drought <- factor(c_Res_MATA_sla_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C5<-ggplot(c_Res_MATA_sla_lag1, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA lag 1") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C5 <-C5 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C5 <-C5 + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C5
ggsave("Figures/C.MATA_lag1_SLA.pdf", width = 6, height = 6, units = "in")



# MATA SLA lag2
c_vis_MATA_sla_lag2<-visreg(c_MATA_sla_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
c_Res_MATA_sla_lag2<-c_vis_MATA_sla_lag2$res  # Extract residuals

#Reorder Treatments
c_Res_MATA_sla_lag2$Drought <- as.factor(c_Res_MATA_sla_lag2$Drought)
c_Res_MATA_sla_lag2$Drought <- factor(c_Res_MATA_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C6<-ggplot(c_Res_MATA_sla_lag2, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA lag 2") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C6 <-C6 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C6 <-C6 + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C6
ggsave("Figures/C.MATA_lag2_SLA.pdf", width = 6, height = 6, units = "in")

# MATA SLA lag01
c_vis_MATA_sla_lag01<-visreg(c_MATA_sla_lag01, xvar="MATA_lag01", by="Drought") #set up visreg for Drought
c_Res_MATA_sla_lag01<-c_vis_MATA_sla_lag01$res  # Extract residuals

#Reorder Treatments
c_Res_MATA_sla_lag01$Drought <- as.factor(c_Res_MATA_sla_lag01$Drought)
c_Res_MATA_sla_lag01$Drought <- factor(c_Res_MATA_sla_lag01$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C7<-ggplot(c_Res_MATA_sla_lag01, aes(MATA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA lag 01") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C7 <-C7 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C7 <-C7 + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C7
ggsave("Figures/C.MATA_lag01_SLA.pdf", width = 6, height = 6, units = "in")



# MATA SLA lag012
c_vis_MATA_sla_lag012<-visreg(c_MATA_sla_lag012, xvar="MATA_lag012", by="Drought") #set up visreg for Drought
c_Res_MATA_sla_lag012<-c_vis_MATA_sla_lag012$res  # Extract residuals

#Reorder Treatments
c_Res_MATA_sla_lag012$Drought <- as.factor(c_Res_MATA_sla_lag012$Drought)
c_Res_MATA_sla_lag012$Drought <- factor(c_Res_MATA_sla_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C8<-ggplot(c_Res_MATA_sla_lag012, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA lag 01") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C8 <-C8 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C8 <-C8 + 
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C8
ggsave("Figures/C.MATA_lag012_SLA.pdf", width = 6, height = 6, units = "in")


############ MAPA ############

# MAPA SLA lag0
c_MAPA_vis_sla_lag0<-visreg(c_MAPA_sla_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
c_MAPA_Res_sla_lag0<-c_MAPA_vis_sla_lag0$res # Extract residuals

#Reorder Treatments
c_MAPA_Res_sla_lag0$Drought <- as.factor(c_MAPA_Res_sla_lag0$Drought)
c_MAPA_Res_sla_lag0$Drought <- factor(c_MAPA_Res_sla_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C8.5<-ggplot(c_MAPA_Res_sla_lag0, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (lag0)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C8.5 <-C8.5 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C8.5 <-C8.5 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C8.5
ggsave("Figures/C.MAPA_lag0_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA SLA lag1
c_MAPA_vis_sla_lag1<-visreg(c_MAPA_sla_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
c_MAPA_Res_sla_lag1<-c_MAPA_vis_sla_lag1$res # Extract residuals

#Reorder Treatments
c_MAPA_Res_sla_lag1$Drought <- as.factor(c_MAPA_Res_sla_lag1$Drought)
c_MAPA_Res_sla_lag1$Drought <- factor(c_MAPA_Res_sla_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C9<-ggplot(c_MAPA_Res_sla_lag1, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (lag1)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C9 <-C9 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C9 <-C9 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C9
ggsave("Figures/C.MAPA_lag1_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA SLA lag2
c_MAPA_vis_sla_lag2<-visreg(c_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
c_MAPA_Res_sla_lag2<-c_MAPA_vis_sla_lag2$res # Extract residuals

#Reorder Treatments
c_MAPA_Res_sla_lag2$Drought <- as.factor(c_MAPA_Res_sla_lag2$Drought)
c_MAPA_Res_sla_lag2$Drought <- factor(c_MAPA_Res_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C10<-ggplot(c_MAPA_Res_sla_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (lag2)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C10 <-C10 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C10 <-C10 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C10
ggsave("Figures/C.MAPA_lag2_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA SLA lag012
c_MAPA_vis_sla_lag012<-visreg(c_MAPA_sla_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
c_MAPA_Res_sla_lag012<-c_MAPA_vis_sla_lag012$res  # Extract residuals

#Reorder Treatments
c_MAPA_Res_sla_lag012$Drought <- as.factor(c_MAPA_Res_sla_lag012$Drought)
c_MAPA_Res_sla_lag012$Drought <- factor(c_MAPA_Res_sla_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C11<-ggplot(c_MAPA_Res_sla_lag012, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA 3-Year Average)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C11 <-C11 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C11 <-C11 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C11
ggsave("Figures/C.MAPA_lag012_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag0
c_MAPA_vis_ft_lag0<-visreg(c_MAPA_fl_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
c_MAPA_Res_ft_lag0<-c_MAPA_vis_ft_lag0$res  # Extract residuals
#Reorder Treatments
c_MAPA_Res_ft_lag0$Drought <- as.factor(c_MAPA_Res_ft_lag0$Drought)
c_MAPA_Res_ft_lag0$Drought <- factor(c_MAPA_Res_ft_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C12<-ggplot(c_MAPA_Res_ft_lag0, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA lag0)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C12 <-C12 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C12 <-C12  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C12
ggsave("Figures/C.MAPA_lag0_ft.pdf", width = 6, height = 6, units = "in")



# MAPA Date of Flowering Lag1
c_MAPA_vis_ft_lag1<-visreg(c_MAPA_fl_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
c_MAPA_Res_ft_lag1<-c_MAPA_vis_ft_lag1$res  # Extract residuals
#Reorder Treatments
c_MAPA_Res_ft_lag1$Drought <- as.factor(c_MAPA_Res_ft_lag1$Drought)
c_MAPA_Res_ft_lag1$Drought <- factor(c_MAPA_Res_ft_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C13<-ggplot(c_MAPA_Res_ft_lag1, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA lag1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C13 <-C13 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C13 <-C13  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C13
ggsave("Figures/C.MAPA_lag1_ft.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag2
C_MAPA_vis_ft_lag2<-visreg(c_MAPA_fl_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
C_MAPA_Res_ft_lag2<-C_MAPA_vis_ft_lag2$res  # Extract residuals
#Reorder Treatments
C_MAPA_Res_ft_lag2$Drought <- as.factor(C_MAPA_Res_ft_lag2$Drought)
C_MAPA_Res_ft_lag2$Drought <- factor(C_MAPA_Res_ft_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C14<-ggplot(C_MAPA_Res_ft_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (lag2)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C14 <-C14 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C14 <-C14  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C14
ggsave("Figures/C.MAPA_lag2_ft.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag01
C_MAPA_vis_ft_lag01<-visreg(c_MAPA_fl_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
C_MAPA_Res_ft_lag01<-C_MAPA_vis_ft_lag01$res  # Extract residuals
#Reorder Treatments
C_MAPA_Res_ft_lag01$Drought <- as.factor(C_MAPA_Res_ft_lag01$Drought)
C_MAPA_Res_ft_lag01$Drought <- factor(C_MAPA_Res_ft_lag01$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C15<-ggplot(C_MAPA_Res_ft_lag01, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (2-year average)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C15 <-C15 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C15 <-C15  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C15
ggsave("Figures/C.MAPA_lag01_ft.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag012
C_MAPA_vis_ft_lag012<-visreg(c_MAPA_fl_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
C_MAPA_Res_ft_lag012<-C_MAPA_vis_ft_lag012$res  # Extract residuals
#Reorder Treatments
C_MAPA_Res_ft_lag012$Drought <- as.factor(C_MAPA_Res_ft_lag012$Drought)
C_MAPA_Res_ft_lag012$Drought <- factor(C_MAPA_Res_ft_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C16<-ggplot(C_MAPA_Res_ft_lag012, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (3-yr average)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C16 <-C16 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C16 <-C16  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C16
ggsave("Figures/C.MAPA_lag012_ft.pdf", width = 6, height = 6, units = "in")


############ CMDA ##  Drought*CMDA_lag0
#
C_cmda_vis_sla_lag0<-visreg(c_CMDA_sla_lag0, xvar="CMDA_lag0", by="Drought") #set up visreg for Drought
C_cmda_Res_sla_lag0<-C_cmda_vis_sla_lag0$res  # Extract residuals
#Reorder Treatments
C_cmda_Res_sla_lag0$Drought <- as.factor(C_cmda_Res_sla_lag0$Drought)
C_cmda_Res_sla_lag0$Drought <- factor(C_cmda_Res_sla_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C17<-ggplot(C_cmda_Res_sla_lag0, aes(CMDA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA (lag0)") +
  scale_y_continuous(name="Specific Leaf Area", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C17 <-C17 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C17 <-C17  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C17
ggsave("Figures/c.CMDA_lag0_sla.pdf", width = 6, height = 6, units = "in")


############ CMDA ##  Drought*CMDA_lag1
#
C_cmda_vis_sla_lag1<-visreg(c_CMDA_sla_lag1, xvar="CMDA_lag1", by="Drought") #set up visreg for Drought
C_cmda_Res_sla_lag1<-C_cmda_vis_sla_lag1$res  # Extract residuals
#Reorder Treatments
C_cmda_Res_sla_lag1$Drought <- as.factor(C_cmda_Res_sla_lag1$Drought)
C_cmda_Res_sla_lag1$Drought <- factor(C_cmda_Res_sla_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C18<-ggplot(C_cmda_Res_sla_lag1, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA (lag1)") +
  scale_y_continuous(name="Specific Leaf Area", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C18 <-C18 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C18 <-C18  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C18
ggsave("Figures/c.CMDA_lag1_sla.pdf", width = 6, height = 6, units = "in")

#Cowplot
plot_grid(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,ncol = 4)
plot_grid(C13,C14,C15,C16,C17,C18,ncol = 3)


##############################################################################
############ South ##  

#Drought*SPEI_lag0
s_vis_sla_lag0<-visreg(s_sla_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
s_Res_sla_lag0<-s_vis_sla_lag0$res  # Extract residuals
#Reorder Treatments
s_Res_sla_lag0$Drought <- as.factor(s_Res_sla_lag0$Drought)
s_Res_sla_lag0$Drought <- factor(s_Res_sla_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S1<-ggplot(s_Res_sla_lag0, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (lag0)") +
  scale_y_continuous(name="Specific Leaf Area", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S1 <-S1 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S1 <-S1  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S1
ggsave("Figures/s.SPEI_lag0_sla.pdf", width = 6, height = 6, units = "in")

#Drought*SPEI_lag012
s_vis_sla_lag012<-visreg(s_sla_lag012, xvar="lag012", by="Drought") #set up visreg for Drought
s_Res_sla_lag012<-s_vis_sla_lag012$res  # Extract residuals
#Reorder Treatments
s_Res_sla_lag012$Drought <- as.factor(s_Res_sla_lag012$Drought)
s_Res_sla_lag012$Drought <- factor(s_Res_sla_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S2<-ggplot(s_Res_sla_lag012, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI (3-yr average)") +
  scale_y_continuous(name="Specific Leaf Area", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S2 <-S2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S2 <-S2  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S2
ggsave("Figures/s.SPEI_lag012_sla.pdf", width = 6, height = 6, units = "in")


#Drought*MATA_lag0
s_mata_vis_sla_lag0<-visreg(s_MATA_sla_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
s_mata_Res_sla_lag0<-s_mata_vis_sla_lag0$res  # Extract residuals
#Reorder Treatments
s_mata_Res_sla_lag0$Drought <- as.factor(s_mata_Res_sla_lag0$Drought)
s_mata_Res_sla_lag0$Drought <- factor(s_mata_Res_sla_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S3<-ggplot(s_mata_Res_sla_lag0, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA (lag0)") +
  scale_y_continuous(name="Specific Leaf Area", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S3 <-S3 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S3 <-S3  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S3
ggsave("Figures/s.MATA_lag0_sla.pdf", width = 6, height = 6, units = "in")


#Drought*MATA_lag2 flower
s_mata_vis_ft_lag2<-visreg(s_MATA_fl_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
s_mata_Res_ft_lag2<-s_mata_vis_ft_lag2$res  # Extract residuals
#Reorder Treatments
s_mata_Res_ft_lag2$Drought <- as.factor(s_mata_Res_ft_lag2$Drought)
s_mata_Res_ft_lag2$Drought <- factor(s_mata_Res_ft_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S4<-ggplot(s_mata_Res_ft_lag2, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA (lag2)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S4 <-S4 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S4 <-S4  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S4
ggsave("Figures/s.MATA_lag2_ft.pdf", width = 6, height = 6, units = "in")




#Drought*MAPA_lag0
s_mapa_vis_sla_lag012<-visreg(s_MAPA_sla_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
s_mapa_Res_sla_lag012<-s_mapa_vis_sla_lag012$res  # Extract residuals
#Reorder Treatments
s_mapa_Res_sla_lag012$Drought <- as.factor(s_mapa_Res_sla_lag012$Drought)
s_mapa_Res_sla_lag012$Drought <- factor(s_mapa_Res_sla_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S5<-ggplot(s_mapa_Res_sla_lag012, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (3-yr average)") +
  scale_y_continuous(name="Specific Leaf Area", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S5 <-S5 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S5 <-S5  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S5
ggsave("Figures/s.MAPA_lag012_sla.pdf", width = 6, height = 6, units = "in")


#Drought*MATA_lag2 flower
s_mapa_vis_ft_lag2<-visreg(s_MAPA_fl_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
s_mapa_Res_ft_lag2<-s_mapa_vis_ft_lag2$res  # Extract residuals
#Reorder Treatments
s_mapa_Res_ft_lag2$Drought <- as.factor(s_mapa_Res_ft_lag2$Drought)
s_mapa_Res_ft_lag2$Drought <- factor(s_mapa_Res_ft_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S6<-ggplot(s_mapa_Res_ft_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (lag2)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S6 <-S6 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S6 <-S6  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S6
ggsave("Figures/s.MAPA_lag2_ft.pdf", width = 6, height = 6, units = "in")



#Drought*MATPA_lag012 flower
s_mapa_vis_ft_lag012<-visreg(s_MAPA_fl_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
s_mapa_Res_ft_lag012<-s_mapa_vis_ft_lag012$res  # Extract residuals
#Reorder Treatments
s_mapa_Res_ft_lag012$Drought <- as.factor(s_mapa_Res_ft_lag012$Drought)
s_mapa_Res_ft_lag012$Drought <- factor(s_mapa_Res_ft_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S7<-ggplot(s_mapa_Res_ft_lag012, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA (3-year average)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S7 <-S7 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S7 <-S7  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S7
ggsave("Figures/s.MAPA_lag012_ft.pdf", width = 6, height = 6, units = "in")

#Cowplot
plot_grid(S1,S2,S3,S4,S5,S6,S7,ncol = 3)
