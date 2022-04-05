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
  xlab("SPEI Lag 0") +
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
  xlab("SPEI Lag 1") +
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


############ MAPA ############
# MAPA SLA lag0
n_vis_sla_lag0<-visreg(n_MAPA_sla_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla_lag0<-n_vis_sla_lag0$res # Extract residuals

#Reorder Treatments
n_Res_sla_lag0$Drought <- as.factor(n_Res_sla_lag0$Drought)
n_Res_sla_lag0$Drought <- factor(n_Res_sla_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla_lag0, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA Lag 0)") +
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
ggsave("Figures/N.MAPA_lag0_SLA.pdf", width = 6, height = 6, units = "in")



# MAPA SLA lag2
n_vis_sla_lag2<-visreg(n_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla_lag2<-n_vis_sla_lag2$res # Extract residuals

#Reorder Treatments
n_Res_sla_lag2$Drought <- as.factor(n_Res_sla_lag2$Drought)
n_Res_sla_lag2$Drought <- factor(n_Res_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA Lag 2)") +
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
ggsave("Figures/N.MAPA_lag2_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA Date of Flowering Lag1
n_vis_ft_lag1<-visreg(n_MAPA_fl_lag1a, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
n_Res_ft_lag1<-n_vis_ft_lag1$res  # Extract residuals
#Reorder Treatments
n_Res_ft_lag1$Drought <- as.factor(n_Res_ft_lag1$Drought)
n_Res_ft_lag1$Drought <- factor(n_Res_ft_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N5<-ggplot(n_Res_ft_lag1, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N5 <-N5 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N5 <-N5  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N5
ggsave("Figures/N.MAPA_lag1_ft.pdf", width = 6, height = 6, units = "in")


############ MATA ############

# MATA Date of Flowering Lag1
n_vis_ft_lag1<-visreg(n_MATA_fl_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_ft_lag1<-n_vis_ft_lag1$res # Extract residuals

#Reorder Treatments
n_Res_ft_lag1$Drought <- as.factor(n_Res_ft_lag1$Drought)
n_Res_ft_lag1$Drought <- factor(n_Res_ft_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N6<-ggplot(n_Res_ft_lag1, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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
ggsave("Figures/N.MATA_lag1_ft.pdf", width = 6, height = 6, units = "in")



#Cowplot
plot_grid(N1,N2,N3,N4,N5,N6,ncol = 2) #save 10x14



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
  xlab("SPEI Lag 0") +
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
  xlab("SPEI Lag 1") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
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
  xlab("SPEI Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
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



############ MAPA ############

# MAPA SLA lag0
c_MAPA_vis_sla_lag1<-visreg(c_MAPA_sla_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
c_MAPA_Res_sla_lag1<-c_MAPA_vis_sla_lag1$res # Extract residuals

#Reorder Treatments
c_MAPA_Res_sla_lag1$Drought <- as.factor(c_MAPA_Res_sla_lag1$Drought)
c_MAPA_Res_sla_lag1$Drought <- factor(c_MAPA_Res_sla_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C4<-ggplot(c_MAPA_Res_sla_lag1, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) Lag 1") +
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
#ggsave("Figures/C.MAPA_lag0_SLA.pdf", width = 6, height = 6, units = "in")


# MAPA SLA lag2
c_MAPA_vis_sla_lag2<-visreg(c_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
c_MAPA_Res_sla_lag2<-c_MAPA_vis_sla_lag2$res # Extract residuals

#Reorder Treatments
c_MAPA_Res_sla_lag2$Drought <- as.factor(c_MAPA_Res_sla_lag2$Drought)
c_MAPA_Res_sla_lag2$Drought <- factor(c_MAPA_Res_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C5<-ggplot(c_MAPA_Res_sla_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) Lag 2") +
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
ggsave("Figures/C.MAPA_lag2_SLA.pdf", width = 6, height = 6, units = "in")

# MAPA Date of Flowering Lag0
c_MAPA_vis_ft_lag0<-visreg(c_MAPA_fl_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
c_MAPA_Res_ft_lag0<-c_MAPA_vis_ft_lag0$res  # Extract residuals
#Reorder Treatments
c_MAPA_Res_ft_lag0$Drought <- as.factor(c_MAPA_Res_ft_lag0$Drought)
c_MAPA_Res_ft_lag0$Drought <- factor(c_MAPA_Res_ft_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C6<-ggplot(c_MAPA_Res_ft_lag0, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C6 <-C6 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C6 <-C6  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C6

# MAPA Date of Flowering Lag2
C_MAPA_vis_ft_lag2<-visreg(c_MAPA_fl_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
C_MAPA_Res_ft_lag2<-C_MAPA_vis_ft_lag2$res  # Extract residuals
#Reorder Treatments
C_MAPA_Res_ft_lag2$Drought <- as.factor(C_MAPA_Res_ft_lag2$Drought)
C_MAPA_Res_ft_lag2$Drought <- factor(C_MAPA_Res_ft_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C7<-ggplot(C_MAPA_Res_ft_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C7 <-C7 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C7 <-C7  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C7
ggsave("Figures/C.MAPA_lag2_ft.pdf", width = 6, height = 6, units = "in")



############ MATA ############
# MATA SLA lag0
c_vis_MATA_lag0<-visreg(c_MATA_sla_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
c_Res_MATA_lag0<-c_vis_MATA_lag0$res # Extract residuals

#Reorder Treatments
c_Res_MATA_lag0$Drought <- as.factor(c_Res_MATA_lag0$Drought)
c_Res_MATA_lag0$Drought <- factor(c_Res_MATA_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C8<-ggplot(c_Res_MATA_lag0, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 0") +
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
ggsave("Figures/C.MATA_lag0_SLA.pdf", width = 6, height = 6, units = "in")

# MATA SLA lag1
c_vis_MATA_sla_lag1<-visreg(c_MATA_sla_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
c_Res_MATA_sla_lag1<-c_vis_MATA_sla_lag1$res  # Extract residuals

#Reorder Treatments
c_Res_MATA_sla_lag1$Drought <- as.factor(c_Res_MATA_sla_lag1$Drought)
c_Res_MATA_sla_lag1$Drought <- factor(c_Res_MATA_sla_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C9<-ggplot(c_Res_MATA_sla_lag1, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
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
ggsave("Figures/C.MATA_lag1_SLA.pdf", width = 6, height = 6, units = "in")



# MATA SLA lag2
c_vis_MATA_sla_lag2<-visreg(c_MATA_sla_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
c_Res_MATA_sla_lag2<-c_vis_MATA_sla_lag2$res  # Extract residuals

#Reorder Treatments
c_Res_MATA_sla_lag2$Drought <- as.factor(c_Res_MATA_sla_lag2$Drought)
c_Res_MATA_sla_lag2$Drought <- factor(c_Res_MATA_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C10<-ggplot(c_Res_MATA_sla_lag2, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 2") +
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
ggsave("Figures/C.MATA_lag2_SLA.pdf", width = 6, height = 6, units = "in")


# MATA SLA lag012
c_vis_MATA_sla_lag012<-visreg(c_MATA_sla_lag012, xvar="MATA_lag012", by="Drought") #set up visreg for Drought
c_Res_MATA_sla_lag012<-c_vis_MATA_sla_lag012$res  # Extract residuals

#Reorder Treatments
c_Res_MATA_sla_lag012$Drought <- as.factor(c_Res_MATA_sla_lag012$Drought)
c_Res_MATA_sla_lag012$Drought <- factor(c_Res_MATA_sla_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
C11<-ggplot(c_Res_MATA_sla_lag012, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("Average 3-Year MATA") +
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
ggsave("Figures/C.MATA_lag012_SLA.pdf", width = 6, height = 6, units = "in")


#Cowplot
plot_grid(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,ncol = 4) #saved 14x10



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
  xlab("SPEI Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
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
  xlab("Cumulative 3-Year SPEI") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
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


#Drought*MAPA_lag0
s_mapa_vis_sla_lag0<-visreg(s_MAPA_sla_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
s_mapa_Res_sla_lag0<-s_mapa_vis_sla_lag0$res  # Extract residuals
#Reorder Treatments
s_mapa_Res_sla_lag0$Drought <- as.factor(s_mapa_Res_sla_lag0$Drought)
s_mapa_Res_sla_lag0$Drought <- factor(s_mapa_Res_sla_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S3<-ggplot(s_mapa_Res_sla_lag0, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
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
ggsave("Figures/s.MAPA_lag012_sla.pdf", width = 6, height = 6, units = "in")



#Drought*MAPA sla lag2
s_mapa_vis_sla_lag2<-visreg(s_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
s_mapa_Res_sla_lag2<-s_mapa_vis_sla_lag2$res  # Extract residuals
#Reorder Treatments
s_mapa_Res_sla_lag2$Drought <- as.factor(s_mapa_Res_sla_lag2$Drought)
s_mapa_Res_sla_lag2$Drought <- factor(s_mapa_Res_sla_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S4<-ggplot(s_mapa_Res_sla_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
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
ggsave("Figures/s.MAPA_lag2_ft.pdf", width = 6, height = 6, units = "in")


#Drought*MAPA sla lag01
s_mapa_vis_sla_lag01<-visreg(s_MAPA_sla_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
s_mapa_Res_sla_lag01<-s_mapa_vis_sla_lag01$res  # Extract residuals
#Reorder Treatments
s_mapa_Res_sla_lag01$Drought <- as.factor(s_mapa_Res_sla_lag01$Drought)
s_mapa_Res_sla_lag01$Drought <- factor(s_mapa_Res_sla_lag01$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S5<-ggplot(s_mapa_Res_sla_lag01, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) 2-Years") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
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

#Drought*MAPA_lag012 flower
s_mapa_vis_sla_lag012<-visreg(s_MAPA_sla_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
s_mapa_Res_sla_lag012<-s_mapa_vis_sla_lag012$res  # Extract residuals
#Reorder Treatments
s_mapa_Res_sla_lag012$Drought <- as.factor(s_mapa_Res_sla_lag012$Drought)
s_mapa_Res_sla_lag012$Drought <- factor(s_mapa_Res_sla_lag012$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S6<-ggplot(s_mapa_Res_sla_lag012, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) 3-Years") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
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
ggsave("Figures/s.MAPA_lag012_ft.pdf", width = 6, height = 6, units = "in")

#Drought*MAPA fl lag2
s_mapa_vis_fl_lag2<-visreg(s_MAPA_fl_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
s_mapa_Res_fl_lag2<-s_mapa_vis_fl_lag2$res  # Extract residuals
#Reorder Treatments
s_mapa_Res_fl_lag2$Drought <- as.factor(s_mapa_Res_fl_lag2$Drought)
s_mapa_Res_fl_lag2$Drought <- factor(s_mapa_Res_fl_lag2$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S7<-ggplot(s_mapa_Res_fl_lag2, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("log(MAPA) Lag 2") +
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

#Drought*MATA_lag0
s_mata_vis_sla_lag0<-visreg(s_MATA_sla_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
s_mata_Res_sla_lag0<-s_mata_vis_sla_lag0$res  # Extract residuals
#Reorder Treatments
s_mata_Res_sla_lag0$Drought <- as.factor(s_mata_Res_sla_lag0$Drought)
s_mata_Res_sla_lag0$Drought <- factor(s_mata_Res_sla_lag0$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S8<-ggplot(s_mata_Res_sla_lag0, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S8 <-S8 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S8 <-S8  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S8
ggsave("Figures/s.MATA_lag0_sla.pdf", width = 6, height = 6, units = "in")


#Drought*MATA_lag1
s_mata_vis_sla_lag1<-visreg(s_MATA_sla_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
s_mata_Res_sla_lag1<-s_mata_vis_sla_lag1$res  # Extract residuals
#Reorder Treatments
s_mata_Res_sla_lag1$Drought <- as.factor(s_mata_Res_sla_lag1$Drought)
s_mata_Res_sla_lag1$Drought <- factor(s_mata_Res_sla_lag1$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
S9<-ggplot(s_mata_Res_sla_lag1, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
S9 <-S9 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
S9 <-S9  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
S9

#Cowplot
plot_grid(S1,S2,S3,S4,S5,S6,S7,S8,S9,ncol = 3)

##############################################################################
############ Cowplot organized pannels ## NOT final figures


##SLA Pannels
plot_grid(N1,C1,C2,C3,S1,S2,ncol = 3) #SPEI
plot_grid(N5.5,N6,N7,C9,C10,C11,S5,ncol = 3) #MAPA
plot_grid(N3,N4,C4,C5,C6,C7,C8,S3,ncol = 3) #MATA

## Date of Flowering Pannels
plot_grid(N2,N12,S4,N5,ncol = 2) #Random
plot_grid(N8,N10,N11,C12,C14,C15,C16,S6,S7,ncol = 3) #MAPA




