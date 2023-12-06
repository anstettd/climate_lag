#### All regional graphs ######

### North SPEI SLA ####

# SPEI SLA lag0 = n_sla_lag0
n_vis_sla<-visreg(n_sla_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_sla_lag1
n_vis_sla<-visreg(n_sla_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(n_sla_lag2, xvar="lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(n_sla_lag01, xvar="lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(n_sla_lag012, xvar="lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# SPEI Date of Flowering North ####
# lag0
n_vis_ft<-visreg(n_fl_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(n_fl_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(n_fl_lag2, xvar="lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(n_fl_lag01, xvar="lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(n_fl_lag012, xvar="lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)



### Centre SPEI SLA ####

# SPEI SLA lag0 = c_sla_lag0
n_vis_sla<-visreg(c_sla_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = c_sla_lag1
n_vis_sla<-visreg(c_sla_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 1") +
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

# SPEI SLA lag2 = c_sla_lag2
n_vis_sla<-visreg(c_sla_lag2, xvar="lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(c_sla_lag01, xvar="lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(c_sla_lag012, xvar="lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# SPEI Date of Flowering centre ####
# lag0
n_vis_ft<-visreg(c_fl_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = c_fl_lag1
n_vis_sla<-visreg(c_fl_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = c_fl_lag2
n_vis_sla<-visreg(c_fl_lag2, xvar="lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = c_fl_lag01
n_vis_sla<-visreg(c_fl_lag01, xvar="lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = c_fl_lag012
n_vis_sla<-visreg(c_fl_lag012, xvar="lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)


### South SPEI SLA ####

# SPEI SLA lag0 = s_sla_lag0
n_vis_sla<-visreg(s_sla_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = s_sla_lag1
n_vis_sla<-visreg(s_sla_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 1") +
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

# SPEI SLA lag2 = s_sla_lag2
n_vis_sla<-visreg(s_sla_lag2, xvar="lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = s_sla_lag01
n_vis_sla<-visreg(s_sla_lag01, xvar="lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 2-Year") +
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

# SPEI SLA lag012 = s_sla_lag012
n_vis_sla<-visreg(s_sla_lag012, xvar="lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# SPEI Date of Flowering South ####
# lag0
n_vis_ft<-visreg(s_fl_lag0, xvar="lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = s_fl_lag1
n_vis_sla<-visreg(s_fl_lag1, xvar="lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = s_fl_lag2
n_vis_sla<-visreg(s_fl_lag2, xvar="lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = s_fl_lag01
n_vis_sla<-visreg(s_fl_lag01, xvar="lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = s_fl_lag012
n_vis_sla<-visreg(s_fl_lag012, xvar="lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)


### North CMDA SLA ####

# CMDA SLA lag0 = n_CMDA_sla_lag0
n_vis_sla<-visreg(n_CMDA_sla_lag0, xvar="CMDA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(CMDA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_CMDA_sla_lag1
n_vis_sla<-visreg(n_CMDA_sla_lag1, xvar="CMDA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(n_CMDA_sla_lag2, xvar="CMDA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(n_CMDA_sla_lag01, xvar="CMDA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(CMDA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(n_CMDA_sla_lag012, xvar="CMDA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(CMDA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# CMDA Date of Flowering North ####
# lag0
n_vis_ft<-visreg(n_CMDA_fl_lag0, xvar="CMDA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(CMDA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(n_CMDA_fl_lag1, xvar="CMDA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(n_CMDA_fl_lag2, xvar="CMDA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(n_CMDA_fl_lag01, xvar="CMDA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(CMDA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(n_CMDA_fl_lag012, xvar="CMDA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(CMDA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)


### Centre CMDA SLA ####

# CMDA SLA lag0 = n_CMDA_sla_lag0
n_vis_sla<-visreg(c_CMDA_sla_lag0, xvar="CMDA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(CMDA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_CMDA_sla_lag1
n_vis_sla<-visreg(c_CMDA_sla_lag1, xvar="CMDA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(c_CMDA_sla_lag2, xvar="CMDA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(c_CMDA_sla_lag01, xvar="CMDA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(CMDA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(c_CMDA_sla_lag012, xvar="CMDA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(CMDA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# CMDA Date of Flowering North ####
# lag0
n_vis_ft<-visreg(c_CMDA_fl_lag0, xvar="CMDA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(CMDA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(c_CMDA_fl_lag1, xvar="CMDA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(c_CMDA_fl_lag2, xvar="CMDA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(c_CMDA_fl_lag01, xvar="CMDA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(CMDA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(c_CMDA_fl_lag012, xvar="CMDA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(CMDA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)



### South CMDA SLA ####

# CMDA SLA lag0 = n_CMDA_sla_lag0
n_vis_sla<-visreg(s_CMDA_sla_lag0, xvar="CMDA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(CMDA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_CMDA_sla_lag1
n_vis_sla<-visreg(s_CMDA_sla_lag1, xvar="CMDA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(s_CMDA_sla_lag2, xvar="CMDA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(s_CMDA_sla_lag01, xvar="CMDA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(CMDA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(s_CMDA_sla_lag012, xvar="CMDA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(CMDA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# CMDA Date of Flowering North ####
# lag0
n_vis_ft<-visreg(s_CMDA_fl_lag0, xvar="CMDA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(CMDA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(s_CMDA_fl_lag1, xvar="CMDA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(s_CMDA_fl_lag2, xvar="CMDA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(s_CMDA_fl_lag01, xvar="CMDA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(CMDA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(s_CMDA_fl_lag012, xvar="CMDA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(CMDA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("CMDA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)



### North MAPA SLA ####

# SPEI SLA lag0 = n_MAPA_sla_lag0
n_vis_sla<-visreg(n_MAPA_sla_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_sla_lag1
n_vis_sla<-visreg(n_MAPA_sla_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(n_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(n_MAPA_sla_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(n_MAPA_sla_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# MAPA Date of Flowering North ####
# lag0
n_vis_ft<-visreg(n_MAPA_fl_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(n_MAPA_fl_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(n_MAPA_fl_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(n_MAPA_fl_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(n_MAPA_fl_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)


### Centre MAPA SLA ####

# SPEI SLA lag0 = n_MAPA_sla_lag0
n_vis_sla<-visreg(c_MAPA_sla_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_sla_lag1
n_vis_sla<-visreg(c_MAPA_sla_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(c_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(c_MAPA_sla_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(c_MAPA_sla_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# MAPA Date of Flowering North ####
# lag0
n_vis_ft<-visreg(c_MAPA_fl_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(c_MAPA_fl_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(c_MAPA_fl_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(c_MAPA_fl_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(c_MAPA_fl_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)



### South MAPA SLA ####

# SPEI SLA lag0 = n_MAPA_sla_lag0
n_vis_sla<-visreg(s_MAPA_sla_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_sla_lag1
n_vis_sla<-visreg(s_MAPA_sla_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(s_MAPA_sla_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(s_MAPA_sla_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(s_MAPA_sla_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# MAPA Date of Flowering North ####
# lag0
n_vis_ft<-visreg(s_MAPA_fl_lag0, xvar="MAPA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(MAPA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(s_MAPA_fl_lag1, xvar="MAPA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MAPA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(s_MAPA_fl_lag2, xvar="MAPA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MAPA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(s_MAPA_fl_lag01, xvar="MAPA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MAPA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(s_MAPA_fl_lag012, xvar="MAPA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MAPA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)


### North MATA SLA ####

# SPEI SLA lag0 = n_sla_lag0
n_vis_sla<-visreg(n_MATA_sla_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_sla_lag1
n_vis_sla<-visreg(n_MATA_sla_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(n_MATA_sla_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(n_MATA_sla_lag01, xvar="MATA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MATA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(n_MATA_sla_lag012, xvar="MATA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# SPEI Date of Flowering North ####
# lag0
n_vis_ft<-visreg(n_MATA_fl_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(n_MATA_fl_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(n_MATA_fl_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(n_MATA_fl_lag01, xvar="MATA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MATA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(n_MATA_fl_lag012, xvar="MATA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)



### Centre MATA SLA ####

# SPEI SLA lag0 = n_sla_lag0
n_vis_sla<-visreg(c_MATA_sla_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_sla_lag1
n_vis_sla<-visreg(c_MATA_sla_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(c_MATA_sla_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(c_MATA_sla_lag01, xvar="MATA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MATA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(c_MATA_sla_lag012, xvar="MATA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# SPEI Date of Flowering North ####
# lag0
n_vis_ft<-visreg(c_MATA_fl_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(c_MATA_fl_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(c_MATA_fl_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(c_MATA_fl_lag01, xvar="MATA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MATA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(c_MATA_fl_lag012, xvar="MATA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)


### South MATA SLA ####

# SPEI SLA lag0 = n_sla_lag0
n_vis_sla<-visreg(s_MATA_sla_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_sla, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0

# SPEI SLA lag1 = n_sla_lag1
n_vis_sla<-visreg(s_MATA_sla_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
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

# SPEI SLA lag2 = n_sla_lag2
n_vis_sla<-visreg(s_MATA_sla_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 2") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI SLA lag01 = n_sla_lag01
n_vis_sla<-visreg(s_MATA_sla_lag01, xvar="MATA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MATA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 2-Year") +
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

# SPEI SLA lag012 = n_sla_lag012
n_vis_sla<-visreg(s_MATA_sla_lag012, xvar="MATA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 3-Year") +
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)

#save 8x8 

# SPEI Date of Flowering North ####
# lag0
n_vis_ft<-visreg(s_MATA_fl_lag0, xvar="MATA_lag0", by="Drought") #set up visreg for Drought
n_Res_ft<-n_vis_ft$res  # Extract residuals

#Reorder Treatments
n_Res_ft$Drought <- as.factor(n_Res_ft$Drought)
n_Res_ft$Drought <- factor(n_Res_ft$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N0<-ggplot(n_Res_ft, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 0") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N0 <-N0 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N0 <-N0  +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N0


# SPEI FT lag1 = n_fl_lag1
n_vis_sla<-visreg(s_MATA_fl_lag1, xvar="MATA_lag1", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N1<-ggplot(n_Res_sla, aes(MATA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 1") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag2 = n_fl_lag2
n_vis_sla<-visreg(s_MATA_fl_lag2, xvar="MATA_lag2", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N2<-ggplot(n_Res_sla, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA Lag 2") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
N2 <-N2 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
N2 <-N2 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
N2

# SPEI fl lag01 = n_fl_lag01
n_vis_sla<-visreg(s_MATA_fl_lag01, xvar="MATA_lag01", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N3<-ggplot(n_Res_sla, aes(MATA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 2-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

# SPEI fl lag012 = n_fl_lag012
n_vis_sla<-visreg(s_MATA_fl_lag012, xvar="MATA_lag012", by="Drought") #set up visreg for Drought
n_Res_sla<-n_vis_sla$res # Extract residuals

#Reorder Treatments
n_Res_sla$Drought <- as.factor(n_Res_sla$Drought)
n_Res_sla$Drought <- factor(n_Res_sla$Drought, levels=c("W", "D"))

#Use ggplot to generate plot with all required formating
N4<-ggplot(n_Res_sla, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("MATA 3-Year") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
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

plot_grid(N0,N1,N2,N3,N4, ncol=2)
