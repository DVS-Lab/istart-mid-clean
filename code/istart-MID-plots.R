# set working directory
setwd("~/Desktop")
maindir <- getwd()
#datadir <- file.path("~/Documents/Github/rf1-sra-socdoors/code")

# load packages
library("readxl")
library("ggplot2")
library("ggpubr")

# import data
data <- read_xlsx("~/Desktop/projects/MID/total_df_n46.xlsx")

# Fig 2 B1: TEPS x Behavioral Motivation
scatter <- ggplot(data, aes(x=score_teps_ant,V_beta))+
  geom_point(colour="blue")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
  labs(x="TEPS (Ant)",y="Behavioral Motivation")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 2 B2: RS x Behavioral Motivation
scatter <- ggplot(data, aes(x=comp_RS,y=V_beta))+
  geom_point(colour="blue")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
  labs(x="Reward Sensitivity",y="Behavioral Motivation")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 2 B2: TEPS x RS x Behavioral Motivation
scatter <- ggplot(data, aes(x=comp_RS, y=V_beta, col=teps_ant_split))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
  labs(x="Reward Sensitivity",y="Behavioral Motivation")+
  stat_cor(method="pearson")
scatter + scale_color_manual(values = c("orange", "blue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 3 B1: LL>SL x Reward Sensitivity
scatter <- ggplot(data, aes(x=comp_RS,y=Act_LL_minus_SL))+
  geom_point(colour="blue")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
  labs(x="Reward Sensitivity",y="LL > SL")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 3 B2: LG>SG x Reward Sensitivity
scatter <- ggplot(data, aes(x=comp_RS,y=Act_LG_minus_SG))+
  geom_point(colour="blue")+
  geom_smooth(method=lm, formula= y ~ poly(x,2), se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
  labs(x="Reward Sensitivity",y="LG > SG")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 3 B3: Salience x Reward Sensitivity
scatter <- ggplot(data, aes(x=comp_RS,y=Act_Salience))+
  geom_point(colour="blue")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
  labs(x="Reward Sensitivity",y="Salience")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 3 C1: LL>SL x TEPS (Ant)
scatter <- ggplot(data, aes(x=score_teps_ant,y=Act_LL_minus_SL))+
  geom_point(colour="blue")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
  labs(x="TEPS (Ant)",y="LL > SL")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 3 C2: LG>SG x TEPS (Ant)
scatter <- ggplot(data, aes(x=score_teps_ant,y=Act_LG_minus_SG))+
  geom_point(colour="blue")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
  labs(x="TEPS (Ant)",y="LG > SG")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 3 C3: Salience x TEPS
scatter <- ggplot(data, aes(x=score_teps_ant,y=Act_Salience))+
  geom_point(colour="blue")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
  labs(x="TEPS (Ant)",y="Salience")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))
