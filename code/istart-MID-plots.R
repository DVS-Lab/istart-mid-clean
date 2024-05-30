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
covs <- read_xlsx("~/Desktop/projects/MID/ISTART-ALL-Combined-042122_MID-subs.xlsx")

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
