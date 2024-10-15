# set working directory
setwd("~/Desktop")
maindir <- getwd()
#datadir <- file.path("~/Documents/Github/rf1-sra-socdoors/code")

# load packages
library("readxl")
library("ggplot2")
library("ggpubr")
library("tidyverse")
library("reshape2")
library("ppcor")
library("dplyr")
library("ggcorrplot")
library("psych")

# import data
data <- read_xlsx("~/Documents/Github/istart-mid-clean/code/total_df_n46.xlsx")

## Fig 2 B1: TEPS x Behavioral Motivation
#scatter <- ggplot(data, aes(x=score_teps_ant,V_beta))+
#  geom_point(colour="blue")+
#  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
#  labs(x="TEPS (Ant)",y="Behavioral Motivation")+
#  stat_cor(method="pearson")
#scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

## Fig 2 B2: RS x Behavioral Motivation
#scatter <- ggplot(data, aes(x=comp_RS,y=V_beta))+
#  geom_point(colour="blue")+
#  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="lightblue")+
#  labs(x="Reward Sensitivity",y="Behavioral Motivation")+
#  stat_cor(method="pearson")
#scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# Fig 2 B2: TEPS x RS x Behavioral Motivation **********************************
scatter <- ggplot(data, aes(x=comp_RS, y=V_beta, col=teps_ant_split))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
  labs(x="Reward Sensitivity",y="Behavioral Motivation")+
  stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     panel.background = element_blank(), axis.line = element_line(colour = "gray"))

model1 <- lm(data$V_beta ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# Fig 3: TEPS x RS x LL>SL VS Act (Beta) **********************************************
scatter <- ggplot(data, aes(x=comp_RS, y=Act_LL_minus_SL, col=teps_ant_split))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
  labs(x="Reward Sensitivity",y="VS Activation (LL>SL)")+
  stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# ACT (VS): Salience
model1 <- lm(data$Act_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): LG>SG
model1 <- lm(data$Act_LG_minus_SG ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): LL>SL [Significant RS*TEPS 3interaction; plot above]
model1 <- lm(data$Act_LL_minus_SL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): LG>Neut
model1 <- lm(data$ACT_VS_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): LL>Neut
model1 <- lm(data$ACT_VS_LL_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): LG>LL
model1 <- lm(data$ACT_VS_LG_minus_LL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): Hit>Miss
model1 <- lm(data$ACT_VS_Hit_minus_Miss ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

### RS x V_beta ****************************************************************
# ACT (VS): Salience
model1 <- lm(data$Act_Salience ~
               data$comp_RS * data$V_beta)
summary(model1)

# ACT (VS): LG>SG [Significant RS*V_beta interaction]
model1 <- lm(data$Act_LG_minus_SG ~
               data$comp_RS * data$LG_SG_new)# * data$score_teps_ant)
summary(model1)

scatter <- ggplot(data, aes(x=LG_SG_new, y=Act_LG_minus_SG, col=RS_split))+
    geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
  labs(x="Behavioral Motivation (LG>SG)",y="VS Activation (LG>SG)")+
  stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# OR
#scatter <- ggplot(data, aes(x=comp_RS, y=Act_LG_minus_SG, col=V_beta_split))+
#    geom_point()+
#  geom_point(shape=1)+
#  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
#  labs(x="Reward Sensitivity",y="VS Activation (LG>SG)")+
#  stat_cor(method="pearson")
#scatter + scale_color_manual(values = c("black", "gray")) + 
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# ACT (VS): LL>SL
model1 <- lm(data$Act_LL_minus_SL ~
               data$comp_RS * data$LL_SL_new)
summary(model1)

# ACT (VS): LG>Neut
model1 <- lm(data$ACT_VS_LG_minus_N ~
               data$comp_RS * data$LG_N_new)
summary(model1)

# ACT (VS): LL>Neut
model1 <- lm(data$ACT_VS_LL_minus_N ~
               data$comp_RS * data$LL_N_new)
summary(model1)

# ACT (VS): LG>LL
model1 <- lm(data$ACT_VS_LG_minus_LL ~
               data$comp_RS * data$LG_LL_new)
summary(model1)

# ACT (VS): Hit>Miss
model1 <- lm(data$ACT_VS_Hit_minus_Miss ~
               data$comp_RS * data$V_beta)
summary(model1)

# Fig 4: TEPS x RS x PPI_seed-VS_target-VMPFC **********************************

# PPI (VS seed, VMPFC Target): Salience
model1 <- lm(data$PPI_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>SG
model1 <- lm(data$PPI_LG_minus_SG ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LL>SL
model1 <- lm(data$PPI_LL_minus_SL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>Neut
model1 <- lm(data$PPI_vmpfc_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LL>Neut
model1 <- lm(data$PPI_vmpfc_LL_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>LL
model1 <- lm(data$PPI_vmpfc_LG_minus_LL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): Hit>Miss
model1 <- lm(data$PPI_vmpfc_Hit_minus_Miss ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): Salience
model1 <- lm(data$PPI_Salience ~
               data$comp_RS * data$V_beta)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>SG
model1 <- lm(data$PPI_LG_minus_SG ~
               data$comp_RS * data$LG_SG_new)
summary(model1)

# PPI (VS seed, VMPFC Target): LL>SL
model1 <- lm(data$PPI_LL_minus_SL ~
               data$comp_RS * data$LL_SL_new)
summary(model1)

# Below: Main effect of behavior when we control for RS ************************
# interaction effect RS*BM (LG>N) p=.0961, main effect LG>N p=.0460* ***********
# PPI (VS seed, VMPFC Target): LG>Neut *****************************************
model1 <- lm(data$PPI_vmpfc_LG_minus_N ~
               data$comp_RS * data$LG_N_new)
summary(model1)

# PPI (VS seed, VMPFC Target): LL>Neut
model1 <- lm(data$PPI_vmpfc_LL_minus_N ~
               data$comp_RS * data$LL_N_new)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>LL
model1 <- lm(data$PPI_vmpfc_LG_minus_LL ~
               data$comp_RS * data$LG_LL_new)
summary(model1)

# PPI (VS seed, VMPFC Target): Hit>Miss
model1 <- lm(data$PPI_vmpfc_Hit_minus_Miss ~
               data$comp_RS * data$V_beta)
summary(model1)


# PPI (VS seed, VMPFC Target): Hit>Miss
model1 <- lm(data$PPI_vmpfc_Hit_minus_Miss ~
               data$comp_RS)
summary(model1)

# Wrong mask, need the "Outcome" VMPFC region
scatter <- ggplot(data, aes(x=comp_RS, y=PPI_vmpfc_Hit_minus_Miss))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
  labs(x="Reward Sensitivity",y="VS-vmPFC Connectivity (Hit>Miss)")+
  stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

################################################################################
#model1 <- lm(data$PPI_Salience ~
#               data$score_teps_ant * data$V_beta * data$comp_RS)
#summary(model1)

#model1 <- lm(data$PPI_LG_minus_SG ~
#               data$score_teps_ant * data$V_beta * data$comp_RS)
#summary(model1)

#model1 <- lm(data$PPI_LL_minus_SL ~
#               data$score_teps_ant * data$V_beta * data$comp_RS)
#summary(model1)

#model1 <- lm(data$vmpfc_ppi_Gain_minus_Neut ~
#               data$score_teps_ant * data$V_beta * data$comp_RS)
#summary(model1)

#model1 <- lm(data$vmpfc_ppi_Loss_minus_Neut ~
#               data$score_teps_ant * data$V_beta * data$comp_RS)
#summary(model1)

### Figure 5: TEPS x RS x nPPI_seed-DMN_target-VMPFC *****************************

# nPPI (DMN seed, VMPFC Target): Salience
model1 <- lm(data$DMN_VS_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>SG
model1 <- lm(data$DMN_VS_LG_minus_SG ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LL>SL
model1 <- lm(data$DMN_VS_LL_minus_SL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>Neut
model1 <- lm(data$DMN_VS_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LL>Neut
model1 <- lm(data$DMN_VS_LL_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>LL
model1 <- lm(data$DMN_VS_LG_minus_LL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): Hit>Miss
model1 <- lm(data$DMN_VS_Hit_minus_Miss ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): Salience
model1 <- lm(data$DMN_VS_Salience ~
               data$comp_RS * data$V_beta)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>SG
model1 <- lm(data$DMN_VS_LG_minus_SG ~
               data$comp_RS * data$LG_SG_new)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LL>SL
model1 <- lm(data$DMN_VS_LL_minus_SL ~
               data$comp_RS * data$LL_SL_new)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>Neut
model1 <- lm(data$DMN_VS_LG_minus_N ~
               data$comp_RS * data$LG_N_new)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LL>Neut
model1 <- lm(data$DMN_VS_LL_minus_N ~
               data$comp_RS * data$LL_N_new)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>LL
model1 <- lm(data$DMN_VS_LG_minus_LL ~
               data$comp_RS * data$LG_LL_new)
summary(model1)

# nPPI (DMN seed, VMPFC Target): Hit>Miss
model1 <- lm(data$DMN_VS_Hit_minus_Miss ~
               data$comp_RS * data$V_beta)
summary(model1)

################################################################################

#model1 <- lm(data$DMN_VS_Salience ~
#              data$score_teps_ant * data$comp_RS)
#summary(model1)

#model1 <- lm(data$DMN_VS_LG_minus_SG ~
#               data$score_teps_ant * data$comp_RS)
#summary(model1)

#model1 <- lm(data$DMN_VS_LL_minus_SL ~
#               data$score_teps_ant * data$comp_RS)
#summary(model1)

#model1 <- lm(data$DMN_VS_Gain_minus_Neut ~
#               data$score_teps_ant * data$comp_RS)
#summary(model1)

#model1 <- lm(data$DMN_VS_Loss_minus_Neut ~
#               data$score_teps_ant * data$comp_RS)
#summary(model1)


# Figure 5: nPPI-DMN <--> vmPFC

model1 <- lm(data$vmpfc_nppi_Salience ~
               data$score_teps_ant * data$V_beta * data$comp_RS)
summary(model1)

model1 <- lm(data$vmpfc_nppi_LG_minus_SG ~
               data$score_teps_ant * data$V_beta * data$comp_RS)
summary(model1)

model1 <- lm(data$vmpfc_nppi_LL_minus_SL ~
               data$score_teps_ant * data$V_beta * data$comp_RS)
summary(model1)

model1 <- lm(data$vmpfc_nppi_Gain_minus_Neut ~
               data$score_teps_ant * data$V_beta * data$comp_RS)
summary(model1)

model1 <- lm(data$vmpfc_nppi_Loss_minus_Neut ~
               data$score_teps_ant * data$V_beta * data$comp_RS)
summary(model1)

scatter <- ggplot(data, aes(x=score_teps_ant,PPI_Salience))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="TEPS (Ant)",y="VS--VMPFC (Salience)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=score_teps_ant,PPI_LG_minus_SG))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="TEPS (Ant)",y="VS--VMPFC (LG>SG)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=score_teps_ant,PPI_LL_minus_SL))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="TEPS (Ant)",y="VS--VMPFC (LL>SL)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=comp_RS,PPI_Salience))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="Reward Sensitivity",y="VS--VMPFC (Salience)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=comp_RS,PPI_LG_minus_SG))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="Reward Sensitivity",y="VS--VMPFC (LG>SG)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=comp_RS,PPI_LL_minus_SL))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="Reward Sensitivity",y="VS--VMPFC (LL>SL)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=V_beta,PPI_Salience))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="Behavioral Motivation",y="VS--VMPFC (Salience)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=V_beta,PPI_LG_minus_SG))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="Behavioral Motivation",y="VS--VMPFC (LG>SG)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=V_beta,PPI_LL_minus_SL))+
  geom_point(colour="black")+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="gray", fill="gray")+
  labs(x="Behavioral Motivation",y="VS--VMPFC (LL>SL)")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
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

# VS activation # Cover your bases by looking at behavioral motivation here too
model1 <- lm(data$Act_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

model1 <- lm(data$Act_LG_minus_SG ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

model1 <- lm(data$Act_LL_minus_SL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# VS - vmPFC/OFC connectivity
model1 <- lm(data$Act_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

model1 <- lm(data$Act_LG_minus_SG ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

model1 <- lm(data$Act_LL_minus_SL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

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

###############################################################################
###                               Z-Stats:                                  ###

### RS x TEPS

# ACT (VS): Salience
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): LG>SG
model1 <- lm(data$zstat_ACT_VS_LG_minus_SG ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# Zstat ACT (VS): LL>SL [Significant p=0.0362]
model1 <- lm(data$zstat_ACT_VS_LL_minus_SL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

scatter <- ggplot(data, aes(x=comp_RS, y=zstat_ACT_VS_LL_minus_SL, col=teps_ant_split))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
  labs(x="Reward Sensitivity",y="VS Activation (LL>SL) (zstat)")+
  stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# ACT (VS): LG>Neut
model1 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): LL>Neut [Main effect of TEPS here]
model1 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): LG>LL 
model1 <- lm(data$zstat_ACT_VS_LG_minus_LL ~ 
               data$comp_RS * data$score_teps_ant)
summary(model1)

# ACT (VS): Hit>Miss
model1 <- lm(data$zstat_ACT_VS_Hit_minus_Miss ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

##############################################################################
### RS x V_beta
# ACT (VS): Salience
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$comp_RS * data$V_beta)
summary(model1)

# ACT (VS): LG>SG
model1 <- lm(data$zstat_ACT_VS_LG_minus_SG ~
               data$comp_RS * data$LG_SG_new)
summary(model1)

# ACT (VS): LL>SL
model1 <- lm(data$zstat_ACT_VS_LL_minus_SL ~
               data$comp_RS * data$LL_SL_new)
summary(model1)

# ACT (VS): LG>Neut
model1 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$comp_RS * data$LG_N_new)
summary(model1)

# ACT (VS): LL>Neut
model1 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$comp_RS * data$LL_N_new)
summary(model1)

# ACT (VS): LG>LL
model1 <- lm(data$zstat_ACT_VS_LG_minus_LL ~
               data$comp_RS * data$LG_LL_new)
summary(model1)

# ACT (VS): Hit>Miss
model1 <- lm(data$zstat_ACT_VS_Hit_minus_Miss ~
               data$comp_RS * data$V_beta)
summary(model1)

##############################################################################

# PPI (VS seed, VMPFC Target): Salience
model1 <- lm(data$zstat_PPI_vmpfc_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>SG
model1 <- lm(data$zstat_PPI_vmpfc_LG_minus_SG ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LL>SL
model1 <- lm(data$zstat_PPI_vmpfc_LL_minus_SL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>Neut
model1 <- lm(data$zstat_PPI_vmpfc_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LL>Neut
model1 <- lm(data$zstat_PPI_vmpfc_LL_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>LL
model1 <- lm(data$zstat_PPI_vmpfc_LG_minus_LL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# PPI (VS seed, VMPFC Target): Hit>Miss
model1 <- lm(data$zstat_PPI_vmpfc_Hit_minus_Miss ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

##############################################################################

# PPI (VS seed, VMPFC Target): Salience
model1 <- lm(data$zstat_PPI_vmpfc_Salience ~
               data$comp_RS * data$V_beta)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>SG
model1 <- lm(data$zstat_PPI_vmpfc_LG_minus_SG ~
               data$comp_RS * data$LG_SG_new)
summary(model1)

# PPI (VS seed, VMPFC Target): LL>SL [Main effect of RS]
model1 <- lm(data$zstat_PPI_vmpfc_LL_minus_SL ~
               data$comp_RS * data$LL_SL_new)
summary(model1)

# PPI (VS seed, VMPFC Target): LG>Neut
model1 <- lm(data$zstat_PPI_vmpfc_LG_minus_N ~
               data$comp_RS * data$LG_N_new)
summary(model1)

# PPI (VS seed, VMPFC Target): LL>Neut *****************************************
# Significant interaction RS*LL>N p=.041 ***************************************
model1 <- lm(data$zstat_PPI_vmpfc_LL_minus_N ~
               data$comp_RS * data$LL_N_new)
summary(model1)

scatter <- ggplot(data, aes(x=LL_N_new, y=zstat_PPI_vmpfc_LL_minus_N, col=RS_split))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
  labs(x="Behavioral Motivation (LL>N)",y="VS-vmPFC (LL>N) (zstat)")+
  stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))


# PPI (VS seed, VMPFC Target): LG>LL
model1 <- lm(data$zstat_PPI_vmpfc_LG_minus_LL ~
               data$comp_RS * data$LG_LL_new)
summary(model1)

# PPI (VS seed, VMPFC Target): Hit>Miss
model1 <- lm(data$zstat_PPI_vmpfc_Hit_minus_Miss ~
               data$comp_RS * data$V_beta)
summary(model1)

##############################################################################

# nPPI (DMN seed, VMPFC Target): Salience
model1 <- lm(data$zstat_DMN_VS_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>SG
model1 <- lm(data$zstat_DMN_VS_LG_minus_SG ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LL>SL
model1 <- lm(data$zstat_DMN_VS_LL_minus_SL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>Neut
model1 <- lm(data$zstat_DMN_VS_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LL>Neut
model1 <- lm(data$zstat_DMN_VS_LL_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): LG>LL
model1 <- lm(data$zstat_DMN_VS_LG_minus_LL ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

# nPPI (DMN seed, VMPFC Target): Hit>Miss
model1 <- lm(data$zstat_DMN_VS_Hit_minus_Miss ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

##############################################################################

# nPPI (DMN seed, VS Target): Salience [Barely significant, p=0.0443, not gonna survive correction]*********************
model1 <- lm(data$zstat_DMN_VS_Salience ~
               data$comp_RS * data$V_beta * data$score_teps_ant)
summary(model1)

scatter <- ggplot(data, aes(x=comp_RS, y=zstat_DMN_VS_Salience, col=V_beta_split))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, linetype="dashed", fill="lightgray")+
  labs(x="Reward Sensitivity",y="DMN-VS Connectivity (Salience) (zstat)")+
  stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))


# nPPI (DMN seed, VS Target): LG>SG
# Slight main effect of behavioral motivation (LG>SG) when controlling for RS p=.047
model1 <- lm(data$zstat_DMN_VS_LG_minus_SG ~
               data$comp_RS * data$LG_SG_new)
summary(model1)

# nPPI (DMN seed, VS Target): LL>SL
model1 <- lm(data$zstat_DMN_VS_LL_minus_SL ~
               data$comp_RS * data$LL_SL_new)
summary(model1)

# nPPI (DMN seed, VS Target): LG>Neut ******************************************
# Highly significant interaction this is the way ******************************* See this result
# Might want to plot this the other way ****************************************
model1 <- lm(data$zstat_DMN_VS_LG_minus_N ~
               data$comp_RS * data$LG_N_new)
summary(model1)

scatter <- ggplot(data, aes(x=comp_RS, y=zstat_DMN_VS_LG_minus_N, col=LG_N_splitthree))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, linetype="dashed", se=FALSE)+
  labs(x="Reward Sensitivity",y="DMN-VS (LG>N) (zstat)")
  #stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "red", "blue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# nPPI (DMN seed, VS Target): LL>Neut
model1 <- lm(data$zstat_DMN_VS_LL_minus_N ~
               data$comp_RS * data$LL_N_new)
summary(model1)

# nPPI (DMN seed, VS Target): LG>LL
model1 <- lm(data$zstat_DMN_VS_LG_minus_LL ~
               data$comp_RS * data$LG_LL_new)
summary(model1)

# nPPI (DMN seed, VS Target): Hit>Miss
model1 <- lm(data$zstat_DMN_VS_Hit_minus_Miss ~
               data$comp_RS * data$V_beta)
summary(model1)

################################################################################
# Heat map of all correlations:
corr_model <- subset(data, select = c("comp_RS", "score_teps_ant", "score_teps_con", "V_beta"))

cormat <- round(cor(corr_model),2)
head(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)
head(melted_cormat)
melted_cormat

ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Simple Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

