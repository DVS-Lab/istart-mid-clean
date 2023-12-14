# set working directory
setwd("~/Desktop")
maindir <- getwd()
datadir <- file.path("~/Documents/Github/istart-mid-clean/code")

# load packages
library("readxl")
library("ggplot2")
library("ggpubr")

# import data
data <- read_excel("~/Documents/Github/istart-mid-clean/code/plot_nppi-results.xlsx")

### Model 4B Result
scatter <- ggplot(data, aes(x=RS, y=model4B))+
  geom_point()+
  geom_point(shape=1,color="black")+
  geom_smooth(method=lm, formula= y ~ poly(x,2), level=0.99, se=FALSE, fullrange=TRUE, linetype="dashed")+
  labs(x="RS", y="Hit-Miss")
  #formula= y ~ x+I(x^2)
  #scale_x_continuous(breaks = seq(-6, 6, by = 2))

# remove gridlines
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))

### Model 4E Result
scatter <- ggplot(data, aes(x=Vbeta, y=model4E, col=Reward_Sensitivity))+
  geom_point()+
  geom_point(shape=1,color="black")+
  geom_smooth(method=lm, formula= y ~ poly(x,2), level=0.99, se=FALSE, fullrange=TRUE, linetype="dashed")+ #formula= y ~ x+I(x^2)
  labs(x="VBeta (i.e., Fit)", y="Salience")
  #scale_x_continuous(breaks = seq(-6, 6, by = 2))

# remove gridlines
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))


### Model 4G Result
scatter <- ggplot(data,aes(x=RS,y=model4G))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE, level=0.99, fullrange=TRUE, linetype="dashed", colour="black")+
  labs(x="RS",y="Hit-Miss")+
  stat_cor(method="pearson")
scatter + scale_color_hue() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
