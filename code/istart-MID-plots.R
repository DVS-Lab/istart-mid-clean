# Script for istart-mid manuscript figures & stats
# See also: https://github.com/DVS-Lab/istart-mid-clean/blob/main/code/Behave_update.ipynb

setwd("~/Desktop")
maindir <- getwd()
#datadir <- file.path("~/Documents/Github/rf1-sra-socdoors/code")

# load packages
library("readxl")
library("ggplot2")
library("ggpubr")
library("tidyverse")
library("tidyr")
library("reshape2")
library("ppcor")
library("dplyr")
library("ggcorrplot")
library("psych")
library("car")      # For repeated measures ANOVA
library("emmeans")  # For post-hoc analysis (Tukey's HSD)

# import data
data <- read_xlsx("~/Documents/Github/istart-mid-clean/code/total_df_n46.xlsx")

## Figure 2 Panel B ############################################################
# Group-level differences & pairwise comparisons:
# Reshape the data for repeated measures analysis
long_data <- data %>%
  pivot_longer(
    cols = starts_with("Beh_"),  # Select columns of interest
    names_to = "Condition",         # New column for condition labels
    values_to = "Value"             # New column for values
  )
# Create a factor variable for the repeated measures condition
long_data$Condition <- factor(long_data$Condition, 
                              levels = c("Beh_LG", "Beh_SG", "Beh_N", "Beh_SL", "Beh_LL"),
                              labels = c("Large Gain", "Small Gain", "Neutral", "Small Loss", "Large Loss"))
# Run the repeated measures ANOVA
anova_results <- aov(Value ~ Condition + Error(Sub/Condition), data = long_data)
# Print the ANOVA results
summary(anova_results)
# Perform Tukey's HSD for all pairwise comparisons
# Compute emmeans for the model
tukey_results <- emmeans(anova_results, ~ Condition)
# Compute all pairwise comparisons
pairwise_comparisons <- contrast(
  tukey_results,
  method = "pairwise"
)
# Print all pairwise comparisons
pairwise_summary <- summary(pairwise_comparisons)
print(pairwise_summary)

# Figure 2 Panel C: Heatmap ####################################################
corr_model <- subset(data, select = c("comp_RS", "score_teps_ant", "V_beta", "LG_N_new", "LL_N_new"))
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
  scale_fill_gradient2(low = "steelblue3", high = "indianred3", mid = "white", 
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


## Fig 2 Panel D: TEPS x RS x Behavioral Motivation ############################

# Stats: toggle TEPSc
model1 <- lm(data$V_beta ~
               data$score_teps_con + data$comp_RS * data$score_teps_ant)
summary(model1)

# Plot
scatter <- ggplot(data, aes(x=comp_RS, y=V_beta, col=teps_ant_split))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, linetype="solid", fill="lightgray")+
  labs(x="Reward Sensitivity",y="Behavioral Motivation")
#stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

## Stats for Fig 3 #############################################################

# Reshape the data for repeated measures analysis
long_data <- data %>%
  pivot_longer(
    cols = starts_with("zstat_ACT_VS_"),  # Select columns of interest
    names_to = "Condition",         # New column for condition labels
    values_to = "Value"             # New column for values
  )
# Create a factor variable for the repeated measures condition
long_data$Condition <- factor(long_data$Condition, 
                              levels = c("zstat_ACT_VS_LargeGain", "zstat_ACT_VS_SmallGain", "zstat_ACT_VS_Neut", "zstat_ACT_VS_SmallLoss", "zstat_ACT_VS_LargeLoss"),
                              labels = c("Large Gain", "Small Gain", "Neutral", "Small Loss", "Large Loss"))
# Run the repeated measures ANOVA
anova_results <- aov(Value ~ Condition + Error(Sub/Condition), data = long_data)
# Print the ANOVA results
summary(anova_results)
# Perform Tukey's HSD for all pairwise comparisons
# Compute emmeans for the model
tukey_results <- emmeans(anova_results, ~ Condition)
# Compute all pairwise comparisons
pairwise_comparisons <- contrast(
  tukey_results,
  method = "pairwise"
)
# Print all pairwise comparisons
pairwise_summary <- summary(pairwise_comparisons)
print(pairwise_summary)

# Stats:
# For main Hyp: RS * TEPSa
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

model2 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model2)

model3 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model3)
# For continuity: RS * Behavioral Motivation
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$comp_RS * data$V_beta_new)
summary(model1)

model2 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$comp_RS * data$LG_N_new)
summary(model2)

model3 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$comp_RS * data$LL_N_new)
summary(model3)
# For main hyp: RS * Behavioral Motivation contr. for TEPSa
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$score_teps_ant + data$comp_RS * data$V_beta_new)
summary(model1)

model2 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$score_teps_ant + data$comp_RS * data$LG_N_new)
summary(model2)

model3 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$score_teps_ant + data$comp_RS * data$LL_N_new)
summary(model3)
# For continuity: RS * TEPSa
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)

model2 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model2)

model3 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model3)
# Testing main effects: RS
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$comp_RS)
summary(model1)

model2 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$comp_RS)
summary(model2)

model3 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$comp_RS)
summary(model3)
# Testing main effects: TEPSa
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$score_teps_ant)
summary(model1)

model2 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$score_teps_ant)
summary(model2)

model3 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$score_teps_ant)
summary(model3)
# Testing main effects: Behavioral Motivation
model1 <- lm(data$zstat_ACT_VS_Salience ~
               data$V_beta_new)
summary(model1)

model2 <- lm(data$zstat_ACT_VS_LG_minus_N ~
               data$LG_N_new)
summary(model2)

model3 <- lm(data$zstat_ACT_VS_LL_minus_N ~
               data$LL_N_new)
summary(model3)

## Figure 4 ####################################################################
# nPPI (DMN seed, VS Target)

# Stats: Salience
model1 <- lm(data$zstat_DMN_VS_Salience ~
               data$score_teps_ant + data$comp_RS * data$V_beta_new)
summary(model1) # This one too
model1 <- lm(data$zstat_DMN_VS_Salience ~
               data$comp_RS * data$score_teps_ant)
summary(model1)
model1 <- lm(data$zstat_DMN_VS_Salience ~
               data$score_teps_ant * data$V_beta_new)
summary(model1)

# LG>N
model1 <- lm(data$zstat_DMN_VS_LG_minus_N ~
               data$score_teps_ant + data$comp_RS * data$LG_N_new)
summary(model1) # This one
model1 <- lm(data$zstat_DMN_VS_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)
model1 <- lm(data$zstat_DMN_VS_LG_minus_N ~
               data$score_teps_ant * data$LG_N_new)
summary(model1)

# LL>N
model1 <- lm(data$zstat_DMN_VS_LL_minus_N ~
               data$score_teps_ant + data$comp_RS * data$LL_N_new)
summary(model1) # And this one
model1 <- lm(data$zstat_DMN_VS_LG_minus_N ~
               data$comp_RS * data$score_teps_ant)
summary(model1)
model1 <- lm(data$zstat_DMN_VS_LL_minus_N ~
               data$score_teps_ant * data$LL_N_new)
summary(model1)

# Plots

# DMN-VS (LG>N) ~ RS * LG>N
scatter <- ggplot(data, aes(x=comp_RS, y=zstat_DMN_VS_LG_minus_N, col=LG_N_splitthree))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, linetype="solid", se=FALSE)+
  labs(x="Reward Sensitivity",y="DMN-VS (LG>N)\n(zstat)", color="Behavioral\nMotivation")
#stat_cor(method="pearson")
#scatter + scale_color_manual(values = c("black", "gray")) + 
scatter + scale_color_manual(values = c("red", "blue", "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# DMN-VS (LL>N) ~ RS * LL>N
scatter <- ggplot(data, aes(x=comp_RS, y=zstat_DMN_VS_LL_minus_N, col=LL_N_splitthree))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, linetype="solid", se=FALSE)+
  labs(x="Reward Sensitivity",y="DMN-VS (LL>N)\n(zstat)", color="Behavioral\nMotivation")
#stat_cor(method="pearson")
#scatter + scale_color_manual(values = c("black", "gray")) + 
scatter + scale_color_manual(values = c("red", "blue", "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# DMN-VS (Salience) ~ RS * V_beta
scatter <- ggplot(data, aes(x=comp_RS, y=zstat_DMN_VS_Salience, col=V_beta_new_splitthree))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, linetype="solid", se=FALSE)+
  labs(x="Reward Sensitivity",y="DMN-VS (Salience)\n(zstat)", color="Behavioral\nMotivation\n(HS>LS)")
#stat_cor(method="pearson")
#scatter + scale_color_manual(values = c("black", "gray")) + 
scatter + scale_color_manual(values = c("red", "blue", "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))


## Forest Plots

# Load required packages
library(forestplot)
library(dplyr)

# Note: Check forestplot version for compatibility
#cat("forestplot version:", packageVersion("forestplot"), "\n")

# Assuming 'data' is your dataframe with all variables
# Define behavioral and brain activity variables
behavioral_vars <- c("comp_RS", "V_beta_new", "LG_N_new", "LL_N_new", "score_teps_ant")
brain_vars <- c("Act_Salience", "Act_Gain_minus_Neut", "Act_Loss_minus_Neut")

# Check data types and convert to numeric if necessary
for (var in c(behavioral_vars, brain_vars)) {
  if (!is.numeric(data[[var]])) {
    data[[var]] <- as.numeric(as.character(data[[var]]))
    warning(paste("Converted", var, "to numeric"))
  }
}

# Define specific combinations in the desired order
desired_pairs <- data.frame(
  behavioral = c("comp_RS", "comp_RS", "comp_RS", 
                 "V_beta_new", "LG_N_new", "LL_N_new", 
                 "score_teps_ant", "score_teps_ant", "score_teps_ant"),
  brain = c("Act_Salience", "Act_Gain_minus_Neut", "Act_Loss_minus_Neut",
            "Act_Salience", "Act_Gain_minus_Neut", "Act_Loss_minus_Neut",
            "Act_Salience", "Act_Gain_minus_Neut", "Act_Loss_minus_Neut")
)

# Create combinations
combinations <- desired_pairs
cat("Number of combinations:", nrow(combinations), "\n")

# Calculate correlations, confidence intervals, and p-values
results <- combinations %>%
  rowwise() %>%
  mutate(
    correlation = tryCatch({
      if (sd(data[[behavioral]], na.rm = TRUE) == 0 || sd(data[[brain]], na.rm = TRUE) == 0) {
        NA
      } else {
        cor_result <- cor.test(data[[behavioral]], data[[brain]], method = "pearson")
        cor_result$estimate
      }
    }, error = function(e) {
      warning(paste("Error in cor.test for", behavioral, "vs", brain, ":", e$message))
      NA
    }),
    ci_lower = tryCatch({
      if (sd(data[[behavioral]], na.rm = TRUE) == 0 || sd(data[[brain]], na.rm = TRUE) == 0) {
        NA
      } else {
        cor_result <- cor.test(data[[behavioral]], data[[brain]], method = "pearson")
        cor_result$conf.int[1]
      }
    }, error = function(e) NA),
    ci_upper = tryCatch({
      if (sd(data[[behavioral]], na.rm = TRUE) == 0 || sd(data[[brain]], na.rm = TRUE) == 0) {
        NA
      } else {
        cor_result <- cor.test(data[[behavioral]], data[[brain]], method = "pearson")
        cor_result$conf.int[2]
      }
    }, error = function(e) NA),
    p_value = tryCatch({
      if (sd(data[[behavioral]], na.rm = TRUE) == 0 || sd(data[[brain]], na.rm = TRUE) == 0) {
        NA
      } else {
        cor_result <- cor.test(data[[behavioral]], data[[brain]], method = "pearson")
        cor_result$p.value
      }
    }, error = function(e) NA)
  ) %>%
  ungroup()

# Print results for debugging
cat("Results before filtering:\n")
print(summary(results))
print(results)

# Remove rows with NA in any critical column
results <- results %>% 
  filter(!is.na(correlation) & !is.na(ci_lower) & !is.na(ci_upper) & !is.na(p_value))

# Check if there are any valid results
cat("Number of valid correlations after filtering:", nrow(results), "\n")
if (nrow(results) == 0) {
  stop("No valid correlations found. Check the 'results' output above for NA or invalid correlations.")
}

# Rename variables for display
results <- results %>%
  mutate(
    behavioral_display = case_when(
      behavioral == "comp_RS" ~ "RS",
      behavioral == "V_beta_new" ~ "Behavioral Motivation (HS>LS)",
      behavioral == "LG_N_new" ~ "Behavioral Motivation (LG>N)",
      behavioral == "LL_N_new" ~ "Behavioral Motivation (LL>N)",
      behavioral == "score_teps_ant" ~ "Anhedonia"
    ),
    brain_display = case_when(
      brain == "Act_Salience" ~ "VS (HS>LS)",
      brain == "Act_Gain_minus_Neut" ~ "VS (LG>N)",
      brain == "Act_Loss_minus_Neut" ~ "VS (LL>N)"
    )
  )

# Prepare data for forest plot
mean <- results$correlation
lower <- results$ci_lower
upper <- results$ci_upper
p_values <- results$p_value

# Create labels for the plot with renamed variables
labels <- paste(results$behavioral_display, "vs", results$brain_display)

# Create table text with p-value column
tabletext <- cbind(
  c("Comparison", labels),
  c("Correlation", sprintf("%.2f", mean)),
  c("P-value", sprintf("%.3f", p_values))
)

# Verify dimensions for debugging
cat("Dimensions of tabletext:", dim(tabletext), "\n")
cat("Length of mean:", length(mean), "\n")
cat("Length of lower:", length(lower), "\n")
cat("Length of upper:", length(upper), "\n")
cat("Length of p_values:", length(p_values), "\n")

# Create a matrix for plotting data
plot_data <- cbind(mean, lower, upper)
plot_data <- rbind(c(NA, NA, NA), plot_data)  # Add NA row for header

# Define which rows are summary (header)
is_summary <- c(TRUE, rep(FALSE, nrow(results)))

# Create the forest plot
forestplot(
  labeltext = tabletext,
  mean = plot_data[, 1],
  lower = plot_data[, 2],
  upper = plot_data[, 3],
  is.summary = is_summary,
  title = "Correlation between Behavioral and Brain Activity Variables",
  xlab = "Pearson Correlation Coefficient",
  zero = 0,
  boxsize = 0.2,
  col = fpColors(
    box = "royalblue",
    line = "royalblue",
    summary = "black"
  ),
  xticks = seq(-1, 1, by = 0.2),
  grid = TRUE,
  lwd.ci = 1,
  ci.vertices = TRUE,
  ci.vertices.height = 0.1,
  align = c("l", "c", "c"),
  txt_gp = fpTxtGp(
    xlab = gpar(cex = 1.2),  # Increase x-axis label font size
    ticks = gpar(cex = 1.2),  # Increase x-axis tick font size
    label = gpar(cex = 0.8)  # Smaller font for labels to reduce spacing
  ),
  rowheight = 0.1,  # Reduce row height to decrease text spacing
  lineheight = "auto"  # Adjust line height automatically
)


################################################################################
################################################################################
################################################################################
scatter <- ggplot(data, aes(x=comp_RS, y=zstat_DMN_VS_Salience, col=V_beta_new_splitthree))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, linetype="solid", se=FALSE)+
  labs(x="Reward Sensitivity",y="DMN-VS (Salience)\n(zstat)")
#stat_cor(method="pearson")
#scatter + scale_color_manual(values = c("black", "gray")) + 
scatter + scale_color_manual(values = c("red", "blue", "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

scatter <- ggplot(data, aes(x=comp_RS, y=zstat_DMN_VS_Salience, col=V_beta_new_split))+
  geom_point()+
  geom_point(shape=1)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, linetype="solid", fill="lightgray")+
  labs(x="Reward Sensitivity",y="DMN-VS (Salience)\n(zstat)")
#stat_cor(method="pearson")
scatter + scale_color_manual(values = c("black", "gray")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

model1 <- lm(data$zstat_DMN_VS_LG_minus_N ~
               data$score_teps_con + data$score_teps_ant + data$comp_RS * data$LG_N_new)
summary(model1) # This one

model1 <- lm(data$zstat_DMN_VS_LL_minus_N ~
               data$score_teps_con + data$score_teps_ant + data$comp_RS * data$LL_N_new)
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
model1 <- lm(data$zstat_ACT_VS_LL_minus_SL ~
               data$score_teps_con + data$comp_RS * data$score_teps_ant)
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
  geom_smooth(method=lm, linetype="solid", se=FALSE)+
  labs(x="Reward Sensitivity",y="DMN-VS (LG>N)\n(zstat)")
  #stat_cor(method="pearson")
#scatter + scale_color_manual(values = c("black", "gray")) + 
scatter + scale_color_manual(values = c("black", "lightgray", "darkgray")) + 
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

# Color palette sheet: https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf