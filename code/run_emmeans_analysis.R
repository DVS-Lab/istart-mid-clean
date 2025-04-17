
# Load required packages
library(emmeans)

# Read the data
data <- read.csv('long_format_data.csv')

# Make Condition a factor with the correct levels
data$Condition <- factor(data$Condition, 
                         levels = c("Large Gain", "Small Gain", "Neutral", "Small Loss", "Large Loss"))
data$Sub <- factor(data$Sub)  # Ensure Sub is treated as a factor

# Run the repeated measures ANOVA
anova_results <- aov(Value ~ Condition + Error(Sub/Condition), data = data)

# Print the ANOVA results
cat("\nR ANOVA Results:\n")
print(summary(anova_results))

# Compute emmeans for the model
emm <- emmeans(anova_results, ~ Condition)

# Compute all pairwise comparisons
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")

# Print all pairwise comparisons
cat("\nR Pairwise Comparisons:\n")
pairwise_summary <- summary(pairwise_comparisons)
print(pairwise_summary)

# Save the results to a CSV file
write.csv(as.data.frame(pairwise_summary), 'r_pairwise_results.csv', row.names = FALSE)
