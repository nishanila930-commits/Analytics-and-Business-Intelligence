# =========================================
# HOTELS_2025: Comprehensive Analytics
# =========================================

# Load Libraries
install.packages("PerformanceAnalytics")

library(tidyverse)
library(psych)
library(GGally)
library(corrplot)
library(ggpubr)
library(car)
library(MASS)
library(lmtest)
library(PerformanceAnalytics)
library(cluster)
library(factoextra)
library(reshape2)
library(gridExtra)

# -----------------------------------------
# 1. Load Dataset
# -----------------------------------------
file_path <- "/Users/suganthankajanan/Desktop/ABI-CIS6008-SEP-2025-Dataset/Question-(a)/HOTELS_2025.csv"
hotels <- read.csv(file_path, stringsAsFactors = FALSE)

# Inspect dataset
str(hotels)
summary(hotels)
colSums(is.na(hotels))

# -----------------------------------------
# 2. Data Cleaning
# -----------------------------------------
hotels <- hotels %>% drop_na(Revenue)  # Drop missing Revenue
if("StarRating" %in% colnames(hotels)) hotels$StarRating <- as.factor(hotels$StarRating)

# -----------------------------------------
# 3. Descriptive Statistics
# -----------------------------------------
describe(hotels)  # Detailed numeric summary
summary(hotels)   # Basic summary

# -----------------------------------------
# 4. Missing Value Check
# -----------------------------------------
colSums(is.na(hotels))

# -----------------------------------------
# 5. Histogram & Density Plots
# -----------------------------------------
numeric_vars <- hotels %>% select_if(is.numeric) %>% colnames()
for(var in numeric_vars){
  p <- ggplot(hotels, aes_string(x=var)) +
    geom_histogram(aes(y=..density..), bins=30, fill="skyblue", color="black") +
    geom_density(color="red", size=1) +
    theme_minimal() +
    labs(title=paste("Histogram & Density of", var))
  print(p)
}

# -----------------------------------------
# 6. Q-Q Plots for Normality
# -----------------------------------------
for(var in numeric_vars){
  print(ggqqplot(hotels[[var]], title=paste("Q-Q Plot of", var)))
}

# -----------------------------------------
# 7. Shapiro-Wilk Test for Normality
# -----------------------------------------
for(var in numeric_vars){
  cat("\nShapiro-Wilk Test for", var, ":\n")
  print(shapiro.test(hotels[[var]]))
}

# -----------------------------------------
# 8. Correlation Matrix & Heatmap
# -----------------------------------------
numeric_data <- hotels %>% select_if(is.numeric)
cor_matrix <- cor(numeric_data, use="complete.obs")
corrplot(cor_matrix, method="color", type="upper", addCoef.col="black", tl.col="black")

# -----------------------------------------
# 9. Scatterplot Matrix
# -----------------------------------------
ggpairs(numeric_data)

# -----------------------------------------
# 10. Hypothesis Testing (t-test example)
# -----------------------------------------
# Example: Revenue difference by StarRating (if exists)
if("StarRating" %in% colnames(hotels)){
  t_test_result <- t.test(Revenue ~ StarRating, data=hotels)
  print(t_test_result)
}

# -----------------------------------------
# 11. Simple Linear Regression
# -----------------------------------------
if("OccupancyRate" %in% colnames(hotels)){
  lm_simple <- lm(Revenue ~ OccupancyRate, data=hotels)
  summary(lm_simple)
  par(mfrow=c(2,2))
  plot(lm_simple)
  par(mfrow=c(1,1))
}

# -----------------------------------------
# 12. Multiple Linear Regression
# -----------------------------------------
lm_multiple <- lm(Revenue ~ ., data=numeric_data)
summary(lm_multiple)



# -----------------------------------------
# 13. Regression Diagnostics
# -----------------------------------------
vif(step_model)      # Multicollinearity
bptest(step_model)   # Heteroscedasticity
dwtest(step_model)   # Autocorrelation

# Residual vs Fitted plot
residuals <- resid(step_model)
fitted <- fitted(step_model)
ggplot(data.frame(Fitted=fitted, Residuals=residuals), aes(x=Fitted, y=Residuals)) +
  geom_point(color="purple", alpha=0.6) + geom_hline(yintercept=0, linetype="dashed", color="red") +
  theme_minimal() + labs(title="Residuals vs Fitted Values", x="Fitted", y="Residuals")

# -----------------------------------------
# 14. Predicted Revenue Plot
# -----------------------------------------
hotels$PredictedRevenue <- predict(step_model, newdata=numeric_data)
ggplot(hotels, aes(x=Revenue, y=PredictedRevenue)) +
  geom_point(color="darkgreen") +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red") +
  theme_minimal() + labs(title="Actual vs Predicted Revenue", x="Actual", y="Predicted")


# -----------------------------------------
# 16. Boxplots for Revenue vs Categorical Variables
# -----------------------------------------
cat_vars <- hotels %>% select_if(is.factor) %>% colnames()
for(var in cat_vars){
  p <- ggplot(hotels, aes_string(x=var, y="Revenue")) +
    geom_boxplot(fill="lightblue") + theme_minimal() +
    labs(title=paste("Revenue vs", var))
  print(p)
}

# -----------------------------------------
# 17. Advanced Scatter Plots with Regression
# -----------------------------------------
for(var in setdiff(numeric_vars, "Revenue")){
  p <- ggplot(hotels, aes_string(x=var, y="Revenue")) +
    geom_point(alpha=0.6, color="blue") +
    geom_smooth(method="lm", se=TRUE, color="red") +
    theme_minimal() + labs(title=paste("Revenue vs", var))
  print(p)
}

# -----------------------------------------
# 18. Cluster Analysis (K-means)
# -----------------------------------------
# Scale numeric data
numeric_scaled <- scale(numeric_data)
set.seed(123)
kmeans_model <- kmeans(numeric_scaled, centers=3, nstart=25)
hotels$Cluster <- as.factor(kmeans_model$cluster)

# Visualize clusters on Revenue vs OccupancyRate
if("OccupancyRate" %in% colnames(hotels)){
  ggplot(hotels, aes(x=OccupancyRate, y=Revenue, color=Cluster)) +
    geom_point(size=2, alpha=0.7) + theme_minimal() +
    labs(title="K-Means Clustering: Revenue vs OccupancyRate")
}