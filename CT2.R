### CAPSTONE PROJECT ###

### To install the required packages ###

install.packages("arules")
install.packages("recommenderlab")
install.packages("tidyverse")
install.packages("caret")
install.packages("Matrix")
install.packages("proxy")
install.packages("DataExplorer")
install.packages("tidyr")
install.packages("pROC")


### To load the packages installed ###

library(Matrix)
library(proxy)
library(arules)
library(recommenderlab)
library(tidyverse)
library(caret)
library(DataExplorer)
library(tidyr)  
library(dplyr)
library(pROC)

### To load the Data into R studio ###

Custchurn.df <- read.csv("C:/Users/Admin/Desktop/CSU Global/Course 8_CAPSTONE_Business Intelligence and Data Analytics/Capstone Project/Datasets/train.csv")

### To remove some variables early on for ethics purpose ###

Custchurn.df <- Custchurn.df %>% select(-Lat.Long, -Latitude, -Longitude,-City,-Zip.Code)

### To explore the data frame " Custchurn.df " in many different ways ###

introduce(Custchurn.df) # To display a summary of the dataset #
str(Custchurn.df)  # To view the structure of the dataset #
head(Custchurn.df)  # To display the first six rows of the data set #
sapply(Custchurn.df, class) # To understand the class(data type) of each column #
dim(Custchurn.df)  # To display the dimension of the dataset #
summary(Custchurn.df)  # To display the summary statistics of all attributes #


### Variability Analysis for numeric variables _ Standard deviation and variance ###

numeric_vars <- Custchurn.df[, sapply(Custchurn.df, is.numeric)] # To select numeric variables only #

summary_stats <- data.frame(
  Variable = names(numeric_vars),
  SD = sapply(numeric_vars, sd, na.rm = TRUE),
  Variance = sapply(numeric_vars, var, na.rm = TRUE)
)
print(summary_stats)

### To display the numeric variables in order of increasing variability (cv) ###

cv_values <- sapply(numeric_vars, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
print(sort(cv_values))

### To Visualize the data ###

plot_histogram(Custchurn.df)
plot_bar(Custchurn.df)
plot_missing(Custchurn.df)
boxplot(numeric_vars, main = "Boxplot of Numeric Variables", las = 2, col = "lightgreen")  # To create boxplots for each numeric variable only #

### To display correlation Matrix between all numeric attributes ###

options(max.print = 10000)      # To adjust print limit for large correlation matrix #
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

### To Pivot the correlation matrix into long format and sort in descending order ###

cor_df <- as.data.frame(cor_matrix)
cor_df_long <- cor_df %>%
  rownames_to_column("Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "value")
cor_df_long <- cor_df_long %>%
  arrange(desc(abs(value)))
print(cor_df_long, n=10000)

### To display the top 10 strongest correlations (excluding self-correlations) only with the dependent "Churn" ###

cor_df_long_churn <- cor_df_long %>%
  filter(Var1 == "Churn" | Var2 == "Churn")
cor_df_long_churn <- cor_df_long_churn %>%
  arrange(desc(abs(value)))
cor_df_long_churn <- cor_df_long_churn %>%
  filter(Var1 != Var2)

print(cor_df_long_churn, n=100)


### To display frequency tables for categorical variables ###

cat_vars <- Custchurn.df[, sapply(Custchurn.df, is.character)]
cat_vars <- cat_vars %>% select(-Customer.ID)
freq_tables <- lapply(cat_vars, table)
print(freq_tables)

### To display ANOVA analysis for categorical variables ###

cat_var_names <- setdiff(names(cat_vars), c("Country", "Quarter", "State", "Customer.ID"))  # To get categorical variable names, excluding variables with only unique value #

anova_results <- lapply(cat_var_names, function(var) {
  formula <- as.formula(paste("Churn ~", var))
  model <- aov(formula, data = Custchurn.df)
  summary_out <- summary(model)[[1]]
  p_value <- summary_out["Pr(>F)"][1, 1]
  return(data.frame(Variable = var, P_Value = p_value))
})
anova_summary <- do.call(rbind, anova_results)   # To combine all results into one data frame #
anova_summary <- anova_summary[order(anova_summary$P_Value), ]  # To sort by smallest p-value (stronger statistical evidence of association) #
anova_summary$Significant <- ifelse(anova_summary$P_Value < 0.05, "Yes", "No")  # To flag significant variables p<0.05 #

print(anova_summary)

### To display Chi-Square Test for categorical variables ###

Custchurn.df$Churn <- as.factor(Custchurn.df$Churn)   # To ensure 'Churn' is a factor variable #

chisq_results <- lapply(cat_var_names, function(var) {
  tbl <- table(Custchurn.df$Churn, cat_vars[[var]])
  
  ## To run the test only if the table is valid ##
  
  if (all(dim(tbl) > 1)) {
    test <- chisq.test(tbl)
    p_value <- test$p.value
  } else {
    p_value <- NA
  }
  
  return(data.frame(Variable = var, P_Value = p_value))
})



chisq_summary <- do.call(rbind, chisq_results)                   # To combine all results into a data frame #
chisq_summary <- chisq_summary[order(chisq_summary$P_Value), ]   # To sort by smallest p-value (stronger statistical evidence of association) #
chisq_summary$Significant <- ifelse(chisq_summary$P_Value < 0.05, "Yes", "No")  # To flag significant variables p<0.05 #

print(chisq_summary)

### To exclude some columns according to the higher weakness of the relationship with the dependent variable "Churn" and not statistically significant ###

exclude_vars <- c(
  "Country", "Quarter", "State", "Customer.ID", "CLTV", "Population", "Senior.Citizen", 
  "Total.Extra.Data.Charges", "Total.Refunds", "Streaming.Music", 
  "Streaming.Movies", "Streaming.TV", "Partner", "Referred.a.Friend",  "Gender", 
  "Phone.Service", "Online.Backup", "Online.Security", 'Under.30'
)

Custchurn_filtered <- Custchurn.df[, !(names(Custchurn.df) %in% exclude_vars)]

### To segment some continuous variables into buckets ###

Custchurn_filtered$Age.Group <- cut(Custchurn_filtered$Age, c(0, 30, 45, 65, Inf),
                                    labels = c("Young Adult", "Adult", "Mature Adult", "Senior"), right = FALSE)


Custchurn_filtered$Monthly.Charge.Group <- cut(Custchurn_filtered$Monthly.Charge, c(0, 35, 70, 105, Inf),
                                               labels = c("Low", "Medium", "High", "Very High"), right = FALSE)


Custchurn_filtered$Tenure.Group <- cut(Custchurn_filtered$Tenure.in.Months, c(0, 12, 36, 60, Inf),
                                       labels = c("<1 Year", "1-3 Years", "3-5 Years", "5+ Years"), right = FALSE)


Custchurn_filtered$Total.Revenue.Group <- cut(Custchurn_filtered$Total.Revenue, c(0, 1000, 2500, 5000, Inf),
                                              labels = c("Low", "Moderate", "High", "Very High"), right = FALSE)


Custchurn_filtered$GB.Download.Group <- cut(Custchurn_filtered$Avg.Monthly.GB.Download, c(0, 5, 15, 30, Inf),
                                            labels = c("Light", "Moderate", "Heavy", "Extensive"), right = FALSE)


Custchurn_filtered$Distance.Charge.Group <- cut(Custchurn_filtered$Avg.Monthly.Long.Distance.Charges, c(0, 10, 30, 60, Inf),
                                                labels = c("Low", "Moderate", "High", "Very High"), right = FALSE)

print (Custchurn_filtered)

str(Custchurn_filtered)  # To check which columns are factors now #

### To convert all categorical predictors to factors ###

Custchurn_filtered <- Custchurn_filtered %>%
  mutate(across(where(is.character), as.factor))

Custchurn_filtered <- Custchurn_filtered %>%
  mutate(across(c(
    Dependents, Device.Protection.Plan, Internet.Service, Married,
    Multiple.Lines, Paperless.Billing, Premium.Tech.Support, 
    Unlimited.Data
  ), as.factor))

### To build a Logistic Regression Model ###

## To define subset with bucketed variables ##

Custchurn_model_data <- Custchurn_filtered %>%
  select(Churn, Satisfaction.Score, Age.Group, Tenure.Group, Monthly.Charge.Group,
         Unlimited.Data, Internet.Service, Contract, Payment.Method)

## To set seed and split into training and testing sets ##

set.seed(123)
train_index <- createDataPartition(Custchurn_model_data$Churn, p = 0.7, list = FALSE)
train_data <- Custchurn_model_data[train_index, ]
test_data <- Custchurn_model_data[-train_index, ]

log_model <- glm(Churn ~ Satisfaction.Score + Age.Group + Tenure.Group + Monthly.Charge.Group + Unlimited.Data + Internet.Service + Contract + Payment.Method, 
    data = train_data, family = "binomial")   # To build the model #

summary(log_model)

## To predict on test set ##

predicted <- predict(log_model, newdata = test_data, type = "response")
predicted_class <- ifelse(predicted > 0.5, "1", "0")
predicted_class <- factor(predicted_class, levels = c("0", "1"))

### To analyze the model accuracy (confusion Matrix) ###

conf_matrix <- confusionMatrix(predicted_class, test_data$Churn, positive = "1")

print(conf_matrix)

### To generate ROC and AUC ###

roc_curve <- roc(test_data$Churn, predicted)
plot(roc_curve, col = "blue", main = "ROC Curve - Customer Churn")
abline(a = 0, b = 1, lty = 2, col = "red")
auc(roc_curve)


### End of program ###
