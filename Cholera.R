# Importing Dataset ----

library(readr)
cholera <- read_csv("data/cholera.csv")
View(cholera)

# Dimensions ----
# Dimensions refer to the number of observations (rows) and the number of
# attributes/variables/features (columns). Execute the following commands to
# display the dimensions of your datasets:

dim(cholera)

# Data Types ----
# Knowing the data types will help you to identify the most appropriate
# visualization types and algorithms that can be applied. It can also help you
# to identify the need to convert from categorical data (factors) to integers
# or vice versa where necessary. Execute the following command to identify the
# data types:

sapply(cholera, class)

# Descriptive Statistics ----
## Measures of Frequency ----

cholera_freq <- cholera$male
cbind(frequency = table(cholera_freq),
      percentage = prop.table(table(cholera_freq)) *100)

cholera_freq <- cholera$education
cbind(frequency = table(cholera_freq),
      percentage = prop.table(table(cholera_freq)) *100)

cholera_freq <- cholera$wateryDiarrhoea
cbind(frequency = table(cholera_freq),
      percentage = prop.table(table(cholera_freq)) *100)

cholera_freq <- cholera$dehydration
cbind(frequency = table(cholera_freq),
      percentage = prop.table(table(cholera_freq)) *100)

cholera_freq <- cholera$vomiting
cbind(frequency = table(cholera_freq),
      percentage = prop.table(table(cholera_freq)) *100)

cholera_freq <- cholera$muscleCramps
cbind(frequency = table(cholera_freq),
      percentage = prop.table(table(cholera_freq)) *100)

cholera_freq <- cholera$rapidHeartRate
cbind(frequency = table(cholera_freq),
      percentage = prop.table(table(cholera_freq)) *100)

cholera_freq <- cholera$choleraDiagnosis
cbind(frequency = table(cholera_freq),
      percentage = prop.table(table(cholera_freq)) *100)

## Measures of Central Tendency - Mode ----

cholera_male_mode <- names(table(cholera$male))[
  which(table(cholera$male) == max(table(cholera$male)))
]

cholera_education_mode <- names(table(cholera$education))[
  which(table(cholera$education) == max(table(cholera$education)))
]

cholera_wateryDiarrhoea_mode <- names(table(cholera$wateryDiarrhoea))[
  which(table(cholera$wateryDiarrhoea) == max(table(cholera$wateryDiarrhoea)))
]

cholera_dehydration_mode <- names(table(cholera$dehydration))[
  which(table(cholera$dehydration) == max(table(cholera$dehydration)))
]

cholera_vomiting_mode <- names(table(cholera$vomiting))[
  which(table(cholera$vomiting) == max(table(cholera$vomiting)))
]

cholera_muscleCramps_mode <- names(table(cholera$muscleCramps))[
  which(table(cholera$muscleCramps) == max(table(cholera$muscleCramps)))
]

cholera_rapidHeartRate_mode <- names(table(cholera$rapidHeartRate))[
  which(table(cholera$rapidHeartRate) == max(table(cholera$rapidHeartRate)))
]

cholera_choleraDiagnosis_mode <- names(table(cholera$choleraDiagnosis))[
  which(table(cholera$choleraDiagnosis) == max(table(cholera$choleraDiagnosis)))
]

# Summary

summary(cholera)

## Check Columns with Missing Values ----

check_missing_values <- function(data) {
  missing_columns <- colnames(data)[apply(data, 2, function(x) any(is.na(x)))]
  if (length(missing_columns) > 0) {
    cat("Columns with missing values in dataset: ", paste(missing_columns, collapse = ", "), "\n")
  } else {
    cat("No missing values in any column of the dataset.\n")
  }
}

# Call the function with your dataset 'cholera'

check_missing_values(cholera)

# Conclusions: 
# 1. Type of missing data - MNAR
# 2. Imputation method - MICE

## Data Imputation ----
# Data needs to be imputed to further proceed with the descriptive statistics
### Install Required Packages to Begin Imputation ----

if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")


if (!is.element("naniar", installed.packages()[, 1])) {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("naniar")


if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggplot2")

# We use the MICE package to perform data imputation

if (!is.element("mice", installed.packages()[, 1])) {
  install.packages("mice", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("mice")

if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("Amelia")

### Confirm the "missingness" in the Dataset before Imputation ----
# Are there missing values in the dataset?

any_na(cholera)

# How many?

n_miss(cholera)

# What is the percentage of missing data in the entire dataset?

prop_miss(cholera)

# How many missing values does each variable have?

cholera %>% is.na() %>% colSums()

# What is the number and percentage of missing values grouped by
# each variable?

miss_var_summary(cholera)

# What is the number and percentage of missing values grouped by
# each observation?

miss_case_summary(cholera)

# Which variables contain the most missing values?

gg_miss_var(cholera)

# Where are missing values located (the shaded regions in the plot)?

vis_miss(cholera) + theme(axis.text.x = element_text(angle = 80))

# Which combinations of variables are missing together?

gg_miss_upset(cholera)

# Assuming cholera is your dataset

library(mice)

# Create an empty predictor matrix

predictorMatrix <- matrix(0, ncol = ncol(cholera), nrow = ncol(cholera), 
                          dimnames = list(names(cholera), names(cholera)))

# Specify the imputation methods for the 'education' and 'vomiting' columns

predictorMatrix['education', ] <- "polr"
predictorMatrix['vomiting', ] <- "logreg"

# Run mice with the specified imputation methods

cholera_mice <- mice(cholera, m = 11, method = 'pmm', seed = 7, predictorMatrix = predictorMatrix)

View(cholera_mice)


# We can use multiple scatter plots (a.k.a. strip-plots) to visualize how
# random the imputed data is in each of the 11 datasets.

# For education, which is a categorical variable

stripplot(cholera_mice,
          education ~ age | .imp,
          pch = 15, cex = 1.5)

# For vomiting, which is a binary variable

stripplot(cholera_mice,
          vomiting ~ age | .imp,
          pch = 17, cex = 1.5)

### Impute the missing data ----

cholera_imputed <- mice::complete(cholera_mice, 1)

### Confirm the "missingness" in the Imputed Dataset ----
# A textual confirmation that the dataset has no more missing values in any
# feature:

miss_var_summary(cholera_imputed)

# A visual confirmation that the dataset has no more missing values in any
# feature:

Amelia::missmap(cholera_imputed)


# Are there missing values in the dataset?

any_na(cholera_imputed)

# How many?

n_miss(cholera_imputed)

# What is the percentage of missing data in the entire dataset?

prop_miss(cholera_imputed)

# How many missing values does each variable have?

cholera_imputed %>% is.na() %>% colSums()

# What is the number and percentage of missing values grouped by
# each variable?

miss_var_summary(cholera_imputed)

# What is the number and percentage of missing values grouped by
# each observation?

miss_case_summary(cholera_imputed)

# Which variables contain the most missing values?

gg_miss_var(cholera_imputed)

# We require the "ggplot2" package to create more appealing visualizations
# Where are missing values located (the shaded regions in the plot)?

vis_miss(cholera_imputed) + theme(axis.text.x = element_text(angle = 80))

# Which combinations of variables are missing together?
# Note: The following command should give you an *error* stating that at least 2
# variables should have missing data for the plot to be created.

# gg_miss_upset(cholera_imputed)

## Measures of Distribution ----
# Standard Deviation

sapply(cholera[, c(1, 2, 3, 4, 5, 6, 7, 8)], sd)
sapply(cholera_imputed[, c(1, 2, 3, 4, 5, 6, 7, 8)], sd)

# Variance

sapply(cholera[, c(1, 2, 3, 4, 5, 6, 7, 8)], var)
sapply(cholera_imputed[, c(1, 2, 3, 4, 5, 6, 7, 8)], var)

# Kurtosis

if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

sapply(cholera_imputed[, c(1, 2, 3, 4, 5, 6, 7, 8)],kurtosis, type=2)

# Skewness

sapply(cholera_imputed[, c(1, 2, 3, 4, 5, 6, 7, 8)],skewness, type=2)

## Measure of Relationship ----
# Covariance

cholera_imputed_cov <- cov(cholera_imputed [c(1, 2, 3, 4, 5, 6, 7, 8)])
View(cholera_imputed_cov)

# Correlation

cholera_imputed_cor <- cor(cholera_imputed [c(1, 2, 3, 4, 5, 6, 7, 8)])
View(cholera_imputed_cor)

# Inferential Statistics ----
# Fit the logistic regression model
cholera_inferential <- glm(choleraDiagnosis ~ age + education + wateryDiarrhoea + 
                       dehydration + vomiting + muscleCramps + 
                       rapidHeartRate, 
                     data = cholera_imputed, family = "binomial")

# Perform ANOVA for the entire model
anova_result <- anova(cholera_inferential, test = "Chisq")
summary(anova_result)

# The degrees of freedom (Df) represent the number of parameters estimated in the model.
# Deviance is a measure of how well the model fits the data. It decreases as the model improves.
# Resid. Df is the residual degrees of freedom, indicating the number of data points minus the number of parameters estimated.
# Resid. Dev is the residual deviance, a measure of how well the model explains the variability not accounted for by the predictors.
# Pr(>Chi) represents the p-value associated with the Chi-square test of the entire model.

# The first quartile (1st Qu.) of p-values is extremely low, suggesting that the overall model is statistically significant.
# Median and mean values of deviance and p-values provide additional insights into the goodness of fit.

# Note: One NA value indicates that there might be missing data in the model.

# It's important to carefully consider these results to assess the overall effectiveness of the logistic regression model.

# Univariate Plots ----
# Univariate plot for Age
ggplot(cholera_imputed, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Univariate plot for Education
ggplot(cholera_imputed, aes(x = education)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Education", x = "Education", y = "Frequency")

# Univariate plot for Rapid Heart Rate
ggplot(cholera_imputed, aes(x = rapidHeartRate)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Rapid Heart Rate", x = "Rapid Heart Rate", y = "Frequency")

# Multivariate Plots ----
# Multivariate plot for Age and Rapid Heart Rate
ggplot(cholera_imputed, aes(x = age, y = rapidHeartRate)) +
  geom_point(color = "red", alpha = 0.5) +
  labs(title = "Scatter Plot of Age vs. Rapid Heart Rate", x = "Age", y = "Rapid Heart Rate")

# Data Transformation ----

class_distribution <- table(cholera_imputed$choleraDiagnosis)
print(class_distribution)

## Undersampling ----
# Load the necessary libraries

library(caret)

# Convert 'choleraDiagnosis' to a factor

cholera_imputed$choleraDiagnosis <- as.factor(cholera_imputed$choleraDiagnosis)

# Identify the positive and negative class samples

positive_samples <- cholera_imputed[cholera_imputed$choleraDiagnosis == 1, ]
negative_samples <- cholera_imputed[cholera_imputed$choleraDiagnosis == 0, ]

# Undersample the majority class (in this case, the negative class)

negative_samples_undersampled <- negative_samples[sample(nrow(negative_samples), nrow(positive_samples)), ]

# Combine the undersampled negative class with the positive class

cholera_undersampled <- rbind(negative_samples_undersampled, positive_samples)

# Check the class distribution after applying undersampling

table(cholera_undersampled$choleraDiagnosis)

# Conclusion: The class distribution is now balanced after performing undersampling
# This balance in the number of instances for each class will help mitigate the 
# effects of class imbalance and ensure that your logistic regression model can 
# learn from both classes effectively, leading to more accurate predictions and 
# improved model performance.

# Initial Data Analysis

summary(cholera_undersampled)

# Histogram for Age

hist(cholera_undersampled$age, main = "Histogram for Age")

# Histogram for Education

hist(cholera_undersampled$education, main = "Histogram for Education")

# Histogram for Watery Diarrhoea

hist(cholera_undersampled$wateryDiarrhoea, main = "Histogram for Watery Diarrhoea")

# Histogram for Dehydration

hist(cholera_undersampled$dehydration, main = "Histogram for Dehydration")

# Histogram for Vomiting

hist(cholera_undersampled$vomiting, main = "Histogram for Vomiting")

# Histogram for Muscle Cramps

hist(cholera_undersampled$muscleCramps, main = "Histogram for Muscle Cramps")

# Histogram for Rapid Heart Rate

hist(cholera_undersampled$rapidHeartRate, main = "Histogram for Rapid Heart Rate")

# Bar plot for Cholera Diagnosis

barplot(table(cholera_undersampled$choleraDiagnosis), main = "Bar Plot for Cholera Diagnosis")

# Conclusion: The variables that appear to be positively skewed are vomiting, muscle  
# cramps and rapid heart rate. This means that their distributions are skewed to the
# right, and the tail of the distribution extends towards the higher values. In this 
# case, the mean is likely to be greater than the median for these variables.

# Evaluation Metrics ----

# Identify the number of instances that belong to each class (distribution or class breakdown).

cholera_undersampled_choleraDiagnosis_freq <- cholera_undersampled$choleraDiagnosis
class_distribution <- cbind(frequency = table(cholera_undersampled_choleraDiagnosis_freq),
                            percentage = prop.table(table(cholera_undersampled_choleraDiagnosis_freq)) * 100)

# Print the class distribution and percentage breakdown

print(class_distribution)

# Dataset Splitting ----
# Define a 75:25 train:test data split of the dataset.
# That is, 75% of the original data will be used to train the model and
# 25% of the original data will be used to test the model.

train_index <- createDataPartition(cholera_undersampled$choleraDiagnosis,
                                   p = 0.75,
                                   list = FALSE)
cholera_train <- cholera_undersampled[train_index, ]
cholera_test <- cholera_undersampled[-train_index, ]

# Cross Validation ----
# 5-fold cross-validation

train_control <- trainControl(method = "cv", number = 5)

## Random Forest ----
# Train the Random Forest model

set.seed(7)
cholera_model_rf <- train(choleraDiagnosis ~ ., data = cholera_train, method = "rf",
                          metric = "Accuracy", trControl = train_control)

# Print the trained model

print(cholera_model_rf)

# Conclusion: The random forest model has been successfully trained on your 
# cholera dataset. The results show that the model has achieved perfect accuracy
# and kappa values across the tested values of mtry. The final selected value 
# for the model was mtry = 2. You can further analyze the model and make any 
# necessary adjustments based on this information.

### Calculating Metrics ----
# Compute the metric yourself using the test dataset
# Confusion matrix for binary classification problem

cholera_predictions_rf <- predict(cholera_model_rf, cholera_test)
cholera_confusion_matrix_rf <- caret::confusionMatrix(cholera_predictions_rf, cholera_test$choleraDiagnosis)
print(cholera_confusion_matrix_rf)

### Confusion Matrix ----
# Display a graphical confusion matrix

fourfoldplot(as.table(cholera_confusion_matrix_rf), color = c("grey", "lightblue"),
             main = "Confusion Matrix (RF)")

# Save the model

saveRDS(cholera_model_rf, "Cholera_Model/cholera_model_rf.rds")

## Logistic Regression 1 ----
# We can use "regLogistic" 
# Notice the data transformation applied when we call the train function
# in caret, i.e., a standardize data transform (center + scale)

set.seed(7)
cholera_caret_model_logistic <-
  train(choleraDiagnosis ~ ., data = cholera_train,
        method = "regLogistic", metric = "Accuracy",
        preProcess = c("center", "scale"), trControl = train_control)

# Display the model's details 

print(cholera_caret_model_logistic)

### Calculating Metrics ----
# Make predictions

cholera_predictions_lr <- predict(cholera_caret_model_logistic,
                                  cholera_test)

# Display the model's evaluation metrics 

cholera_confusion_matrix_lr <-
  caret::confusionMatrix(cholera_predictions_lr,
                         cholera_test$choleraDiagnosis)
print(cholera_confusion_matrix_lr)

### Confusion Matrix ----

fourfoldplot(as.table(cholera_confusion_matrix_lr), color = c("grey", "lightblue"),
             main = "Confusion Matrix (LR 1)")

# Save the model

saveRDS(cholera_caret_model_logistic, "Cholera_Model/cholera_model.rds")

## Logistic Regression 2 ----
# We can use "glmnet" 

set.seed(7)
cholera_caret_model_logistic_two <- train(
  choleraDiagnosis ~ ., 
  data = cholera_train,
  method = "glmnet",
  family = "binomial",
  metric = "Accuracy",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  maxit = 1000
)

# Check for multicollinearity
findLinearCombos(cholera_train[, -which(names(cholera_train) %in% "choleraDiagnosis")])

print(cholera_caret_model_logistic_two)

### Calculating Metrics ----
# Make predictions

cholera_predictions_lr_two <- predict(cholera_caret_model_logistic_two,
                                      cholera_test)

# Display the model's evaluation metrics 

cholera_confusion_matrix_lr_two <-
  caret::confusionMatrix(cholera_predictions_lr_two,
                         cholera_test$choleraDiagnosis)
print(cholera_confusion_matrix_lr_two)

### Confusion Matrix ----

fourfoldplot(as.table(cholera_confusion_matrix_lr_two), color = c("grey", "lightblue"),
             main = "Confusion Matrix (LR 2)")

# Save the model

saveRDS(cholera_caret_model_logistic_two, "Cholera_Model/cholera_model_two.rds")

# Performance Comparison using Resamples ----
# Create a list of the model results
model_list <- list(
  Random_Forest = cholera_model_rf,
  Logistic_Regression_1 = cholera_caret_model_logistic,
  Logistic_Regression_2 = cholera_caret_model_logistic_two
)

# Pass the list as an argument to the resamples function
results <- resamples(model_list)

# Display the summary of resampling results
summary(results)

# Conclusion ----

# After evaluating three different models, it is evident that Logistic Regression 2 
# performs the best in terms of accuracy and balance between sensitivity and specificity.
# The model employs a glmnet algorithm with alpha value set to 0.1 and lambda value 
# set to 0.0294.
# Its accuracy of 67.08% and balanced sensitivity and specificity make it a relatively
# robust choice for classifying cholera cases.
# The regularization approach used in this model aids in preventing overfitting, 
# ensuring generalizability to unseen data.
# Therefore, Logistic Regression 2 is the preferred model for the classification task at hand.

# Bootstrapping for the Logistic Regression 2 Model ----

# Number of bootstrap samples
num_bootstrap_samples <- 1000

# Create an empty vector to store bootstrapped accuracy values
bootstrap_accuracies <- numeric(num_bootstrap_samples)

# Set seed for reproducibility
set.seed(7)

# Perform bootstrapping
for (i in 1:num_bootstrap_samples) {
  # Sample with replacement from the training data
  bootstrap_sample <- cholera_train[sample(nrow(cholera_train), replace = TRUE), ]
  
  # Train Logistic Regression 2 model on the bootstrap sample
  cholera_model_lr_two_bootstrap <- train(
    choleraDiagnosis ~ ., 
    data = bootstrap_sample,
    method = "glmnet",
    family = "binomial",
    metric = "Accuracy",
    preProcess = c("center", "scale"),
    trControl = trainControl(method = "cv", number = 5, verboseIter = FALSE),
    maxit = 1000
  )
  
  # Make predictions on the test set
  cholera_predictions_bootstrap <- predict(cholera_model_lr_two_bootstrap, cholera_test)
  
  # Calculate accuracy and store it
  bootstrap_accuracies[i] <- confusionMatrix(cholera_predictions_bootstrap, cholera_test$choleraDiagnosis)$overall["Accuracy"]
}

# Display summary statistics of bootstrapped accuracies
summary(bootstrap_accuracies)

# Visualize the distribution of bootstrapped accuracies
hist(bootstrap_accuracies, main = "Bootstrapped Accuracies for Logistic Regression 2", xlab = "Accuracy")

