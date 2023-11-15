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
## Measures of frequency ----

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

## Measures of central tendency - Mode ----

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

## Check columns with missing values ----

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
### Install required packages to begin imputation ----

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

## Measures of distribution ----
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
