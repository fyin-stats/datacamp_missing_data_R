#############################
#### Model-based imputation
#############################
########################
########################
#
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}

packages <- c( "ggplot2", "sparklyr","foreach",
               "doParallel", "dplyr",
               "xtable", "data.table",  "DescTools", 
               "ggplot2", "devtools", "stringr", "stringi", "NHANES", "VIM", "simputation", "missForest")
ipak(packages)


############################
# chapter 3
############################

## model-based imputation step by step
## a data frame with 4 columns
## A, B, C, D
## 1, predict missing values in A
## 2, treat data imputed in A as observed and predict missing values in C
## 3, treat data imputed in C as observed and predict A again where it was originally missing
## 4, continue until convergence

## how to choose the model
## continuous: linear regression
## binary: logistic regression
## categorical: multinomial
## count: poisson


## library(simputation)
## impute_lm

## check if they were indeed imputed
## initialize missing values in hotdeck and save missing locations
## 

## detecting convergence
## for loop (e.g., 5)
## diff_height
## diff_weight
## check difference


# linear regression imputation

# Load the simputation package
library(simputation)

# Impute air_temp and humidity with linear regression
formula <- air_temp + humidity ~ year + latitude + sea_surface_temp
tao_imp <- impute_lm(tao, formula)

# Check the number of missing values per column
tao_imp %>% 
    is.na() %>% 
    colSums()

# Print rows of tao_imp in which air_temp or humidity are still missing 
tao_imp %>% 
    filter(is.na(air_temp) | is.na(humidity) )

### three rows not imputed, because the sea_surface_temp values
### are missing


### 
# Initializing missing values & iterating over variables
# 
# As you have just seen, running impute_lm() might not fill-in all the missing values. To ensure you impute all of them, you should initialize the missing values with a simple method, such as the hot-deck imputation you learned about in the previous chapter, which simply feeds forward the last observed value.
# 
# Moreover, a single imputation is usually not enough. It is based on the basic initialized values and could be biased. A proper approach is to iterate over the variables, imputing them one at a time in the locations where they were originally missing.
# 
# In this exercise, you will first initialize the missing values with hot-deck imputation and then loop five times over air_temp and humidity from the tao data to impute them with linear regression. Let's get to it!


# 
# Initialize missing values with hot-deck
tao_imp <- hotdeck(tao)

# Create boolean masks for where air_temp and humidity are missing
missing_air_temp <- tao_imp$air_temp_imp
missing_humidity <- tao_imp$humidity_imp

for (i in 1:5) {
    # Set air_temp to NA in places where it was originally missing and re-impute it
    tao_imp$air_temp[missing_air_temp] <- NA
    tao_imp <- impute_lm(tao_imp, air_temp ~ year + latitude + sea_surface_temp + humidity)
    # Set humidity to NA in places where it was originally missing and re-impute it
    tao_imp$humidity[missing_humidity] <- NA
    tao_imp <- impute_lm(tao_imp, humidity ~ year + latitude + sea_surface_temp + air_temp)
}


## 
# Detecting convergence
# 
# Great job iterating over the variables in the last exercise! But how many iterations are needed? When the imputed values don't change with the new iteration, we can stop.
# 
# You will now extend your code to compute the differences between the imputed variables in subsequent iterations. To do this, you will use the Mean Absolute Percentage Change function, defined for you as follows:
# 
# mapc <- function(a, b) {
#   mean(abs(b - a) / a, na.rm = TRUE)
# }
# 
# mapc() outputs a single number that tells you how much b differs from a. You will use it to check how much the imputed variables change across iterations. Based on this, you will decide how many of them are needed!
# 
# The boolean masks missing_air_temp and missing_humidity are available for you, as is the hotdeck-initialized tao_imp data.


diff_air_temp <- c()
diff_humidity <- c()

for (i in 1:5) {
    # Assign the outcome of the previous iteration (or initialization) to prev_iter
    prev_iter <- tao_imp
    # Impute air_temp and humidity at originally missing locations
    tao_imp$air_temp[missing_air_temp] <- NA
    tao_imp <- impute_lm(tao_imp, air_temp ~ year + latitude + sea_surface_temp + humidity)
    tao_imp$humidity[missing_humidity] <- NA
    tao_imp <- impute_lm(tao_imp, humidity ~ year + latitude + sea_surface_temp + air_temp)
    # Calculate MAPC for air_temp and humidity and append them to previous iteration's MAPCs
    diff_air_temp <- c(diff_air_temp, mapc(prev_iter$air_temp, tao_imp$air_temp))
    diff_humidity <- c(diff_humidity, mapc(prev_iter$humidity, tao_imp$humidity))
}


#######
#######
# Replicating data variability
#######
#######

# mean imputation: no variability in imputed data
# we would like the imputation to replicate the variability of observed data
# what is a prediction, conditional distribution of the response variable

# Instead, we can draw from the estimated distribution
# 

# logistic regression imputation
# use rbinom to draw random samples
# 

# Logistic regression imputation
# 
# A popular choice for imputing binary variables is logistic regression. Unfortunately, there is no function similar to impute_lm() that would do it. That's why you'll write such a function yourself!
#     
#     Let's call the function impute_logreg(). Its first argument will be a data frame df, whose missing values have been initialized and only containing missing values in the column to be imputed. The second argument will be a formula for the logistic regression model.
# 
# The function will do the following:
# 
#     Keep the locations of missing values.
#     Build the model.
#     Make predictions.
#     Replace missing values with predictions.
# 
# Don't worry about the line creating imp_var - this is just a way to extract the name of the column to impute from the formula. Let's do some functional programming!


impute_logreg <- function(df, formula) {
    # Extract name of response variable
    imp_var <- as.character(formula[2])
    # Save locations where the response is missing
    missing_imp_var <- is.na(df[imp_var])
    # Fit logistic regression mode
    logreg_model <- glm(formula, data = df, family = binomial)
    # Predict the response
    preds <- predict(logreg_model, type = "response")
    # Sample the predictions from binomial distribution
    preds <- rbinom(length(preds), size = 1, prob = preds)
    # Impute missing values with predictions
    df[missing_imp_var, imp_var] <- preds[missing_imp_var]
    return(df)
}


################
## 
# Model-based imputation with multiple variable types
# 
# Great job on writing the function to implement logistic regression imputation with drawing from conditional distribution. That's pretty advanced statistics you have coded! In this exercise, you will combine what you learned so far about model-based imputation to impute different types of variables in the tao data.
# 
# Your task is to iterate over variables just like you have done in the previous chapter and impute two variables:
# 
#     is_hot, a new binary variable that was created out of air_temp, which is 1 if air_temp is at or above 26 degrees and is 0 otherwise;
#     humidity, a continuous variable you are already familiar with.
# 
# You will have to use the linear regression function you have learned before, as well as your own function for logistic regression. Let's get to it!



# Initialize missing values with hot-deck
tao_imp <- hotdeck(tao)

# Create boolean masks for where is_hota and humidity are missing
missing_is_hot <- tao_imp$is_hot_imp
missing_humidity <- tao_imp$humidity_imp

for (i in 1:3) {
    # Set is_hot to NA in places where it was originally missing and re-impute it
    tao_imp$is_hot[missing_is_hot] <- NA
    tao_imp <- impute_logreg(tao_imp, is_hot ~ sea_surface_temp)
    # Set humidity to NA in places where it was originally missing and re-impute it
    tao_imp$humidity[missing_humidity] <- NA
    tao_imp <- impute_lm(tao_imp, humidity ~ sea_surface_temp + air_temp)
}





########### Tree-based models for imputation
########### 
########### nonparametric approach: no assumptions on relationships between variables
########### can pick up complex non-linear patterns
########### often better predictive performance compared to simple statistical models
###########

# missForest package: with randomForest under the hood
# results from all trees are aggregated
# 

# missForest algorithm
# 1, make an initial guess for missing values with mean imputation
# 2, sort the variables in ascending order by the amount of missing values
# 3, for each variable x, 
# fit a random forest to the observed of x (using other variables as predictors)
# use it to predict the missing part of x

# imputation error
# out-of-bag imputation error estimate
# continuous variables: normalized root mean squared error (NRMSE) for continuous variables
# proportion of falsely classified entries (PFC) for categorical varaiables

# in both cases, good performance leads to a value close to 0 and values around 1 indicate a poor results
# OOBerror, one error 
# variablewise = TRUE 

# Speed accuracy trade off
# ntree: linear reduction
# mtry: more reduction

# default : 100 trees
# 
# Load the missForest package
library(missForest)

# Impute biopics data using missForest
imp_res <- missForest(biopics)

# Extract imputed data and check for missing values
imp_data <- imp_res$ximp
print(sum(is.na(imp_data)))

# Extract and print imputation errors
imp_err <- imp_res$OOBerror
print(imp_err)



# 
# Impute biopics data with missForest computing per-variable errors
imp_res <- missForest(biopics, variablewise = TRUE)

# Extract and print imputation errors
per_variable_errors <- imp_res$OOBerror
print(per_variable_errors)

# Rename errors' columns to include variable names
names(per_variable_errors) <- paste(names(biopics), 
                                    names(per_variable_errors),
                                    sep = "_")

# Print the renamed errors
print(per_variable_errors)

