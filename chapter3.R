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
               "ggplot2", "devtools", "stringr", "stringi", "NHANES", "VIM", "simputation")
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