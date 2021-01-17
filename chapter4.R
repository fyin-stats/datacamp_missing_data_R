#############################
#### 
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
               "ggplot2", "devtools", "stringr", "stringi", "NHANES", "VIM", "simputation", "missForest",
               "mice")
ipak(packages)


#############################
#############################
# multiple imputation by bootstrapping
# imputed values can have uncertainty
# 


# Pros:
# Works with any imputation method
# Can approximate quantities that are hard to compute analytically
# Work with MCAR and MAR data


# Cons:
# Slow for many replicates or time-consuming computations
# 

# Bootstrapping in practice
# 
# calc_correlation

# data_boot
# data_imp
# calculate statistics of interests
# 
# boot function

# plot on bootstrap results
# CI, boot.ci function, conf, 95% sure

# 

# Wrapping imputation & modeling in a function
# 
# Whenever you perform any analysis or modeling on imputed data, you should account for the uncertainty from imputation. Running a model on a dataset imputed only once ignores the fact that imputation estimates the missing values with uncertainty. Standard errors from such a model tend to be too small. The solution to this is multiple imputation and one way to implement it is by bootstrapping.
# 
# In the upcoming exercises, you will work with the familiar biopics data. The goal is to use multiple imputation by bootstrapping and linear regression to see if, based on the data at hand, biographical movies featuring females earn less than those about males.
# 
# Let's start with writing a function that creates a bootstrap sample, imputes it, and fits a linear regression model.


calc_gender_coef <- function(data, indices) {
    # Get bootstrap sample
    data_boot <- data[indices, ]
    # Impute with kNN imputation
    data_imp <- kNN(data_boot, k = 5)
    # Fit linear regression
    linear_model <- lm(earnings ~ sub_sex + sub_type + year, data = data_imp)
    # Extract and return gender coefficient
    gender_coefficient <- coef(linear_model)[2]
    return(gender_coefficient)
}


# 
# Running the bootstrap
# 
# Good job writing calc_gender_coef() in the last exercise! This function creates a bootstrap sample, imputes it and, outputs the linear regression coefficient describing the impact of movie subject's being a female on the movie's earnings.
# 
# In this exercise, you will use the boot package in order to obtain a bootstrapped distribution of such coefficients. The spread of this distribution will capture the uncertainty from imputation. You will also look at how the bootstrapped distribution differs from a single-time imputation and regression. Let's do some bootstrapping!


# 
# Load the boot library
library(boot)

# Run bootstrapping on biopics data
boot_results <- boot(biopics, statistic = calc_gender_coef, R = 50)

# Print and plot bootstrapping results
print(boot_results)
plot(boot_results)



##############################
# Bootstrapping confidence intervals
# 
# Having bootstrapped the distribution of the female-effect coefficient in the last exercise, you can now use it to estimate a confidence interval. It will allow you to make the following assessment about your data: "Given the uncertainty from imputation, we are 95% sure that the female-effect on earnings is between a and b", where a and b are the lower and upper bounds of the interval.
# 
# In the last exercise, you have run bootstrapping with R = 50 replicates. In most applications, however, this is not enough. In this exercise, you can use boot_results that were prepared for you using 1000 replicates. First, you will look at the bootstrapped distribution to see if it looks normal. If so, you can then rely on the normal distribution to calculate the confidence interval.


###############################
###############################
# Multiple imputation by chained equations
###############################
###############################


# MICE algorithm

# MICE: pros and cons
# Pros
# Requires fewer replications than the bootstrap
# Works for MAR and MCAR
# Allows for sensitivity analysis towards MNAR data

# Cons
# only works with selected imputation methods
# requires more tuning effort (model selection, choosing predictors)

# mice, m = 20
# 
# fit a linear regression model to each imputed data set
# lm_multimp <- with( , lm())



####################################
# analyzing pooled results
# summary() conf.int, conf.level










#######################################
# Choosing method per variable type
#######################################
# 1, continuous variables
# 2, binary variables
# 3, categorical variables (unordered factors)
# 4, factor variables (ordered factors)


###########################################
# predictor matrix
###########################################
# choosing predictors for each variable
# ideally, a proper model selection should be performed
# a quick alternative, use variables correlated with the target
# 



# The mice flow: mice - with - pool
# 
# Multiple imputation by chained equations, or MICE, allows us to estimate the uncertainty from imputation by imputing a data set multiple times with model-based imputation, while drawing from conditional distributions. This way, each imputed data set is slightly different. Then, an analysis is conducted on each of them and the results are pooled together, yielding the quantities of interest, alongside their confidence intervals that reflect the imputation uncertainty.
# 
# In this exercise, you will practice the typical MICE flow: mice() - with() - pool(). You will perform a regression analysis on the biopics data to see which subject occupation, sub_type, is associated with highest movie earnings. Let's play with mice!
# 
# 
# 

# Load mice package
library(mice)

# Impute biopics with mice using 5 imputations
biopics_multiimp <- mice(biopics, m = 5, seed = 3108)

# Fit linear regression to each imputed data set 
lm_multiimp <- with(biopics_multiimp, lm(earnings ~ year + sub_type))

# Pool and summarize regression results
lm_pooled <- pool(lm_multiimp)
summary(lm_pooled, conf.int = TRUE, conf.level = 0.95)

##################################


# Choosing default models
# 
# MICE creates a separate imputation model for each variable in the data. What kind of model it is depends on the type of the variable in question. A popular way to specify the kinds of models we want to use is set a default model for each of the four variable types.
# 
# You can do this by passing the defaultMethod argument to mice(), which should be a vector of length 4 containing the default imputation methods for:
#     
#     Continuous variables,
# Binary variables,
# Categorical variables (unordered factors),
# Factor variables (ordered factors).
# 
# In this exercise, you will take advantage of mice's documentation to view the list of available methods and to pick the desired ones for the algorithm to use. Let's do some model selection!


# 
# Impute biopics using the methods specified in the instruction
biopics_multiimp <- mice(biopics, m = 20, 
                         defaultMethod = c("cart", "lda", "pmm", "polr"))

# Print biopics_multiimp
print(biopics_multiimp)



# Using predictor matrix
#
# Create predictor matrix with minimum correlation of 0.1
pred_mat <- quickpred(biopics, mincor = 0.1)

# Impute biopics with mice
biopics_multiimp <- mice(biopics, 
                         m=10, 
                         predictorMatrix = pred_mat,
                         seed = 3108)

# Print biopics_multiimp
print(biopics_multiimp)






################## Case study: civil liberties in Africa
##################
##################


################## Assessing imputation quality with MICE
################## VIM for all imputed data sets from mice can be cumbersome 
################## can use stripplot
###################

# Analyzing missing data patterns
# 
# The first step in working with incomplete data is to gain some insights into the missingness patterns, and a good way to do it is with visualizations. You will start your analysis of the africa data with employing the VIM package to create two visualizations: the aggregation plot and the spine plot. They will tell you how many data are missing, in which variables and configurations, and whether we can say something about the missing data mechanism. Let's kick off with some plotting!

#
# Load VIM
library(VIM)

# Draw a combined aggregation plot of africa
africa %>%
    aggr(combined = TRUE, numbers = TRUE)


#
# Load VIM
library(VIM)

# Draw a combined aggregation plot of africa
africa %>%
    aggr(combined = TRUE, numbers = TRUE)

# Draw a spine plot of country vs trade
africa %>% 
    select(country, trade) %>%
    spineMiss()

#########################################
### 
# Load mice
library(mice)

# Impute africa with mice
africa_multiimp <- mice(africa, m = 5, defaultMethod = "cart", seed = 3108)

# Draw a stripplot of gdp_pc versus trade
stripplot(africa_multiimp, gdp_pc ~ trade | .imp, pch = 20, cex = 2)