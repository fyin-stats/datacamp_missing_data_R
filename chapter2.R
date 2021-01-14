#######################
#######################
########## chapter 2
#######################
#######################

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
               "ggplot2", "devtools", "stringr", "stringi", "NHANES", "VIM")
ipak(packages)

########################
########################
# chapter 2 imputation
########################
########################
# imputation = making an educated guess about what the missing values might be
# Donor-based imputation: missing values are filled in using other, complete observations
# model-based imputation: missing values are predicted with a statistical or machine learning model


# Three donor based methods:
# Mean imputation
# hot-deck imputation
# kNN imputation

# Mean imputation works well for time-series data that randomly fluctuate around a long-term
# average

# For cross-sectional data, mean imputation is often a very poor choice
# 1, destroys relation between variables
# 2, there is no variance in the imputed values

# Mean imputation in practice: replace NA by column mean
# Assesing imputation quality: margin plot (R function, marginplot)

# Troubles with mean imputation: destroying relation between variables
# After mean-imputing Height and Weight, their positive correlation is weaker.
# Models predicting one using the other will be fooled by the outlying imputed values and will produce 
# biased results

# No variability in imputed data, with less variance the data, all standard errors will be underestimated.
# this prevents reliable hypothesis testing and calculating confidence intervals

# Median and mode imputation
# median imputation is a better choice when there are outliers in the data
# for categorical variables, we cannot compute neither mean or median, so we use mode instead

# Both median and mode imputation present the same drawbacks as mean imputation





############################################################
### Exercise 1
############################################################
# Smelling the danger of mean imputation
# 
# One of the most popular imputation methods is the mean imputation, in which missing values in a variable are replaced with the mean of the observed values in this variable. However, in many cases this simple approach is a poor choice. Sometimes a quick look at the data can already alert you to the dangers of mean-imputing.
# 
# In this chapter, you will be working with a subsample of the Tropical Atmosphere Ocean (tao) project data. The dataset consists of atmospheric measurements taken in two different time periods at five different locations. The data comes with the VIM package.
# 
# In this exercise you will familiarize yourself with the data and perform a simple analysis that will indicate what the consequences of mean imputation could be. Let's take a look at the tao data!

# Print first 10 observations
head(tao, 10)

# Get the number of missing values per column
tao %>%
    is.na() %>% 
    colSums()

# Calculate the number of missing values in air_temp per year
tao %>% 
    group_by(year) %>% 
    summarize(num_miss = sum(is.na(air_temp)))


### Exercise 2
# Mean-imputing the temperature
# 
# Mean imputation can be a risky business. If the variable you are mean-imputing is correlated with other variables, this correlation might be destroyed by the imputed values. You saw it looming in the previous exercise when you analyzed the air_temp variable.
# 
# To find out whether these concerns are valid, in this exercise you will perform mean imputation on air_temp, while also creating a binary indicator for where the values are imputed. It will come in handy in the next exercise, when you will be assessing your imputation's performance. Let's fill in those missing values!

tao_imp <- tao %>% 
    # Create a binary indicator for missing values in air_temp
    mutate(air_temp_imp = ifelse(is.na(air_temp), TRUE, FALSE)) %>%
    # Impute air_temp with its mean
    mutate(air_temp = ifelse(is.na(air_temp), mean(air_temp, na.rm = TRUE), air_temp))

# Print the first 10 rows of tao_imp
head(tao_imp, 10)


### Exercise 3
# 
# Assessing imputation quality with margin plot
# 
# In the last exercise, you have mean-imputed air_temp and added an indicator variable to denote which values were imputed, called air_temp_imp. Time to see how well this works.
# 
# Upon examining the tao data, you might have noticed that it also contains a variable called sea_surface_temp, which could reasonably be expected to be positively correlated with air_temp. If that's the case, you would expect these two temperatures to be both high or both low at the same time. Imputing mean air temperature when the sea temperature is high or low would break this relation.
# 
# To find out, in this exercise you will select the two temperature variables and the indicator variable and use them to draw a margin plot. Let's assess the mean imputation!


# Draw a margin plot of air_temp vs sea_surface_temp
tao_imp %>% 
    select(air_temp, sea_surface_temp, air_temp_imp) %>%
    marginplot(delimiter = "imp")