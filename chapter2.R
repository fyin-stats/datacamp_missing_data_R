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




### Hot deck imputation
### Dates back to 1950s, when data was stored on punched cards
### US Census

### Hot deck imputation: for each variable, replace every missing value with the last observed value
### Hot deck refers to the deck of punched cards actually being processed

### Cons:
### Requires data to be MCAR
### vanilla hot-deck can destroy relations between variables

### Pros:
### Fast
### imputed data are not constant
### simple tricks prevent breaking relations


### 
# hotdeck function from VIM package


# imputing within domains, simple improvement, specify domain vars
# sorting by correlated variables, specify ord_var

# 
# Load VIM package
library(VIM)

# Impute air_temp in tao with hot-deck imputation
tao_imp <- hotdeck(tao, variable = "air_temp")

# Check the number of missing values in each variable
tao_imp %>% 
    is.na() %>% 
    colSums()

# Draw a margin plot of air_temp vs sea_surface_temp
tao_imp %>% 
    select(air_temp, sea_surface_temp, air_temp_imp) %>% 
    marginplot(delimiter = "imp")



# Hot-deck tricks & tips I: imputing within domains
# 
# One trick that may help when hot-deck imputation breaks the relations between the variables is imputing within domains. What this means is that if the variable to be imputed is correlated with another, categorical variable, one can simply run hot-deck separately for each of its categories.
# 
# For instance, you might expect air temperature to depend on time, as we are seeing the average temperatures rising due to global warming. The time indicator you have available in the tao data is a categorical variable, year. Let's first check if the average air temperature is different in each of the two studied years and then run hot-deck within year domains. Finally, you will draw the margin plot again to assess the imputation performance.


# 
# Calculate mean air_temp per year
tao %>% 
    group_by(year) %>% 
    summarize(average_air_temp = mean(air_temp, na.rm = TRUE))

# Hot-deck-impute air_temp in tao by year domain
tao_imp <- hotdeck(tao, variable = "air_temp", domain_var = "year")

# Draw a margin plot of air_temp vs sea_surface_temp
tao_imp %>% 
    select(air_temp, sea_surface_temp, air_temp_imp) %>% 
    marginplot(delimiter = "imp")



## Hot-deck tricks & tips II: sorting by correlated variables

# Another trick that can boost the performance of hot-deck imputation is sorting the data by variables correlated to the one we want to impute.
# 
# For instance, in all the margin plots you have been drawing recently, you have seen that air temperature is strongly correlated with sea surface temperature, which makes a lot of sense. You can exploit this knowledge to improve your hot-deck imputation. If you first order the data by sea_surface_temp, then every imputed air_temp value will come from a donor with a similar sea_surface_temp. Let's see how this will work!


# 
# Hot-deck-impute air_temp in tao ordering by sea_surface_temp
tao_imp <- hotdeck(tao, variable = "air_temp", ord_var = "sea_surface_temp")

# Draw a margin plot of air_temp vs sea_surface_temp
tao_imp %>% 
    select(air_temp, sea_surface_temp, air_temp_imp) %>% 
    marginplot(delimiter = "imp")




#############################################
######## KNN imputation
## k-nearest neighbors imputation
## replace the missing values with aggregated values from k donors


## distance measures
## numerical: euclidean
## factors: manhattan distance
## categorical: hamming distance

## Gower distance to combine them

## kNN imputation in practice

kNN()

## 
# weighting donors: put higher weight on the points that are closer
# sorting variables by the number of missing values before running kNN
# kNN algorithm loops over over variables, imputing them one by one
# vars_by_NAs

# 
# Impute humidity using 30 neighbors
tao_imp <- kNN(tao, k = 30, variable = "humidity")

# Draw a margin plot of sea_surface_temp vs humidity
tao_imp %>% 
    select(sea_surface_temp, humidity, humidity_imp) %>% 
    marginplot(delimiter = "imp", main = "k = 30")



# 
# Impute humidity using 15 neighbors
tao_imp <- kNN(tao, k = 15, variable = "humidity")

# Draw a margin plot of sea_surface_temp vs humidity
tao_imp %>% 
    select(sea_surface_temp, humidity, humidity_imp) %>% 
    marginplot(delimiter = "imp", main = "k = 15")


# 
# Impute humidity using 5 neighbors
tao_imp <- kNN(tao, k = 5, variable = "humidity")

# Draw a margin plot of sea_surface_temp vs humidity
tao_imp %>% 
    select(sea_surface_temp, humidity, humidity_imp) %>% 
    marginplot(delimiter = "imp", main = "k = 5")



# kNN tricks & tips I: weighting donors
# 
# A variation of kNN imputation that is frequently applied uses the so-called distance-weighted aggregation. What this means is that when we aggregate the values from the neighbors to obtain a replacement for a missing value, we do so using the weighted mean and the weights are inverted distances from each neighbor. As a result, closer neighbors have more impact on the imputed value.
# 
# In this exercise, you will apply the distance-weighted aggregation while imputing the tao data. This will only require passing two additional arguments to the kNN() function. Let's try it out!


# Load the VIM package
library(VIM)

# Impute humidity with kNN using distance-weighted mean
tao_imp <- kNN(tao, 
               k = 5, 
               variable = "humidity", 
               numFun = weighted.mean,
               weightDist = TRUE)

tao_imp %>% 
    select(sea_surface_temp, humidity, humidity_imp) %>% 
    marginplot(delimiter = "imp")


# 
# kNN tricks & tips II: sorting variables
# 
# As the k-Nearest Neighbors algorithm loops over the variables in the data to impute them, it computes distances between observations using other variables, some of which have already been imputed in the previous steps. This means that if the variables located earlier in the data have a lot of missing values, then the subsequent distance calculation is based on a lot of imputed values. This introduces noise to the distance calculation.
# 
# For this reason, it is a good practice to sort the variables increasingly by the number of missing values before performing kNN imputation. This way, each distance calculation is based on as much observed data and as little imputed data as possible.
# 
# Let's try this out on the tao data!

# Get tao variable names sorted by number of NAs
vars_by_NAs <- tao %>%
    is.na() %>%
    colSums() %>%
    sort(decreasing = FALSE) %>% 
    names()

# Sort tao variables and feed it to kNN imputation
tao_imp <- tao %>% 
    select(vars_by_NAs) %>% 
    kNN()

tao_imp %>% 
    select(sea_surface_temp, humidity, humidity_imp) %>% 
    marginplot(delimiter = "imp")