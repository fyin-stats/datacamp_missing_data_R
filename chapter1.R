# https://learn.datacamp.com/courses/handling-missing-data-with-imputations-in-r
###############
###############
###############
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
################
################
# Linear regression with incomplete data
# missing data might caused biased estimates
# 

################
# chapter 1
################
# obviously the best way to treat missing data is not to have them
################
# unfortunately
# nonresponse in surveys
# technical issues with data collecting equipment
# joining data from different sources
# stay watchful for missing data

#################
# NHANES data
#################
head()



##################
# main takeaways
##################
# missing data is sometimes ignored silently by statistical software
# as a result, it might be impossible to compare different models
# simply dropping all incomplete observations might lead to biased results
# missing data, if present, have to be addressed appropriately

# exercise 1

# Linear regression with incomplete data

# Missing data is a common problem and dealing with it appropriately is extremely important. Ignoring the missing data points or filling them incorrectly may cause the models to work in unexpected ways and cause the predictions and inferences to be biased.
# 
# In this chapter, you will be working with the biopics dataset. It contains information on a number of biographical movies, including their earnings, subject characteristics and some other variables. Some of the data points are, however, missing. The original data comes with the fivethirtyeight R package, but in this course, you will work with a slightly preprocessed version.
# 
# In this exercise, you will get to know the dataset and fit a linear regression model to explain a movie's earnings. Let's begin!

# Print first 10 observations
head(biopics, 10)

# Get the number of missing values per variable
biopics %>%
    is.na() %>% 
    colSums()

# Fit linear regression to predict earnings
model_1 <- lm(earnings ~ country + year + sub_type, 
              data = biopics)

# exercise 2

# Comparing models
# 
# Choosing the best of multiple competing models can be tricky if these models are built on incomplete data. In this exercise, you will extend the model you have built previously by adding one more explanatory variable: the race of the movie's subject. Then, you will try to compare it to the previous model.
# 
# As a reminder, this is how you have fitted the first model:
# 
# model_1 <- lm(earnings ~ country + year + sub_type, 
#               data = biopics)
# 
# Let's see if we can judge whether adding the race variable improves the model!

# # Fit linear regression to predict earnings
# model_2 <- lm(earnings ~ country + year + sub_type + sub_race, 
#               data = biopics)
# 
# # Print summaries of both models
# summary(model_1)
# summary(model_2)




# missing data mechanisms
# three categories:
# 1, missing completely at random (MCAR)
# 2, missing at random (MAR)
# 3, missing not at random (MNAR)

# definitions

# MCAR
# locations of missing values in the dataset are purely random
# they do not depend on any other data


# MAR
# locations of missing values in the dataset depend on some other, observed data


# MNAR
# locations of missing values in the dataset depend on the missing values themselves

# handling the mechanisms
# if the data are MCAR, removing them results in an information loss
# if the data are MAR or MNAR, removing them introduces bias to models built on these data
# in this case, missing values need to removed



# many imputation method assume MAR
# so it is important to detect it
# statistical testing

# 1, testing MAR assumption
# t-test for difference in means
# p-value: how likely is it to obtain the test statistic that you got, assuming the null hypothesis is true

# small p-value: reject the null

# Testing for MAR
# test if the percentage of missing values in one variable differs for different values of another variable
# is the percentage of missing values in PhysActive different for females and males
# create a dummy to indicate if PhysActive is missing
# is.na function


# exercise 3
# t-test for MAR: data preparation

# Great work on classifying the missing data mechanisms in the last exercise! Of all three, MAR is arguably the most important one to detect, as many imputation methods assume the data are MAR. This exercise will, therefore, focus on testing for MAR.
# 
# You will be working with the familiar biopics data. The goal is to test whether the number of missing values in earnings differs per subject's gender. In this exercise, you will only prepare the data for the t-test. First, you will create a dummy variable indicating missingness in earnings. Then, you will split it per gender by first filtering the data to keep one of the genders, and then pulling the dummy variable. For filtering, it might be helpful to print biopics's head() in the console and examine the gender variable.

# create a missing value indicator



# Problems with the testing approach
# Detecting missing data patterns with statistical tsets can be cumbersome
# t-tests comes with many assumptions about the data
# Inference based on p-values are prone to errors

# Visualizing missing data
# VIM package for plotting missing data
# Aggregation plot
# Spine plot
# Mosaic plot

# Aggregation plot: in which combinations data are missing; aggr(combined, numbers)
# Spine plot: percentage of missing values in one variable vs the other; spineMiss()
# mosaic plot: Mosaic plots; mosaicMiss


#### Exercise 4: aggregation plot
#### 
# Aggregation plot
# 
# The aggregation plot provides the answer to the basic question one may ask about an incomplete dataset: in which combinations of variables the data are missing, and how often? It is very useful for gaining a high-level overview of the missingness patterns. For example, it makes it immediately visible if there is some combination of variables that are often missing together, which might suggest some relation between them.
# 
# In this exercise, you will first draw the aggregation plot for the biopics data and then practice making conclusions based on it. Let's do some plotting!


#### Exercise 5: spine plot
#### Spine plot

# The aggregation plot you have drawn in the previous exercise gave you some high-level overview of the missing data. If you are interested in the interaction between specific variables, a spine plot is the way to go. It allows you to study the percentage of missing values in one variable for different values of the other, which is conceptually very similar to the t-tests you have been running in the previous lesson.
# 
# In this exercise, you will draw a spine plot to investigate the percentage of missing data in earnings for different categories of sub_race. Is there more missing data on earnings for some specific races of the movie's main character? Let's find out! The VIM package has already been loaded for you.



#### Exercise 6: Mosaic plot
# The spine plot you have created in the previous exercise allows you to study missing data patterns between two variables at a time. This idea is generalized to more variables in the form of a mosaic plot.
# 
# In this exercise, you will start by creating a dummy variable indicating whether the United States was involved in the production of each movie. To do this, you will use the grepl() function, which checks if the string passed as its first argument is present in the object passed as its second argument. Then, you will draw a mosaic plot to see if the subject's gender correlates with the amount of missing data on earnings for both US and non-US movies.
# 
# The biopics data as well as the VIM package are already loaded for you. Let's do some exploratory plotting!
#     
#     Note that a proprietydisplay_image()function has been created to return the output from the latestVIMpackage version. Make sure to expand theHTML Viewer section.


# # Prepare data for plotting and draw a mosaic plot
# biopics %>%
#     # Create a dummy variable for US-produced movies
#     mutate(is_US_movie = grepl("US", country)) %>%
#     # Draw mosaic plot
#     mosaicMiss(highlight = "earnings", 
#                plotvars = c("is_US_movie", "sub_sex"))
# 
# # Return plot from latest VIM package - expand the HTML viewer section
# display_image()