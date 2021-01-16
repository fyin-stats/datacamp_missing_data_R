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
               "ggplot2", "devtools", "stringr", "stringi", "NHANES", "VIM", "simputation", "missForest")
ipak(packages)


#############################
#############################
# multiple imputation by bootstrapping
# imputed values can have uncertainty
# 