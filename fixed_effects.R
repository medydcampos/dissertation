####### ANALYSING THE DATA #####

### My data is usually not normal and not linear.
### I face heteroscedasticity and outliers. 

## I am not removing outliers because I think I can lose a lot of information
## by removing it. 


# Packages ----------------------------------------------------------------

install.packages("plm")
install.packages("sandwich")
install.packages("lmtest")
install.packages("performance")
install.packages("patchwork")
install.packages("randomForest")


# Loading variables -------------------------------------------------------

library(plm)
library(sandwich)
library(lmtest)
library(performance)
library(patchwork)
library(randomForest)

# Running the model with outliers  -------------------------------------------------------

## creating dummies 

municipalities$dummy_enrollments_final_yers_full <- ifelse(municipalities$enrollments_final_yers_full > 0, 1, 0)
municipalities$dummy_enrollments_high_school_full <- ifelse(municipalities$enrollments_high_school_full > 0, 1, 0)

## Let's use the logarithm of GDP since the distribution is very skewed in the right, following Johnson & Wichern, 2019.

## Reference: Johnson, R. A., & Wichern, D. W. (2019). Applied multivariate statistical analysis. Pearson.

municipalities$log_gdp_pc <- log(municipalities$gdp_pc)

## First model

pdata_data <- pdata.frame(municipalities, index = c("municipality", "year"))

model_fe_1 <- plm(early_pregnancy_rt ~ dummy_enrollments_final_yers_full, 
                  data = pdata_data,
                  model = "within", 
                  effect = "twoways",
                  vcov = vcovHC,
                  cluster = "municipality")
summary(model_fe_1)

model_fe_2 <- plm(early_pregnancy_rt ~ dummy_enrollments_high_school_full, 
                  data = pdata_data,
                  model = "within", 
                  effect = "twoways",
                  vcov = vcovHC,
                  cluster = "municipality")

summary(model_fe_2)

## The only significant dummy is enrollments in high school. Final years of 
## primary education is not significant. From here, I am going to run the model
## only for the high school data. 

## Adding controls 

model_fe_3 <- plm(early_pregnancy_rt ~ dummy_enrollments_high_school_full + approvals_high_school +
                    failures_high_school + dropouts_high_school + log_gdp_pc + HDI_index_FIRJAN,
                  data = pdata_data,
                  model = "within", 
                  effect = "twoways",
                  vcov = vcovHC,
                  cluster = "municipality")
summary(model_fe_3)

### Once adding controls, enrollments in high school is still significant, but at 10%. 
## Failures in high school is highly significant but it is negative: higher failure rates are
## correlated with lower early pregnancy rates. Weird? Approvals are negatively correlated and significant at 5%:
## higher approvals rates are correlated to lower early pregnancy rates - which makes sense.


# Running with transformations --------------------------------------------

## Failures.

### This variable is very skewed in the right and I have outliers. 
### Since it is a rate variable, I decided to do the log version of it following Johnson & Wichern, 2019. 

municipalities$failures_high_school_log <- log(municipalities$failures_high_school + 1)


## Approvals 

### the distribution of this variable is moderately negatively skewed.
### Since it is a rate variable, I decided to do the log version of it following Johnson & Wichern, 2019. 

municipalities$approvals_high_school_log <- log(municipalities$approvals_high_school + 1)

# running with the transformations ----------------------------------------

## log of failures

pdata_data <- pdata.frame(municipalities, index = c("municipality", "year"))
model_fe_4 <- plm(early_pregnancy_rt ~ dummy_enrollments_high_school_full + approvals_high_school_log + 
                    failures_high_school_log + dropouts_high_school + log_gdp_pc + HDI_index_FIRJAN,
                  data = pdata_data,
                  model = "within", 
                  effect = "twoways",
                  vcov = vcovHC,
                  cluster = "municipality")
summary(model_fe_4)

## adjusting the failures variable, it became insignificant and the dummy is significant at 10%. 
## Dropouts and approvals lost significance. 
## My model is not valid as a whole. F-statistic is too high. 

### CONCLUSION ###

## My adjusted R-squared is negative, indicating a bad model. F-statistic is not valid. 
## However, the dummy variable is persist in being significant. 
## I have to try other methodologies. 
## Non-parametric methodologies? Adding more controls?

# Let's try with other methodology ----------------------------------------

## Random forests

## setting seeds

set.seed(123)

# Defining the formula

formula <- early_pregnancy_rt ~ dummy_enrollments_high_school_full + approvals_high_school + 
  failures_high_school_log + dropouts_high_school + log_gdp_pc + HDI_index_FIRJAN


## filtering for 2010 to 2016 due to problems with th IDH variable

municipalities_clean <- subset(municipalities, year >= 2010 & year <= 2016)

cols_to_check <- c("early_pregnancy_rt", "dummy_enrollments_high_school_full", "approvals_high_school", 
                   "failures_high_school_log", "dropouts_high_school", "log_gdp_pc", "HDI_index_FIRJAN")

municipalities_clean <- na.omit(municipalities_clean)

colSums(is.na(municipalities_clean[cols_to_check]))

## running the model

model_rf <- randomForest(formula, data = municipalities_clean, importance = TRUE, ntree = 500)
print(model_rf)

## My model is explaining only 13.13% of the variance in the early pregnancy rate.


## evaluating the importance of the variables

importance(model_rf)
varImpPlot(model_rf)

