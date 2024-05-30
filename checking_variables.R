###### Checking Variables #####

# loading packages --------------------------------------------------------

install.packages("plm")
install.packages("sandwich")
install.packages("lmtest")
install.packages("performance")
install.packages("patchwork")
install.packages("e1071")
install.packages("e1071")

library(plm)
library(sandwich)
library(lmtest)
library(performance)
library(patchwork)
library(e1071)
library(tidyverse)


# Checking data -----------------------------------------------------------

####### DEPENDENT VARIABLE ########

#### CHECKING SUMMARY, HISTOGRAM AND SKEWNESS ####

## Early pregnancy rate (from 10 to 19)

summary(municipalities$early_pregnancy_rt) 
hist(municipalities$early_pregnancy_rt, 
     main = "Histogram Early Pregnancy Rate", xlab = "Early Pregnancy Rate")
skewness(municipalities$early_pregnancy_rt)
shapiro.test(municipalities$early_pregnancy_rt)
kurtosis_early_pregnancy <- kurtosis(municipalities$early_pregnancy_rt)
print(kurtosis_early_pregnancy)
var(municipalities$early_pregnancy_rt)

## My data is not normally distributed.
## The distribution of this variable is slightly negatively skewed.
## However, the skewness value is very close to zero, suggesting 
## that the distribution is nearly symmetrical.
## The left tail of the distribution is longer. 
## Kurtosis > 3: Indicates that the distribution is more peaked than 
## normal (with sharper peaks and longer tails).

####### INDEPENDENT VARIABLES ########

## Enrollments high school

summary(municipalities$enrollments_high_school) 
hist(municipalities$enrollments_high_school, 
     main = "Histogram Enrollments high school", xlab = "Enrollments high school")
skewness(municipalities$enrollments_high_school)
shapiro.test(municipalities$enrollments_high_school)
kurtosis_enrollments_high_school <- kurtosis(municipalities$enrollments_high_school)
print(kurtosis_enrollments_high_school)

## The distribution of this variable is highly positively skewed. Kurtosis is way
## greater then 3. 
## the majority of municipalities have relatively low enrollments, 
## but there are a few outliers with very high enrollments. 

## Enrollments high school full time

summary(municipalities$enrollments_high_school_full) 
hist(municipalities$enrollments_high_school_full, 
     main = "Histogram Enrollments high school full time", xlab = "Enrollments high school full time")
skewness(municipalities$enrollments_high_school_full)
shapiro.test(municipalities$enrollments_high_school_full)
kurtosis_enrollments_high_school_full <- kurtosis(municipalities$enrollments_high_school_full)
print(kurtosis_enrollments_high_school_full)

## The distribution of this variable is highly positively skewed.
## the majority of municipalities have relatively low enrollments, 
## but there are a few outliers with very high enrollments. 

####### CONTROLS ########

## APPROVALS high school: NEEDS TRANSFORMATION

summary(municipalities$approvals_high_school) 
hist(municipalities$approvals_high_school, 
     main = "Histogram Approvals High School", xlab = "Approvals High School")
skewness(municipalities$approvals_high_school)
shapiro.test(municipalities$approvals_high_school)
kurtosis_approvals <- kurtosis(municipalities$approvals_high_school)
print(kurtosis_approvals)

## this variable is not normally distributed. 
## the distribution of this variable is moderately negatively skewed.
## the distribution has kurtosis > 3: higher peaks and longer, thinner tails compared to the normal distribution. 
## This means there are more data points clustered around the mean and in the tails.

### TRANFORMATION ###

municipalities$approvals_high_school_log <- log(municipalities$approvals_high_school + 1)

## FAILURES high school: NEEDS TRANSFORMATION

summary(municipalities$failures_high_school) 
hist(municipalities$failures_high_school, 
     main = "Histogram Failures High School", xlab = "Failures High School")
skewness(municipalities$failures_high_school)
shapiro.test(municipalities$failures_high_school)
kurtosis_failures <- kurtosis(municipalities$failures_high_school)
print(kurtosis_failures)

## this variable is not normally distributed. 
## the distribution of this variable is moderately to highly positively skewed.
## while many municipalities have relatively low rates of high school failures, 
## there are some with notably higher rates.
## the distribution has kurtosis way grater than 3: the data has a higher likelihood of producing extreme values 
## or outliers. Extreme deviations from the mean. 

### TRANFORMATION ###

municipalities$failures_high_school_log <- log(municipalities$failures_high_school + 1)

## DROPOUTS high school: DON'T NEED TRANSFORMATION

summary(municipalities$dropouts_high_school) 
hist(municipalities$dropouts_high_school, 
     main = "Histogram Dropouts High School", xlab = "Dropouts High School")
skewness(municipalities$dropouts_high_school)
shapiro.test(municipalities$dropouts_high_school)
kurtosis_dropouts <- kurtosis(municipalities$dropouts_high_school)
print(kurtosis_dropouts)

## this variable is not normally distributed. 
## the distribution of this variable is very close to symmetrical.
## this near-zero skewness implies that the distribution of high school dropouts 
## across municipalities is fairly balanced, with no significant skew towards higher or 
## lower dropout rates.

## HDI index
## Too many missing values, maybe we should consider to work with a smaller sample size. 

municipalities_filtered <- municipalities %>%
  filter(year >= 2010 & year <= 2016)
municipalities_filtered <- municipalities_filtered %>%
  filter(!is.na(HDI_index_FIRJAN))

summary(municipalities_filtered$HDI_index_FIRJAN) 
hist(municipalities_filtered$HDI_index_FIRJAN, 
     main = "Histogram HDI index", xlab = "Dropouts HDI index")
skewness(municipalities_filtered$HDI_index_FIRJAN)
shapiro.test(municipalities_filtered$HDI_index_FIRJAN)
kurtosis_HDI_index_FIRJAN <- kurtosis(municipalities_filtered$HDI_index_FIRJAN)
print(kurtosis_HDI_index_FIRJAN)

## GPD per capita: NEEDS TRANSFORMATION.

summary(municipalities$gdp_pc) 
hist(municipalities$gdp_pc, 
     main = "Histogram GDP per capita", xlab = "Dropouts GDP pc")
skewness(municipalities$gdp_pc)
shapiro.test(municipalities$gdp_pc)
kurtosis_gdp_pc <- kurtosis(municipalities$gdp_pc)
print(kurtosis_gdp_pc)

## the distribution of this variable is highly positively skewed.
## extreme GDP per capita values in some municipalities are pulling the mean upwards.


####### CHECKING OUTLIERS ########

## DEPENDENT VARIABLE ##

boxplot(municipalities$early_pregnancy_rt, main = "Boxplot for early pregnancy")

## I have outliers. 

## INDEPENDENT VARIABLE ##

boxplot(municipalities$enrollments_high_school, main = "Boxplot enrollments high school")
boxplot(municipalities$enrollments_high_school_full, main = "Boxplot enrollments high school full time")

## Too many outliers. 

## CONTROLS ##

## Approvals ##

boxplot(municipalities$approvals_high_school, main = "Boxplot Approvals for High School")

## I have outliers.

## Failures ##

boxplot(municipalities$failures_high_school, main = "Boxplot Failures in High School")

## Too many outliers. 

## Dropouts ##

boxplot(municipalities$dropouts_high_school, main = "Boxplot dropouts in High School")

## I have outliers. 

## HDI index ##

boxplot(municipalities$HDI_index_FIRJAN, main = "Boxplot HDI")

## Too many outliers.

## GPD per capita

boxplot(municipalities$gdp_pc, main = "Boxplot GDP per capita")

## Too many outliers.

#### SCATTER PLOTS ####

### EARLY PREGNANCY RATE + INDEPENDENT VARIABLES ####

### ENROLLMENTS HIGH SCHOOL IN CONVENTIONAL EDUCATION

plot(municipalities$enrollments_high_school, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Enrollments (High School)",
     xlab = "Enrollments (High School)",
     ylab = "Early Pregnancy Rate")

linear_enrollments_high_school <- lm(early_pregnancy_rt ~ enrollments_high_school, data = municipalities)
summary(linear_enrollments_high_school)
abline(linear_enrollments_high_school, col = "red")

## Pearson coefficient 

correlation_1 <- cor(municipalities$enrollments_high_school, municipalities$early_pregnancy_rt, method = "pearson")
print(correlation_1)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

test_correlation_1 <- cor.test(municipalities$enrollments_high_school, municipalities$early_pregnancy_rt, method = "pearson")
print(test_correlation_1)

## We rejected the null hypothesis. There is evidence of linear correlation between variables. 

### ENROLLMENTS HIGH SCHOOL IN FULL TIME EDUCATION

plot(municipalities$enrollments_high_school_full, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Enrollments Full Time (High School)",
     xlab = "Enrollments in full time education (High School)",
     ylab = "Early Pregnancy Rate")

linear_enrollments_high_school_full <- lm(early_pregnancy_rt ~ enrollments_high_school_full, data = municipalities)
summary(linear_enrollments_high_school_full)
abline(linear_enrollments_high_school_full, col = "red")

## Pearson coefficient 

correlation_2 <- cor(municipalities$enrollments_high_school_full, municipalities$early_pregnancy_rt, method = "pearson")
print(correlation_2)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

test_correlation_2 <- cor.test(municipalities$enrollments_high_school_full, municipalities$early_pregnancy_rt, method = "pearson")
print(test_correlation_2)

## We rejected the null hypothesis. There is evidence of linear correlation between variables. 

### EARLY PREGNANCY RATE + CONTROLS ####

## Approvals in High School

plot(municipalities$approvals_high_school, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Approvals in High School",
     xlab = "Approvals in High School",
     ylab = "Early Pregnancy Rate")

linear_approvals_high_school <- lm(early_pregnancy_rt ~ approvals_high_school, data = municipalities)
summary(linear_approvals_high_school)
abline(linear_approvals_high_school, col = "red")

## This looks pretty random without any tendency. 

## Pearson coefficient 

correlation_3 <- cor(municipalities$approvals_high_school, municipalities$early_pregnancy_rt, method = "pearson")
print(correlation_3)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

test_correlation_3 <- cor.test(municipalities$approvals_high_school, municipalities$early_pregnancy_rt, method = "pearson")
print(test_correlation_3)

## We cannot reject the null hypothesis. There is evidence of no linear correlation between variables. 
## actually, Pearson's coefficient is almost 0, indicating no relationship at all. 

## Failures in High School

plot(municipalities$failures_high_school, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Failures in High School",
     xlab = "Failures in High School",
     ylab = "Early Pregnancy Rate")

linear_failures_high_school <- lm(early_pregnancy_rt ~ failures_high_school, data = municipalities)
summary(linear_failures_high_school)
abline(linear_failures_high_school, col = "red")

## Pearson coefficient 

correlation_4 <- cor(municipalities$failures_high_school, municipalities$early_pregnancy_rt, method = "pearson")
print(correlation_4)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

test_correlation_4 <- cor.test(municipalities$failures_high_school, municipalities$early_pregnancy_rt, method = "pearson")
print(test_correlation_4)

## We cannot reject the null hypothesis. There is evidence of no linear correlation between variables. 
## Pearson's coefficient is 0.07. 
## P-value is 0.062. 
## Positive tendency? If failures go up, then early pregnancy rate tends to also go up.

##Log of failures in High School

plot(municipalities$failures_high_school_log, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Log of Failures in High School",
     xlab = "Log of Failures in High School",
     ylab = "Early Pregnancy Rate")

linear_failures_high_school_log <- lm(early_pregnancy_rt ~ failures_high_school_log, data = municipalities)
summary(linear_failures_high_school_log)
abline(linear_failures_high_school_log, col = "red")

## Once transforming the variable, the relationship became negative and not significant. 
## Makes no sense to transform. 

## Pearson coefficient 

correlation_5 <- cor(municipalities$failures_high_school_log, municipalities$early_pregnancy_rt, method = "pearson")
print(correlation_5)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

test_correlation_5 <- cor.test(municipalities$failures_high_school_log, municipalities$early_pregnancy_rt, method = "pearson")
print(test_correlation_5)

## We cannot reject the null hypothesis. There is evidence of no linear correlation between variables. 
## Pearson's coefficient is -0.03. 
## P-value is 0.3814. 
## Negative tendency? If failures go up, then early pregnancy rate tends to go down. Weird. 

## Better use the failures variable without transformations, but the relationship is not linear. 

## Dropouts in High School

plot(municipalities$dropouts_high_school, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Dropouts in High School",
     xlab = "Dropouts in High School",
     ylab = "Early Pregnancy Rate")

linear_dropouts_high_school <- lm(early_pregnancy_rt ~ dropouts_high_school, data = municipalities)
summary(linear_dropouts_high_school)
abline(linear_dropouts_high_school, col = "red")

## Negative tendency? If dropouts go up, then early pregnancy rate tends to go down. WEIRD.

## Pearson coefficient 

correlation_6 <- cor(municipalities$dropouts_high_school, municipalities$early_pregnancy_rt, method = "pearson")
print(correlation_6)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

test_correlation_6 <- cor.test(municipalities$dropouts_high_school, municipalities$early_pregnancy_rt, method = "pearson")
print(test_correlation_6)

## We cannot reject the null hypothesis. There is evidence of no linear correlation between variables. 
## Pearson's coefficient is -0.069. 
## P-value is 0.08. 
## Negative tendency? If dropouts go up, then early pregnancy rate tends to also go down. Weird. 

## Dropout is a weird variable. 

## HDI

plot(municipalities_filtered$HDI_index_FIRJAN, municipalities_filtered$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for HDI FIRJAN",
     xlab = "HDI FIRJAN",
     ylab = "Early Pregnancy Rate")

linear_HDI <- lm(early_pregnancy_rt ~ HDI_index_FIRJAN, data = municipalities_filtered)
summary(linear_HDI)
abline(linear_HDI, col = "red")

## Negative tendency? If HDI go up, then early pregnancy rate tends to go down.

## Pearson coefficient 

correlation_7 <- cor(municipalities_filtered$HDI_index_FIRJAN, municipalities_filtered$early_pregnancy_rt, method = "pearson")
print(correlation_7)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

test_correlation_7 <- cor.test(municipalities_filtered$HDI_index_FIRJAN, municipalities_filtered$early_pregnancy_rt, method = "pearson")
print(test_correlation_7)

## We rejected the null hypothesis. There is evidence of linear correlation between variables. 

## GDP per capita

plot(municipalities$gdp_pc, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for GDP",
     xlab = "GDP per capita",
     ylab = "Early Pregnancy Rate")

linear_gpd_pc <- lm(early_pregnancy_rt ~ gdp_pc, data = municipalities)
summary(linear_gpd_pc)
abline(linear_gpd_pc, col = "red")

## Negative tendency? If GDP go up, then early pregnancy rate tends to go down.

## Pearson coefficient 

correlation_8 <- cor(municipalities$gdp_pc, municipalities$early_pregnancy_rt, method = "pearson")
print(correlation_8)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

test_correlation_8 <- cor.test(municipalities$gdp_pc, municipalities$early_pregnancy_rt, method = "pearson")
print(test_correlation_8)

## We rejected the null hypothesis. There is evidence of linear correlation between variables. 

# COUNTING OUTLIERS  -------------------------------------

# Function to count outliers using the IQR rule

## Reference: Tukey, J. W. (1977). Exploratory Data Analysis. Addison-Wesley.

## counting outliers

count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
}

# Apply the function to each numeric column of the dataset

outlier_counts <- sapply(municipalities, function(x) {
  if (is.numeric(x)) {
    count_outliers(x)
  } else {
    NA
  }
})

# Remove NA values from outlier counts

outlier_counts <- outlier_counts[!is.na(outlier_counts)]

# Calculate the total number of outliers

total_outliers <- sum(outlier_counts)

# Calculate the total number of data points analyzed (number of numeric values in the dataset)

total_data_points <- sum(sapply(municipalities, function(x) {
  if (is.numeric(x)) {
    length(x)
  } else {
    0
  }
}))

# Calculate the proportion of outliers

proportion_outliers <- total_outliers / total_data_points
print(paste("Proportion of outliers:", round(proportion_outliers, 4)))
print(outlier_counts)


### Approximately 3.92% of my dataset are outliers according to the 
### interquartile range (IQR) rule.
