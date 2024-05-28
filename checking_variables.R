###### Checking Variables #####

# loading packages --------------------------------------------------------

install.packages("plm")
install.packages("sandwich")
install.packages("lmtest")
install.packages("performance")
install.packages("patchwork")
install.packages("e1071")

library(plm)
library(sandwich)
library(lmtest)
library(performance)
library(patchwork)
library(e1071)

# Checking data -----------------------------------------------------------

####### DEPENDENT VARIABLES ########

#### CHECKING SUMMARY, HISTOGRAM AND SKEWNESS ####

## Early pregnancy rate (from 10 to 19): DON'T NEED TRANSFORMATION

summary(municipalities$early_pregnancy_rt) 
hist(municipalities$early_pregnancy_rt, 
     main = "Histogram Early Pregnancy Rate", xlab = "Early Pregnancy Rate")
skewness(municipalities$early_pregnancy_rt)

## The distribution of this variable is slightly negatively skewed.
## However, the skewness value is very close to zero, suggesting 
## that the distribution is nearly symmetrical. 

####### INDEPENDENT VARIABLES ########

## Enrollments high school: NEEDS TRANSFORMATION

summary(municipalities$enrollments_high_school) 
hist(municipalities$enrollments_high_school, 
     main = "Histogram Enrollments high school", xlab = "Enrollments high school")
skewness(municipalities$enrollments_high_school)

## The distribution of this variable is highly positively skewed.
## the majority of municipalities have relatively low enrollments, 
## but there are a few outliers with very high enrollments. 

## Enrollments high school full time: NEEDS TRANSFORMATION

summary(municipalities$enrollments_high_school_full) 
hist(municipalities$enrollments_high_school_full, 
     main = "Histogram Enrollments high school full time", xlab = "Enrollments high school full time")
skewness(municipalities$enrollments_high_school_full)

## The distribution of this variable is highly positively skewed.
## the majority of municipalities have relatively low enrollments, 
## but there are a few outliers with very high enrollments. 

####### CONTROLS ########

## APPROVALS high school: NEEDS TRANSFORMATION

summary(municipalities$approvals_high_school) 
hist(municipalities$approvals_high_school, 
     main = "Histogram Approvals High School", xlab = "Approvals High School")
skewness(municipalities$approvals_high_school)

## the distribution of this variable is moderately negatively skewed.
## there might be a notable number of municipalities with higher approval rates 
## for high school, but the majority have lower rates.

## FAILURES high school: NEEDS TRANSFORMATION

summary(municipalities$failures_high_school) 
hist(municipalities$failures_high_school, 
     main = "Histogram Failures High School", xlab = "Failures High School")
skewness(municipalities$failures_high_school)

##  the distribution of this variable is positively skewed.
## while many municipalities have relatively low rates of high school failures, 
## there are some with notably higher rates. 

## DROPOUTS high school: DON'T NEED TRANSFORMATION

summary(municipalities$dropouts_high_school) 
hist(municipalities$dropouts_high_school, 
     main = "Histogram Dropouts High School", xlab = "Dropouts High School")
skewness(municipalities$dropouts_high_school)

## the distribution of this variable is very close to symmetrical.
## this near-zero skewness implies that the distribution of high school dropouts 
##across municipalities is fairly balanced, with no significant skew towards higher or 
## lower dropout rates.

## HDI index

summary(municipalities$HDI_index_FIRJAN) 
hist(municipalities$HDI_index_FIRJAN, 
     main = "Histogram HDI index", xlab = "Dropouts HDI index")
skewness(municipalities$HDI_index_FIRJAN)

## Too many missing values, maybe we should consider to work with a smaller sample size. 
## It is not returning skewness because of too many NA's. 

## GPD per capita: NEEDS TRANSFORMATION.

summary(municipalities$gdp_pc) 
hist(municipalities$gdp_pc, 
     main = "Histogram GDP per capita", xlab = "Dropouts GDP pc")
skewness(municipalities$gdp_pc)

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
abline(linear_enrollments_high_school, col = "red")

### ENROLLMENTS HIGH SCHOOL IN FULL TIME EDUCATION

plot(municipalities$enrollments_high_school_full, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Enrollments Full Time (High School)",
     xlab = "Enrollments in full time education (High School)",
     ylab = "Early Pregnancy Rate")

linear_enrollments_high_school_full <- lm(early_pregnancy_rt ~ enrollments_high_school_full, data = municipalities)
abline(linear_enrollments_high_school_full, col = "red")

### EARLY PREGNANCY RATE + CONTROLS ####

## Approvals in High School

plot(municipalities$approvals_high_school, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Approvals in High School",
     xlab = "Approvals in High School",
     ylab = "Early Pregnancy Rate")

linear_approvals_high_school <- lm(early_pregnancy_rt ~ approvals_high_school, data = municipalities)
abline(linear_approvals_high_school, col = "red")

## This looks pretty random without any tendency. 

## Failures in High School

plot(municipalities$failures_high_school, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Failures in High School",
     xlab = "Failures in High School",
     ylab = "Early Pregnancy Rate")

linear_failures_high_school <- lm(early_pregnancy_rt ~ failures_high_school, data = municipalities)
abline(linear_failures_high_school, col = "red")

## Positive tendency? If failures go up, then early pregnancy rate tends to also go up.

## Dropouts in High School

plot(municipalities$dropouts_high_school, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for Dropouts in High School",
     xlab = "Dropouts in High School",
     ylab = "Early Pregnancy Rate")

linear_dropouts_high_school <- lm(early_pregnancy_rt ~ dropouts_high_school, data = municipalities)
abline(linear_dropouts_high_school, col = "red")

## Negative tendency? If dropouts go up, then early pregnancy rate tends to go down. WEIRD.

## HDI 

plot(municipalities$HDI_index_FIRJAN, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for HDI FIRJAN",
     xlab = "HDI FIRJAN",
     ylab = "Early Pregnancy Rate")

linear_HDI <- lm(early_pregnancy_rt ~ HDI_index_FIRJAN, data = municipalities)
abline(linear_HDI, col = "red")

## Negative tendency? If HDI go up, then early pregnancy rate tends to go down.

## GDP per capita

plot(municipalities$gdp_pc, municipalities$early_pregnancy_rt, 
     main = "Scatter Plot with Linear Fit for GDP",
     xlab = "GDP per capita",
     ylab = "Early Pregnancy Rate")

linear_gpd <- lm(early_pregnancy_rt ~ gdp_pc, data = municipalities)
abline(linear_gpd, col = "red")

## Negative tendency? If GDP go up, then early pregnancy rate tends to go down.

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
