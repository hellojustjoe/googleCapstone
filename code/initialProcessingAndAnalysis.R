library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(plotly)
library(corrplot)
library(GGally)

getwd()
#setwd("code/HelloJustJoe/googleCapstoneProject")
# getwd()


#cleaning

##importing data
dailyActivity_merged <- read.csv("data/fitbit_data/dailyActivity_merged.csv")
dailyCalories_merged <- read.csv("data/fitbit_data/dailyCalories_merged.csv")
dailyIntensities_merged <- read.csv("data/fitbit_data/dailyIntensities_merged.csv")
dailySteps_merged <- read.csv("data/fitbit_data/dailySteps_merged.csv")
sleepDay_merged <- read.csv("data/fitbit_data/sleepDay_merged.csv")
heartrate_seconds_merged <- read.csv("data/fitbit_data/heartrate_seconds_merged.csv")
hourlyCalories_merged <- read.csv("data/fitbit_data/hourlyCalories_merged.csv") # Note: This was already mentioned, ensure not to duplicate import if already done
hourlyIntensities_merged <- read.csv("data/fitbit_data/hourlyIntensities_merged.csv")
hourlySteps_merged <- read.csv("data/fitbit_data/hourlySteps_merged.csv")
minuteCaloriesNarrow_merged <- read.csv("data/fitbit_data/minuteCaloriesNarrow_merged.csv")
minuteCaloriesWide_merged <- read.csv("data/fitbit_data/minuteCaloriesWide_merged.csv")
minuteIntensitiesNarrow_merged <- read.csv("data/fitbit_data/minuteIntensitiesNarrow_merged.csv")
minuteIntensitiesWide_merged <- read.csv("data/fitbit_data/minuteIntensitiesWide_merged.csv")
minuteMETsNarrow_merged <- read.csv("data/fitbit_data/minuteMETsNarrow_merged.csv")
minuteSleep_merged <- read.csv("data/fitbit_data/minuteSleep_merged.csv")
minuteStepsNarrow_merged <- read.csv("data/fitbit_data/minuteStepsNarrow_merged.csv")
minuteStepsWide_merged <- read.csv("data/fitbit_data/minuteStepsWide_merged.csv")
weightLogInfo_merged <- read.csv("data/fitbit_data/weightLogInfo_merged.csv")

##initial data check
# head(dailyActivity_merged,10)
# head(dailyCalories_merged,10)
# head(dailyIntensities_merged,10)
# head(dailySteps_merged,10)
#head(sleepDay_merged,10)

list_of_datasets <- list(dailyActivity_merged, dailyCalories_merged, dailyIntensities_merged, dailySteps_merged, sleepDay_merged, heartrate_seconds_merged, hourlyCalories_merged, hourlyIntensities_merged, hourlySteps_merged, minuteCaloriesNarrow_merged, minuteCaloriesWide_merged, minuteIntensitiesNarrow_merged, minuteIntensitiesWide_merged, minuteMETsNarrow_merged, minuteSleep_merged, minuteStepsNarrow_merged, minuteStepsWide_merged, weightLogInfo_merged)

cleaned_datasets <- lapply(list_of_datasets, function(df) {
  #remove missing values
  df <- na.omit(df)
  #remove duplicates
  df <- distinct(df)
  
  
  #example for handling datasets with a known date column 'ActivityDay'
  if("ActivityDay" %in% names(df)) {
    df$ActivityDay <- as.Date(df$ActivityDay, format="%m/%d/%Y")
  }
  
  #convert ActivityDate into ActivityDay
  if("ActivityDate" %in% names(df)) {
    df$ActivityDay <- as.Date(df$ActivityDate, format="%m/%d/%Y")
    df$ActivityDate <- NULL #remove old column
  }
  
  if("SleepDateTime" %in% names(df)) {
    df$SleepDay <- as.Date(df$SleepDateTime)
    df$SleepTime <- format(df$SleepDateTime, "%H:%M:%S")
  }
  
  return(df)
})

#assigning names to the list of dataframes
names(cleaned_datasets) <- c("Daily Activity", "Daily Calories", "Daily Intensities", 
                             "Daily Steps", "Sleep Day", "Heart Rate Seconds", 
                             "Hourly Calories", "Hourly Intensities", "Hourly Steps", 
                             "Minute Calories Narrow", "Minute Calories Wide", 
                             "Minute Intensities Narrow", "Minute Intensities Wide", 
                             "Minute METs Narrow", "Minute Sleep", 
                             "Minute Steps Narrow", "Minute Steps Wide", 
                             "Weight Log Info")


#processing

##handling outliers
remove_outliers <- function(df) {
  # Identify numeric columns, exclude ID columns or any other specific columns if necessary
  numeric_cols <- sapply(df, is.numeric) & !names(df) %in% c("Id")
  
  for (col in names(df)[numeric_cols]) {
    # Calculate IQR
    Q1 <- quantile(df[[col]], 0.20)
    Q3 <- quantile(df[[col]], 0.70)
    IQR <- Q3 - Q1
    
    # Define bounds
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Filter out outliers
    df <- df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
  }
  
  return(df)
}

#remove outliers from all datasets in the list
processed_datasets <- lapply(cleaned_datasets, remove_outliers)

#keep the same names for processed datasets
names(processed_datasets) <- names(cleaned_datasets)

#check the result on one of the processed datasets
head(processed_datasets$`Daily Activity`)


#visualizing

##heatmaps
correlation_matrix <- cor(processed_datasets$`Daily Activity`[, sapply(processed_datasets$`Daily Activity`, is.numeric)])

corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

##linecharts
ggplot(processed_datasets$`Daily Activity`, aes(x = ActivityDay, y = TotalSteps)) +
  geom_line() +
  labs(title = "Daily Steps Over Time", x = "Day", y = "Total Steps") +
  theme_minimal()

##histograms
ggplot(processed_datasets$`Daily Activity`, aes(x = TotalSteps)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Total Steps", x = "Total Steps", y = "Frequency") +
  theme_minimal()

##scatter
ggplot(processed_datasets$`Daily Activity`, aes(x = TotalSteps, y = Calories)) +
  geom_point() +
  labs(title = "Total Steps vs. Calories", x = "Total Steps", y = "Calories") +
  theme_minimal()

##box
ggplot(processed_datasets$`Daily Activity`, aes(y = TotalSteps)) +
  geom_boxplot() +
  labs(title = "Boxplot of Total Steps", y = "Total Steps") +
  theme_minimal()



##merging





