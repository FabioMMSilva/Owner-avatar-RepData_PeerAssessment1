# The variables included in this Dataset are: -----------------------------
# steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# date: The date on which the measurement was taken in YYYY-MM-DD format
# interval: Identifier for the 5-minute interval in which measurement was taken

# Assignment --------------------------------------------------------------
# This assignment will be described in multiple parts. You will need to write a 
# report that answers the questions detailed below. Ultimately, you will need to 
# complete the entire assignment in a single R markdown document that can be 
# processed by knitr and be transformed into an HTML file.

# Throughout your report make sure you always include the code that you used to 
# generate the output you present. When writing code chunks in the R markdown 
# document, always use 
# echo = TRUE
# so that someone else will be able to read the code. This assignment will be 
# evaluated via peer assessment so it is essential that your peer evaluators 
# be able to review the code for your analysis.

# For the plotting aspects of this assignment, feel free to use any plotting 
# system in R (i.e., base, lattice, ggplot2)

# Fork/clone the GitHub repository created for this assignment. You will submit 
# this assignment by pushing your completed files into your forked repository on 
# GitHub. The assignment submission will consist of the URL to your GitHub 
# repository and the SHA-1 commit ID for your repository state.

# NOTE: The GitHub repository also contains the Dataset for the assignment 
# so you do not have to download the Data separately.
# 
# Loading and preprocessing the Data
# Show any code that is needed to
# 
# Load the Data (i.e. 
#                read.csv()
#                read.csv())
# 
# Process/transform the Data (if necessary) into a format suitable for 
# your analysis
# 
# What is mean total number of steps taken per day? For this part of the 
# assignment, you can ignore the missing values in the Dataset.
# 
# Calculate the total number of steps taken per day
# 
# If you do not understand the difference between a histogram and a barplot, 
# research the difference between them. Make a histogram of the total number 
# of steps taken each day
# 
# Calculate and report the mean and median of the total number of steps 
# taken per day
# 
# What is the average daily activity pattern?
#   Make a time series plot (i.e. 
#                            type = "l"
#                            type = "l") of the 5-minute interval (x-axis) and t
# he average number of steps taken, averaged across all days (y-axis)
# 
# Which 5-minute interval, on average across all the days in the Dataset, 
# contains the maximum number of steps?
#   
#   Imputing missing values
# Note that there are a number of days/intervals where there are missing 
# values (coded as NA NA). The presence of missing days may introduce bias 
# into some calculations or summaries of the Data.
# 
# Calculate and report the total number of missing values in the Dataset 
# (i.e. the total number of rows with  NA NAs)
# 
# Devise a strategy for filling in all of the missing values in the Dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.
# 
# Create a new Dataset that is equal to the original Dataset but with the 
# missing Data filled in.
# 
# Make a histogram of the total number of steps taken each day and Calculate and 
# report the mean and median total number of steps taken per day. Do these values 
# differ from the estimates from the first part of the assignment? What is the 
# impact of imputing missing Data on the estimates of the total daily number 
# of steps?
#   
# Are there differences in activity patterns between weekdays and weekends?
# For this part the 
# weekdays()
# weekdays() function may be of some help here. Use the Dataset with the filled-in 
# missing values for this part.
# 
# Create a new factor variable in the Dataset with two levels – “weekday” and 
# “weekend” indicating whether a given date is a weekday or weekend day.
# 
# Make a panel plot containing a time series plot (i.e. 
#                                                  type = "l"
#                                                  type = "l") of the 5-minute 
# interval (x-axis) and the average number of steps taken, averaged across all 
# weekday days or weekend days (y-axis). See the README file in the GitHub 
# repository to see an example of what this plot should look like using simulated 
# Data.


# Libraries and Paths -----------------------------------------------------
library(dplyr); library(ggplot2)
filePath <- file.path(dirname(getwd()), "Inputs")
Data <- read.csv(file.path(
  filePath, "activity.csv")); head(Data)

# What is mean total number of steps taken per day? -----------------------
Steps.pr.Day <- Data %>%
  group_by(date) %>%
  summarise(
    total_steps = sum(steps, na.rm = TRUE)); head(Steps.pr.Day)

ggplot(
  Steps.pr.Day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Histogram of Total Steps Per Day", 
       x = "Total Steps", 
       y = "Frequency")

# Mean and median of total steps per day
Steps.Mean <- mean(
  Steps.pr.Day$total_steps); Steps.Mean
Steps.Median <- median(
  Steps.pr.Day$total_steps); Steps.Median

# What is the average daily activity pattern? -----------------------------
Avg.Steps.per.Interval <- Data %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE)); head(Avg.Steps.per.Interval)

ggplot(Avg.Steps.per.Interval, 
       aes(x = interval, 
           y = average_steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Daily Activity Pattern", 
       x = "5-minute Interval", 
       y = "Average Number of Steps")

# Find the interval with the maximum average steps
max_interval <- Avg.Steps.per.Interval[
  which.max(
    Avg.Steps.per.Interval$average_steps), ]; head(max_interval)

# Imputing missing values -------------------------------------------------
Missing.Values <- sum(is.na(
  Data$steps)); head(Missing.Values)

# Fill missing values with Avg.Steps.per.Interval
Complete.Data <- Data
for (i in 1:nrow(Complete.Data)) {
  if (is.na(Complete.Data$steps[i])) {
    interval_value <- Complete.Data$interval[i]
    Complete.Data$steps[i] <- Avg.Steps.per.Interval$average_steps[
      Avg.Steps.per.Interval$interval == interval_value]
  }
}
str(Data) #Before
str(Complete.Data) #After

# Recalculate total steps per day with imputed Data
Complete.Steps.pr.Day <- Complete.Data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps)); head(Complete.Steps.pr.Day)

# Histogram
ggplot(Complete.Steps.pr.Day, 
       aes(x = total_steps)) +
  geom_histogram(
    binwidth = 1000, 
    fill = "green", 
    color = "black") +
  labs(title = "Histogram of Total Steps Per Day (Imputed Data)", 
       x = "Total Steps", 
       y = "Frequency")

# Recalculate the mean and median
Complete.Steps.Mean <- mean(
  Complete.Steps.pr.Day$total_steps); Complete.Steps.Mean
Complete.Steps.Median <- median(
  Complete.Steps.pr.Day$total_steps); Complete.Steps.Median

# Are there differences in activity patterns between weekdays ... ---------
# ... and weekends? -------------------------------------------------------
Complete.Data$date <- as.Date(Complete.Data$date, format = "%Y-%m-%d")
Complete.Data$week.type <- ifelse(weekdays(Complete.Data$date) %in% 
                                  c("Saturday", "Sunday"), "weekend", "weekday")
# Convert to a factor
Complete.Data$week.type <- as.factor(Complete.Data$week.type); head(Complete.Data)

# Calculate average steps per interval across weekdays and weekends
Avg.pr.Week.Type.pr.Interval <- Complete.Data %>%
  group_by(interval, week.type) %>%
  summarise(average_steps = mean(steps))

# Panel plot
ggplot(Avg.pr.Week.Type.pr.Interval, 
       aes(
         x = interval, 
         y = average_steps,
         color = week.type)) +
  geom_line() +
  facet_wrap(~week.type, ncol = 1) +
  labs(title = "Average Steps per 5-Minute Interval (Weekday vs Weekend)", 
       x = "5-minute Interval", 
       y = "Average Number of Steps")










