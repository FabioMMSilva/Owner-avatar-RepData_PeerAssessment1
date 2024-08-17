# Libraries and Paths -----------------------------------------------------
library(dplyr)
library(ggplot2)
library(knitr)
library(rmarkdown)

# Set the file paths
filePath <- file.path("C:/Users/Gilva/OneDrive",
                      "ESTUDIOS (PAPERS)/Cursos y Certificados/Data Science",
                      "Clase 5 - Report Writing for Data Science in R/Module 2",
                      "Course Project 1/Inputs")
setwd(file.path("C:/Users/Gilva/OneDrive",
                "ESTUDIOS (PAPERS)/Cursos y Certificados/Data Science",
                "Clase 5 - Report Writing for Data Science in R/Module 2",
                "Course Project 1/RepData_PeerAssessment1"))

# Load the data
Data <- read.csv(file.path(filePath, "activity.csv"))
head(Data)

# What is mean total number of steps taken per day? -----------------------
Steps.pr.Day <- Data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))
head(Steps.pr.Day)

# Create a histogram of total steps per day
ggplot(Steps.pr.Day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Histogram of Total Steps Per Day", 
       x = "Total Steps", 
       y = "Frequency")

# Calculate the mean and median of total steps per day
Steps.Mean <- mean(Steps.pr.Day$total_steps)
Steps.Median <- median(Steps.pr.Day$total_steps)
Steps.Mean
Steps.Median

# What is the average daily activity pattern? -----------------------------
Avg.Steps.per.Interval <- Data %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))
head(Avg.Steps.per.Interval)

# Create a time series plot of the average steps per interval
ggplot(Avg.Steps.per.Interval, aes(x = interval, y = average_steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Daily Activity Pattern", 
       x = "5-minute Interval", 
       y = "Average Number of Steps")

# Find the interval with the maximum average steps
max_interval <- Avg.Steps.per.Interval[
  which.max(Avg.Steps.per.Interval$average_steps), ]
max_interval

# Imputing missing values -------------------------------------------------
Missing.Values <- sum(is.na(Data$steps))
Missing.Values

# Fill missing values with Avg.Steps.per.Interval
Complete.Data <- Data
for (i in 1:nrow(Complete.Data)) {
  if (is.na(Complete.Data$steps[i])) {
    interval_value <- Complete.Data$interval[i]
    Complete.Data$steps[i] <- Avg.Steps.per.Interval$average_steps[
      Avg.Steps.per.Interval$interval == interval_value]
  }
}
str(Data)  # Before
str(Complete.Data)  # After

# Recalculate total steps per day with imputed data
Complete.Steps.pr.Day <- Complete.Data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))
head(Complete.Steps.pr.Day)

# Create a histogram of total steps per day with imputed data
ggplot(Complete.Steps.pr.Day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "green", color = "black") +
  labs(title = "Histogram of Total Steps Per Day (Imputed Data)", 
       x = "Total Steps", 
       y = "Frequency")

# Recalculate the mean and median with imputed data
Complete.Steps.Mean <- mean(Complete.Steps.pr.Day$total_steps)
Complete.Steps.Median <- median(Complete.Steps.pr.Day$total_steps)
Complete.Steps.Mean
Complete.Steps.Median

# Are there differences in activity patterns between weekdays and weekends? 
Complete.Data$date <- as.Date(Complete.Data$date, format = "%Y-%m-%d")
Complete.Data$week.type <- ifelse(weekdays(Complete.Data$date) %in% 
                                    c("Saturday", "Sunday"), "weekend", "weekday")
Complete.Data$week.type <- as.factor(Complete.Data$week.type)
head(Complete.Data)

# Calculate average steps per interval across weekdays and weekends
Avg.pr.Week.Type.pr.Interval <- Complete.Data %>%
  group_by(interval, week.type) %>%
  summarise(average_steps = mean(steps))

# Create a panel plot for weekdays vs weekends
ggplot(Avg.pr.Week.Type.pr.Interval, aes(x = interval, y = average_steps, color = week.type)) +
  geom_line() +
  facet_wrap(~week.type, ncol = 1) +
  labs(title = "Average Steps per 5-Minute Interval (Weekday vs Weekend)", 
       x = "5-minute Interval", 
       y = "Average Number of Steps")

# Render the R Markdown document ------------------------------------------
render("PA1_template.Rmd")


