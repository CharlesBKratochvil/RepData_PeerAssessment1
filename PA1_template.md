data <- read.csv("activity.csv")
stepsTotalPerDay <- tapply(data$steps, data$date, sum)
hist(stepsTotalPerDay, breaks = 6, main = "Frequency of number of steps per day",     xlab = "Number of steps per day", ylab = "Frequency", col = "red")
