---
output:
  word_document: default
  pdf_document: default
  html_document: default
---
```{r}
data <- read.csv("activity.csv")
stepsTotalPerDay <- tapply(data$steps, data$date, sum)
hist(stepsTotalPerDay, breaks = 6, main = "Frequency of number of steps per day", 
    xlab = "Number of steps per day", ylab = "Frequency", col = "red")
```
![alt tag](https://github.com/CharlesBKratochvil/RepData_PeerAssessment1/blob/master/instructions_fig/1.png)

```{r}
stepsMeanPerDay <- tapply(data$steps, data$date, mean, na.rm = T)
median(stepsTotalPerDay, na.rm = T)
```
![alt tag](https://github.com/CharlesBKratochvil/RepData_PeerAssessment1/blob/master/instructions_fig/2.png)
```{r}
stepsMeanPerInterval <- tapply(data$steps, data$interval, mean, na.rm = T)
plot(stepsMeanPerInterval, type = "l", main = ("Steps vs. Interval (daily average)"), 
    ylab = "# of steps")
seq(along = stepsMeanPerInterval)[stepsMeanPerInterval == max(stepsMeanPerInterval)]

tmp_stepsMeanPerInterval <- as.vector(stepsMeanPerInterval)

tmp_stepsMeanPerInterval <- rep(tmp_stepsMeanPerInterval, 61)

tmp_stepsMeanPerInterval[!is.na(data$steps)] = 1


tmp_dataTest <- as.vector(data$steps)

tmp_dataTest[is.na(tmp_dataTest)] = 1

data_NoMissing <- data
data_NoMissing$steps <- tmp_stepsMeanPerInterval * tmp_dataTest



stepsTotalPerDay_NoMissing <- tapply(data_NoMissing$steps, data_NoMissing$date, 
    sum)
hist(stepsTotalPerDay_NoMissing, breaks = 6, main = "Frequency of number of steps per day", 
    xlab = "Number of steps per day", ylab = "Frequency", col = "red")
```
![alt tag](https://github.com/CharlesBKratochvil/RepData_PeerAssessment1/blob/master/instructions_fig/3.png)
```{r}
stepsMeanPerInterval_NoMissing <- tapply(data_NoMissing$steps, data_NoMissing$interval, 
    mean)
mean(stepsTotalPerDay_NoMissing)
median(stepsTotalPerDay_NoMissing)
plot(stepsMeanPerInterval_NoMissing, type = "l", xlab = "Interval", ylab = "# of Steps", 
    main = "Steps vs. Interval (missing replaced with mean)")
```
![alt tag]((https://github.com/CharlesBKratochvil/RepData_PeerAssessment1/blob/master/instructions_fig/4.png)
```{r}

tmpLT <- as.POSIXlt(data$date, format = "%Y-%m-%d")
tmpWeekDays <- tmpLT$wday
tmpWeekDays[tmpWeekDays == 0] = 0
tmpWeekDays[tmpWeekDays == 6] = 0
tmpWeekDays[tmpWeekDays != 0] = 1
tmpWeekDaysFactor <- factor(tmpWeekDays, levels = c(0, 1))

data$WD <- tmpWeekDaysFactor

stepsMeanPerWeekday <- tapply(data$steps, list(data$interval, data$WD), mean, 
    na.rm = T)

par(mfrow = c(2, 1))

with(data, {
    par(mai = c(0, 1, 1, 0))
    plot(stepsMeanPerWeekday[, 1], type = "l", main = ("Steps vs. Interval"), 
        xaxt = "n", ylab = "Week ends")
    title = ("# of Steps v.s. Interval")
    par(mai = c(1, 1, 0, 0))
    plot(stepsMeanPerWeekday[, 2], type = "l", xlab = "Interval", ylab = "Week days")

})
```
![alt tag](https://github.com/CharlesBKratochvil/RepData_PeerAssessment1/blob/master/instructions_fig/5.png)
