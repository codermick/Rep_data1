---
title: "R Markdown Peer Assessment"
output: html_document
---


###Loading and preprocessing the data

Show any code that is needed to load and process data
Load the data (i.e. read.csv())

```{r}
        data<-read.csv("activity.csv")
        str(data)
```
**Process/transform the data (if necessary) into a format suitable for your analysis**

```{r}
       data$date<-as.Date(data$date, format="%Y-%m-%d")
        str(data)
```
        
###What is mean total number of steps taken per day?

**Re-org data:**

```{r}
       # Vector - total number of steps:
total<-tapply(data$steps, data$date, function(x)sum(x))
head(total)
totalsteps<-array(NA, c(length(total),1))
for(i in 1:length(total)){
    totalsteps[i]<-total[[i]]
}
head(totalsteps)

# Vector - days:
date_steps<-c(unique(data$date))
head(date_steps)

# Double-check type = Date:
str(date_steps)
# ok!

# Create new data frame:

totalsteps_frame<-data.frame(date_steps,totalsteps)
str(totalsteps_frame)
```
**Using ggplot2 make a histogram:**

```{r}
        library("ggplot2")
        ggplot(aes(x=totalsteps), data=totalsteps_frame)+
        geom_histogram(color="black", fill="white") + ylab("Frequency") + xlab("Total number of steps per day")
```


###Calculate and report the mean and median total number of steps taken per day:

```{r}
        mean(totalsteps_frame$totalsteps, na.rm=T)
        median(totalsteps_frame$totalsteps, na.rm=T)
```


###What is the average daily activity pattern?

###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken averaged across all days (y-axis)

```{r}

# Get average per interval:
        averages<-tapply(data$steps, data$interval, function(x)mean(x, na.rm=T))
        meansteps_interval<-array(NA, c(length(averages),1))
        for(i in 1:length(averages)){
            meansteps_interval[i]<-averages[[i]]
        }
        head(meansteps_interval)
 
# Vector - interval:
        interval<-unique(data$interval)
        str(interval)

# Create new dataset:
        interval_frame<-data.frame(meansteps_interval, interval)
        str(interval_frame)
```
**Plot the time series:**

```{r}
        ggplot(aes(x=interval, y=meansteps_interval), data=interval_frame)+
        geom_line() + xlab("5 min interval")+ylab("Average number of steps")
```


###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
        (d<-interval_frame$interval[max(interval_frame$meansteps_interval)])    
```


###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
missing<-which(is.na(data$steps)==T)
length(missing)

```
###Number of missing N/a's: 


**Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

```{r}
# New vector:
steps2<-data$steps
steps2[is.na(steps2)==T]<-mean(data$steps, na.rm=T)
str(steps2)

```

**Create a new dataset that is equal to the original dataset but with the missing data filled in.**

```{r}
        data2<-data
        data2$steps<-steps2
        str(data2)
```

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
# Vector - total number of steps:
        total2<-tapply(data2$steps, data2$date, function(x)sum(x))
        totalsteps2<-array(NA, c(length(total2),1))
        for(i in 1:length(total2)){
            totalsteps2[i]<-total2[[i]]
        }
        head(totalsteps2)

# Vector - days = date_steps from section B
# Create new dataset
        totalsteps_frame2<-data.frame(totalsteps2, date_steps)
        str(totalsteps_frame2)
```

**Histogram:**
```{r}
ggplot(aes(x=totalsteps2), data=totalsteps_frame2)+
    geom_histogram(color="black", fill="white") + ylab("Frequency") + xlab("Total number of steps per day, no NAs")
```


**Mean and Median :**

```{r}
mean(totalsteps_frame2$totalsteps2)

median(totalsteps_frame2$totalsteps2)
```

###Are there differences in activity patterns between weekdays and weekends?
**New Factor Variables:**
```{r}
# Find days of the week:
        data2$whatdayistoday<-weekdays(data2$date)
        str(data2)
        
# Create variable:
        data2$day<-data2$whatdayistoday
        data2$day[data2$day=="Saturday" | data2$day=="Sunday"]<-"weekend"
        data2$day[data2$day != "weekend"]<-"weekday"
        str(data2)
        
# ok, but change to factors:
        data2$day<-as.factor(data2$day)
```
**Time Series Plot:**
```{r}
# Vector - mean weekdays:

        wds<-tapply(data2$steps[data2$day=="weekday"], data2$interval[data2$day=="weekday"], function(x)mean(x))
        mean_weekdays<-array(NA, c(length(wds),1))
        for(i in 1:length(wds)){
            mean_weekdays[i]<-wds[[i]]
}
head(mean_weekdays)

# Vector - mean weekends:

        wks<-tapply(data2$steps[data2$day=="weekend"], data2$interval[data2$day=="weekend"], function(x)mean(x))
        mean_weekends<-array(NA, c(length(wks),1))
        for(i in 1:length(wks)){
            mean_weekends[i]<-wks[[i]]
}
head(mean_weekends)

# Vector - interval from before = interval
# Dataset

        days_interval<-data.frame(interval, mean_weekdays, mean_weekends)
        str(days_interval)
```
**For multiple plots in a single window with ggplot,  use function multiplot (available from cookbook-r).**

```{r}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
 # Make the panel
 # ncol: Number of columns of plots
 # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
 # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

 # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```
**Time series for weekdays (top) and weekends (bottom):**

```{r}
        plot1<-ggplot(aes(x=interval, y=scale(mean_weekdays)), data=days_interval)+
            geom_line() + xlab("5 min interval") + ylab("Mean number of steps on weekdays") + theme_bw()
        
        plot2<-ggplot(aes(x=interval, y=scale(mean_weekends)), data=days_interval)+
            geom_line() + xlab("5 min interval") + ylab("Mean number of steps on weekends") + theme_bw()
        multiplot(plot1, plot2)
```
