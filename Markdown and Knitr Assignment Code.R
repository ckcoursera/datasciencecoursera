#CK coursera assignment code
#This is the peer assignment from Course 4, Week 2 of the coursera courses
#In this course we learned about markdown and knitr and will apply those skills here
#The data is about personal movement as recorded by an activity monitoring device.
#The assignment makes use of this dta. The device collects data at 5 min intervals daily.
#The data consists of two months (Oct-Nov 2012) of information from an anomymous individual

setwd("C:/Coursera/Markdown and Knitr/Peer assignment/RepData_PeerAssessment1-master") 


#Load the packages necessary
library(ggplot2)
library(lubridate)
library(dplyr)
library(Hmisc)
library(lattice)

#####################################STEP 1########################################
#Load the data
activity <- read.csv('./activity.csv', header=TRUE, sep=',')
head(activity) #We will need to ignore the NAs. Create sep dataset where both day and step are present
names(activity)
str(activity) #Note that date variable is called in as a factor


#Here is where we create the separate dataset where both day and step are present to ignore NAs
both <- complete.cases(activity$date, activity$steps) #complete cases of date and steps
both #Returns logical vectors of True where both date and steps are present and false otherwise

#Convert true false logical "both"data to numeric by aggregating numeric sum
stepsbyday <- aggregate(activity$steps[both], list(activity$date[both]), sum) #sum steps for each date
stepsbyday
names(stepsbyday) <- c("date", "totalsteps") #date is a factor variable, needs to be date variable 
stepsbyday
str(stepsbyday) #Dealt with the NAs but still have to deal with making date a date format

#Change the date variable to a date format and creating a day variable
stepsbyday$date <- as.Date(stepsbyday$date)
str(stepsbyday)
#Soon we will be asked to figure stats in relation to certain days of the week. Convert:
stepsbyday$day <- wday(stepsbyday$date, label=TRUE, abbr=FALSE)
head(stepsbyday)

###################################STEPS 2 and 3###############################################
#SUM, MEAN AND MEDIAN OF THE TOTAL NUMBER OF STEPS TAKEN PER DAY.
stepsbyday_sum <- aggregate(totalsteps ~ day, stepsbyday, sum)
stepsbyday_sum
stepsbyday_mean <- aggregate(totalsteps ~ day, stepsbyday, mean)
stepsbyday_mean
stepsbyday_median <- aggregate(totalsteps ~ day, stepsbyday, median)
stepsbyday_median
totalmean <- mean(stepsbyday$totalsteps) 
totalmean
totalmedian <- median(stepsbyday$totalsteps) 
totalmedian
#Report all these numbers in the final code

#HISTOGRAM OF THE TOTAL NUMBER OF STEPS TAKEN EACH DAY. 
hist(stepsbyday$totalsteps, main="Total steps taken each day", xlab="Total Steps", ylab="Frequency (days)")
#nonimputed peak above 25

##################################STEPS 4 and 5###########################################
#Make a time series plot (type="l") of the 5 minute interval (xaxis) and the average number of steps taken, 
#averaged across all days (yaxis)

#Time series plot of the average number of steps taken per interval 

#Previously we created a dataset called both that took only rows where both steps and date were present
#for the next plot we also want to incorporate interval back into that dataset
both.interval <- complete.cases(activity$interval, activity$steps)
both.interval #Logical result of trues and falses where interval is present or not
#Convert that logical vector into a numeric vector using aggregate 
meanstepsperinterval <- aggregate(activity$steps[both.interval], list(activity$interval[both.interval]), mean)
head(meanstepsperinterval)
names(meanstepsperinterval) <-c("interval", "Total_Mean_Steps") #Renaming
head(meanstepsperinterval)
plot(type="l", meanstepsperinterval, xlab="5min Intervals", ylab="Mean Steps", main="Average number of steps taken each 5 min interval across all day")

#The 5 minute interval that, on average, contains the maximum number of steps
#Find the max steps
maxsteps <- max(meanstepsperinterval$Total_Mean_Steps)
maxsteps #Max steps is 206.17 but at what interval and row does this occur?
#Find the time interval of the max steps
meanstepsperinterval[meanstepsperinterval$Total_Mean_Steps==maxsteps,] #Data[data$variable==maxvariable,]
maxinterval <- meanstepsperinterval[104, "interval"] #newvariable <- dataset[row, "variable"]
maxinterval #The max interval happened in row 104 and was time interval 835. This was assigned to a variable for reporting 

###################################STEP 6 and 7############################################
#Imputing missing values 
# Calculate and report the total number of missing values in the dataset (rows with NA).
numbermissing <- length(which(is.na(activity$steps)))
numbermissing
#There are 2304 rows missing steps information 

#Devise a strategy for filling in all the missing values in the dataset. 
#Fill in the missing values in the dataset by using the median for that day

# Create a dataset that is equal to the original dataset but with the missing data filled in (imputed)
#Copy original datset
imputedcopy <- activity
imputedcopy$steps <- impute(activity$steps, fun=mean)
head(imputedcopy)

#Check the number of missings in the imputed copy 
nmissimpute <- length(which(is.na(imputedcopy$steps)))
nmissimpute
#There are now 0 rows missing steps information 

#Histogram of the total number of steps taken each day and calculate mean and median total steps/day. 
imputedcopy$date <- as.Date(imputedcopy$date)
str(imputedcopy)
imputedcopy$day <- wday(imputedcopy$date, label=TRUE, abbr=FALSE)
head(imputedcopy)
sumstepsbyday_sum <- tapply(imputedcopy$steps, imputedcopy$day, sum)
sumstepsbyday_sum
sumstepsbyday_mean <- tapply(imputedcopy$steps, imputedcopy$day, mean)
sumstepsbyday_mean
sumstepsbyday_median <- tapply(imputedcopy$steps, imputedcopy$day, median)
sumstepsbyday_median

imputedhist <- aggregate(imputedcopy$steps, list(imputedcopy$date),sum)
names(imputedhist) <- c("date", "imputedsteps")
hist(imputedhist$imputedsteps, main="Total steps taken each day", xlab="Total Steps", ylab="Frequency (days)")
#imputed peak above 35

#Do these values differ from the estimates from first part of the assignment? YES
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#Taking out the missing values caused the distribution of the overall frequencies of steps per day 
#to shift up, with a peak at frequency of >35 instead of >25 without imputing the missings 

#######################################STEP 8########################################
#Panel plot comparing the average number of steps taken per 5 minute interval across weekdays and weekends
imputedcopy$weektype[imputedcopy$day=='Monday']<-'Week'
imputedcopy$weektype[imputedcopy$day=='Tuesday']<-'Week'
imputedcopy$weektype[imputedcopy$day=='Wednesday']<-'Week'
imputedcopy$weektype[imputedcopy$day=='Thursday']<-'Week'
imputedcopy$weektype[imputedcopy$day=='Friday']<-'Week'
imputedcopy$weektype[imputedcopy$day=='Saturday']<-'Weekend'
imputedcopy$weektype[imputedcopy$day=='Sunday']<-'Weekend'
unique(imputedcopy$weektype)

par(mfrow=c(2,1))
imputedstepsmeanbyweekbyinterval <- aggregate(imputedcopy$steps, list(imputedcopy$interval, imputedcopy$weektype), mean)
imputedstepsmeanbyweekbyinterval
names(imputedstepsmeanbyweekbyinterval) <- c("interval", "weektype", "meansteps")
xyplot(meansteps~interval | weektype, data=imputedstepsmeanbyweekbyinterval, type="l", aspect=2/5,
       ylab="Average number of steps",
       main="Imputed Average Steps by Week or Weekend and interval")
#More activity happens in the morning on both weekends and weekdays, with a steeper peak in steps happening on weekday mornings
#Activity is more varied (and higher) during the weekends, but is generally lower with some moderate peaks during weekdays












