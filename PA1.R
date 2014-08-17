## Downloading the file
if(!file.exists("./data/activity.csv")) {
    url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url,destfile="./data/archieved_data.zip", method="curl")
    unzip("./data/archieved_data.zip", exdir = "./data")
}

## Read the data
rawdata <- read.csv("./data/activity.csv")

## Importing necessary packages, download and install if necessary
if("reshape2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("reshape2")
}
library(reshape2)

if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggplot2")
}
library(ggplot2)

if("lattice" %in% rownames(installed.packages()) == FALSE) {
    install.packages("lattice")
}
library(lattice)

# Melting the data
datamelt <- melt(rawdata, id = c("date","interval"), measure.vars = c("steps"))

# Plotting histogram with mean and median lines for totalstepsperday
totalstepsperday <- dcast( datamelt, date ~ variable, sum)
colnames(totalstepsperday) <- c("Date","Total_Steps")
histogram <- ggplot (totalstepsperday, aes (x = Total_Steps)) + geom_histogram() 
histogram <- histogram + xlab("Total Steps") + ylab("Count") + ggtitle("Distribution of Total Steps Taken Per Day") 
plot( histogram )

dev.copy(png,file="./Total_Steps_Per_Day.png",height=480, width=480)
dev.off()

# Calculating mean and median for totalstepsperday
totalmean <- mean( as.numeric(totalstepsperday$Total_Steps), na.rm = T)
totalmedian <- median( as.numeric(totalstepsperday$Total_Steps), na.rm = T)

# Plotting time series for daily activity pattern
intervalaverage <- dcast( datamelt, interval ~ variable, mean, na.rm =T)
colnames(intervalaverage) <- c("Interval", "Average_Across_All_Days")
plot(intervalaverage, type = "l", xlab = "5-minute Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern")
maxindex <- which.max(intervalaverage$Average_Across_All_Days)
maxinterval <- intervalaverage[maxindex,1]

dev.copy(png,file="./Average_Daily_Activity_Pattern.png",height=480, width=480)
dev.off()


# Imputations on missing values in the raw data set
numberofnas <- sum(is.na(rawdata$steps))
naindex <- which(is.na(rawdata$steps))
naintervals <- rawdata[naindex,3]
imputeddata <- rawdata
for (i in 1:length(naindex) ) {
    intervalrow <- which(intervalaverage$Interval == rawdata[naindex,3][i])
    imputeddata[naindex,1][i] <- intervalaverage[intervalrow,2]
}

# Melting the imputeddata
imputeddatamelt <- melt(imputeddata, id = c("date","interval"), measure.vars = c("steps"))

# Plotting histogram with mean and median lines for totalstepsperdayimputed
totalstepsperdayimputed <- dcast( imputeddatamelt, date ~ variable, sum)
colnames(totalstepsperdayimputed) <- c("Date","Total_Steps")
histogram2 <- ggplot (totalstepsperdayimputed, aes (x = Total_Steps)) + geom_histogram() 
histogram2 <- histogram2 + xlab("Total Steps") + ylab("Count") + ggtitle("Distribution of Total Steps Taken Per Day For Imputed Data") 
plot( histogram2 )

# Calculating mean and median for totalstepsperdayimputed
totalmeanimputed <- mean( as.numeric(totalstepsperdayimputed$Total_Steps), na.rm = T)
totalmedianimputed <- median( as.numeric(totalstepsperdayimputed$Total_Steps), na.rm = T)

dev.copy(png,file="./Total_Steps_Per_Day_Imputed.png",height=480, width=480)
dev.off()

# Generating the day of the week for all days in the imputed data into a fourth column
for (i in 1:nrow(imputeddata) ) {
    if ( weekdays(as.Date(imputeddata$date[i], "%Y-%m-%d")) %in% c("Friday","Saturday", "Sunday") ) {
        imputeddata[i,4] = "weekend"
    } else {
        imputeddata[i,4] = "weekday"
    }
}

# Formatting the fourth column as a factor
colnames(imputeddata)[4] <- "day"
imputeddata$day <- as.factor(imputeddata$day)

# Melting and casting the imputed data into desired form
meltedimputeddata <- melt( imputeddata, id = c("date","interval", "day"), measure.vars = "steps" )
castimputeddata <- dcast( meltedimputeddata, interval + day ~ variable, mean )

# Plotting the panelgraph
activitydifference <- xyplot(steps ~ interval | day, data = castimputeddata, type = "l", layout = c(1,2) )
plot(activitydifference)

dev.copy(png,file="./Difference_In_Activity.png",height=480, width=480)
dev.off()







