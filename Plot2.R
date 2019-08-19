# Script Plot 2
# Plot line chart of data set from the UC Irvine Machine Learning Repository:
#“Individual household electric power consumption Data Set”
# The date set is available on https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#

#loads Library needs on this script
library(dplyr)
library(lubridate)

#define some constans to be used on script

WorkingDir <- getwd()

DatasetFileName <- "household_power_consumption.txt"

ZipFilename <- "exdata_data_household_power_consumption.zip"

DataSetUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

# Download data set

# Avoid download the file if already exist on working directory

if(!file.exists(ZipFilename))
  {
      download.file(DataSetUrl,destfile = ZipFilename)
      unzip(ZipFilename) 
}  

# Load dataset on R objetc


HouseHoldPowerConsumption <- read.table(DatasetFileName, header = TRUE, sep = ";",
                                        stringsAsFactors = FALSE, na.strings = "?")

#transfrom on a special DF
tmp <- tbl_df(HouseHoldPowerConsumption)

#Subset dateset to hold just observations from 2007-02-01 to 2007-02-02

HouseHoldPowerConsumption <- filter(tmp, Date == as.Date("01/02/2007") | Date == as.Date("02/02/2007")) %>%
                             mutate( DateTime = dmy_hms(paste(Date, Time) ) ) 

#plot the histogram of Global active power variable

with(HouseHoldPowerConsumption,
     
     {
       x <- DateTime[year(DateTime) == 2007]
       y <- Global_active_power[year(DateTime) == 2007]
       
       plot(x,y, xlab = "", ylab = "Global Active Power (kilowatts)", type = "l", main = "Global Active Power Per Hour")
       
       abline(h=mean(y), col="red", lty=2,lwd=4)
       
       abline(h=median(y), col="green", lty=2,lwd=4)
       
       legend("topright", col=c("red", "green"), legend = c("Mean","Median") , pch = c("-","-"),lwd = 4)
       
       dev.copy(png, filename="./figure/plot2.png")
       dev.off()
     }
)







