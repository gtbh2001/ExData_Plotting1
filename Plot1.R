# Script Plot 1
# Plot a Histogram of data set from the UC Irvine Machine Learning Repository:
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
        
        Quant1st <- quantile(Global_active_power, probs = 0.25, na.rm = TRUE, name=FALSE)
        Quant3dr <- quantile(Global_active_power, probs = 0.75, na.rm = TRUE, name=FALSE)
        
        hist(Global_active_power, col="red", main="Global Active Power",
             xlab = "Global Active Power (In kilowatts)")
        
        abline(v=median(Global_active_power, na.rm = TRUE), lty=3, lwd= 3, col="blue")
        text(median(Global_active_power, na.rm = TRUE),4350, "Median", col = "blue")
        
        abline(v=Quant3dr, lty=3, lwd=3, col="green")
        text(Quant3dr,3000, "3st Qu", col = "green")
        
        abline(v=Quant1st, lty=3, lwd=3, col="black")
        text(Quant1st, 2000, "1st Qu", col = "black")
        
# copy chart to a pgn file with size of 480x480 ( default size for png device) 
        
        dev.copy(png, filename="plot1.png")
        dev.off()
        
       }
    )








