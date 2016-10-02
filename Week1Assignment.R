################################################################################################
##################### Exploratory Analysis - Week1 - Peer Reviewed Assignment ##################
################################################################################################

require("downloader")
require("data.table")
require("dplyr")
require("dtplyr")

## Downloading the dataset, unzipping the file
download("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", dest="./W1_assgn.zip", mode="wb") 
unzip ("W1_assgn.zip", exdir = "./")

## Reading in all of the data to a data.table
txt <- "./household_power_consumption.txt"
pow <- tbl_df(read.table(txt, na.strings = "?", sep = ";", header = TRUE))

## Finding the index for the date 1/2/2007, then the number of rows that include the data from there to the end of the day on 2/2/2007
rownum <- length(which(pow$Date == "1/2/2007")) + length(which(pow$Date == "2/2/2007"))
# = 2880 days or 2880 rows of data

namchar <- tbl_df(read.table(txt, nrows = 1, sep = ";", colClasses = c(rep("character",9))))

## Reading only the data between 1/2/2007 and 2/2/2007 (2 days) and overwriting `pow`
pow <- tbl_df(read.table(txt, skip = 66637, nrows = 2880, na.strings = "?", sep = ";", header = TRUE, col.names = namchar))

## Adds new row called DateTime by pasting "Date" and "Time" columns together - though still as a factor
pow_m <- mutate(pow, datetime = paste(Date, Time, sep = ""))

## Selects required data columns and creates a new vector from "datetime" column in Date/Time format
pow_sel <- select(pow_m, datetime, Global_active_power,Global_reactive_power,Voltage, Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3)
DateTime <- strptime(pow_m$datetime, format = "%e/%m/%Y %H:%M:%S")
      
## Creates new data frame from selected data with an added column, the new Date/Time vector --- Then selects only the required data columns
pow_Date <- tbl_df(data.table(pow_m, DateTime))
pow_data1 <- select(pow_Date, DateTime, Global_active_power,Global_reactive_power,Voltage, Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3)
####################################################################################################
####################################################################################################

## Generating Plot1 - Single Histogram
hist(pow_data1$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power")
dev.copy(png, "plot1.png")
dev.off()
dev.off()
## Generating Plot2 - Global Active Power by Day of the Week
require("lubridate")
## Date_Format <- mutate(pow, Format = dmy(Date))
## Date_Day$Global_active_power <- as.numeric(Date_Day$Global_active_power)
## Date_Day$Day <- as.factor(Date_Day$Day)
## Date_Day <- mutate(Date_Format, Day = wday(Date_Format$Format, label = TRUE))

plot(pow_data1$DateTime, pow_data1$Global_active_power, type = "n", ylab = "Global Active Power (kilowatts)", xlab = "")
lines(pow_data1$DateTime, pow_data1$Global_active_power, type = "l")
dev.copy(png, "plot2.png")
dev.off()
dev.off()
## Generating Plot3 - Energy sub metering
lgnd <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
par(mar = c(0.5,0.4,0.4,0.1), fin = c(6,6))
plot(pow_data1$DateTime, pow_data1$Sub_metering_1 , type = "n", ylab = "Energy sub metering", xlab = "")
lines(pow_data1$DateTime, pow_data1$Sub_metering_1, type = "l")
lines(pow_data1$DateTime, pow_data1$Sub_metering_2, type = "l", col = "blue")
lines(pow_data1$DateTime, pow_data1$Sub_metering_3, type = "l", col = "red")
legend('topright', lgnd, lty = 1, col = c("black", "blue", "red"))
dev.copy(png, "plot3.png")
dev.off()
dev.off()
## Generating Plot4 - 4 plots by row: Global Active Power, Voltage, Energy Sub Metering, Global Reactive Power

par(mfrow = c(2,2), mar = c(5,3,3,1))

## Top Left Plot - Global Active Power
plot(pow_data1$DateTime, pow_data1$Global_active_power, type = "n", ylab = "Global Active Power (kilowatts)", xlab = "")
lines(pow_data1$DateTime, pow_data1$Global_active_power, type = "l")

## Top Right Plot - Voltage
plot(pow_data1$DateTime, pow_data1$Voltage, type = "n", ylab = "Voltage", xlab = "datetime")
lines(pow_data1$DateTime, pow_data1$Voltage, type = "l")

## Bottom Left Plot - Energy Sub-Metering
plot(pow_data1$DateTime, pow_data1$Sub_metering_1 , type = "n", ylab = "Energy sub metering", xlab = "")
lines(pow_data1$DateTime, pow_data1$Sub_metering_1, type = "l")
lines(pow_data1$DateTime, pow_data1$Sub_metering_2, type = "l", col = "blue")
lines(pow_data1$DateTime, pow_data1$Sub_metering_3, type = "l", col = "red")
legend('topright', lgnd, lty = 1, col = c("black", "blue", "red"), bty = "n", inset = c(0.2,0))

## Bottom Right Plot - Global Reactive Power
plot(pow_data1$DateTime, pow_data1$Global_reactive_power, type = "n", ylab = "Global Active Power (kilowatts)", xlab = "datetime")
lines(pow_data1$DateTime, pow_data1$Global_reactive_power, type = "l")

dev.copy(png, "plot4.png")
dev.off()
dev.off()




