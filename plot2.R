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
plot(pow_data1$DateTime, pow_data1$Global_active_power, type = "n", ylab = "Global Active Power (kilowatts)", xlab = "")
lines(pow_data1$DateTime, pow_data1$Global_active_power, type = "l")
dev.copy(png, "plot2.png")
dev.off()
dev.off()