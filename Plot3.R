library(dplyr)

plot3 <- function() {  
  png( filename="plot3.png", width=480, height=480, units="px")
  ## create a consumption structure that holds all of the power consumption data
  consumption <- read.table("household_power_consumption.txt", sep=";", header=TRUE, colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings = c("?") )  
  
  # build a DateTime for this data
  consumption$DateTimePasted <- paste( consumption$Date, consumption$Time )
  consumption$DateTime <- as.POSIXct( consumption$DateTimePasted, format="%d/%m/%Y %H:%M:%S")
  
  # convert the Date for other use
  consumption$Date <- as.Date(consumption$Date, "%d/%m/%Y")
  
  # convert the Time for other use
  consumption$Time <- strptime(consumption$Time, "%H:%M:%S")
  
  # subset the date for the particular date of the plot based upon the date 2/1/07 and 2/2/07
  window <- subset(consumption, consumption$Date == as.Date("2007-02-01"))
  window2 <- subset(consumption, consumption$Date == as.Date("2007-02-02"))
  total <- rbind( window, window2 )
  
  # generate the plot of Time vs GlobalActivePower for this subset of data
  plot( total$DateTime, total$Sub_metering_1, pch=".", xlab="", ylab="Energy sub metering" )
  lines( total$DateTime, total$Sub_metering_1 )
  
  points( total$DateTime, total$Sub_metering_2, pch=".", col="red")
  lines( total$DateTime, total$Sub_metering_2, col="red")
  
  points( total$DateTime, total$Sub_metering_3, pch=".", col="blue")
  lines( total$DateTime, total$Sub_metering_3, col="blue")
  
  
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), col = c("black", "red", "blue"))  
  # flip the device off so it will know to render properly
  dev.off()
}