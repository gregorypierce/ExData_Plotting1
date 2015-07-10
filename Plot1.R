library(dplyr)

plot1 <- function() { 
  png( filename="plot1.png", width=480, height=480, units="px")
  ## create a consumption structure that holds all of the power consumption data
  consumption <- read.table("household_power_consumption.txt", sep=";", header=TRUE, colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings = c("?") )  
  
  # build a DateTime for this data
  consumption$DateTime <- paste( consumption$Date, consumption$Time )
  consumption$DateTime <- as.Date( consumption$DateTime, format="%d/%m/%Y %H:%M:%S")
  
  # convert the Date for other use
  consumption$Date <- as.Date(consumption$Date, "%d/%m/%Y")
  
  # convert the Time for other use
  consumption$Time <- strptime(consumption$Time, "%H:%M:%S")

  # subset the date for the particular date of the plot based upon the date 2/1/07 and 2/2/07
  window <- subset(consumption, consumption$Date == as.Date("2007-02-01"))
  window2 <- subset(consumption, consumption$Date == as.Date("2007-02-02"))
  total <- rbind( window, window2 )
  
  # generate the histogram for this plot
  hist( total$Global_active_power, col="red", ylim=c(0,1300), main="Global Active Power", xlab="Global Active Power (kilowatts)" )
  
  dev.off()
}