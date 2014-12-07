data <- read.table("./expo/household_power_consumption.txt", header = T, sep =";", na.strings = "?")

##predict data size
##create function
predict_data_size = function(numeric_size, number_type = "numeric") {
  if(number_type == "integer") {
    byte_per_number = 4
  } else if(number_type == "numeric") {
    byte_per_number = 8
  } else {
    stop(sprintf("Unknown number_type: %s", number_type))
  }
  estimate_size_in_bytes = (numeric_size * byte_per_number)
  class(estimate_size_in_bytes) = "object_size"
  print(estimate_size_in_bytes, units = "auto")
}
predict_data_size(2075259*9, "numeric")

##merge date and time
data$datetime <- paste(data$Date, data$Time)
##format date
date2 <- as.Date(data$Date, format = "%d/%m/%Y")
date1 <- strptime(data$datetime, "%d/%m/%Y %H:%M:%S")
## subset 
library(lubridate)
date.sel <- which(year(date1) == 2007 & month(date1) == 2 & mday(date1) == 01)
date.sel.2 <- which(year(date1) == 2007 & month(date1) == 2 & mday(date1) == 02)
data.sel <- data[c(date.sel, date.sel.2), ]

##hist plot1
png(file = "plot1.png") 
hist(data.sel$Global_active_power, col ="red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()
