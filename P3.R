library(lubridate)
library(ggplot2)

### Import data ###
getwd()
df <- read.table("Group_Assignment_3_Dataset.txt", header = TRUE, sep = ",")
df <- na.omit(df)


selected_data <- df[c("Date", "Time", "Global_active_power")]
scaled_data <- cbind(selected_data["Date"], selected_data["Time"], scale(selected_data["Global_active_power"]) )


# Extracting Data
scaled_data <- subset(scaled_data, wday(as.Date(scaled_data$Date, format = "%d/%m/%y")) == 5)
scaled_data$Date <- as.Date(scaled_data$Date, format = "%d/%m/%y")
startTime <- strptime("06:20:00", format="%H:%M:%S")
endTime <- strptime("10:20:00", format="%H:%M:%S")
scaled_data <- subset(scaled_data, difftime(strptime(scaled_data$Time, format="%H:%M:%S"), startTime) >= 0 & difftime(strptime(scaled_data$Time, format="%H:%M:%S"), endTime) < 0)
#scaled_data <- subset(scaled_data, scaled_data$Date == as.Date("30/1/2007", format = "%d/%m/%y"))
#ggplot(scaled_data, aes(x= scaled_data$Time, y= scaled_data$Global_active_power)) + geom_point()
