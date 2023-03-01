library(lubridate)
library(ggplot2)
library(depmixS4)

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
#ggplot(scaled_data, aes(x= scaled_data$Time, y= scaled_data$Global_active_power)) + geom_point()
#unique(scaled_data$Date)

set.seed(1)

res_list <- array(c(0), dim = c(14,3))

# Insert logLik and BIC values for each model and number of states into a 2d array
for (i in 3:16) {

  print(paste("nstates ", i, " run ", i-2))
  
  d <- weeks[[i]]
  mod <- depmix(response = scaled_data$Global_active_power ~ 1, data = scaled_data, nstates = i, ntimes = rep(c(240), each=52))
  
  fm <- fit(mod)
  
  res_list[i-2, 1] = logLik(fm)
  res_list[i-2, 2] = AIC(fm)
  res_list[i-2, 3] = BIC(fm)
  print(paste("logLik ", logLik(fm), " AIC ", AIC(fm), " BIC ", BIC(fm)))
}

print(res_list)

d <- data.frame(res_list)
names(d) = c("logLik", "AIC", "BIC")
d$nstates = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
print(d)

plot(unlist(d[1]), unlist(d[3]), xlab = "logLik", ylab = "BIC")
plot(unlist(d[4]), unlist(d[1]), xlab = "nstates", ylab = "logLik")
plot(unlist(d[4]), unlist(d[3]), xlab = "nstates", ylab = "BIC")
