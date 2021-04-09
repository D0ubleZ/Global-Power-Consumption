library(e1071)
library(rgl)
library(scatterplot3d)
library(RColorBrewer)
library(lubridate)
library(TTR)


read_data <- function(file){
  test <- read.table(file,
                     sep = "," , header = TRUE)
  test$Date <- as.POSIXlt(test$Date,format="%d/%m/%Y")
  test$Time <- as.POSIXlt(strptime(paste(test$Date,test$Time), format = "%Y-%m-%d %H:%M:%S"))
  test <- na.omit(test)
  time_window <- subset(test,(wday(as.Date(test$Date,'%d-%m-%Y'), label = TRUE) == 'Mon' &(format(Time,"%H:%M:%S") > '20:30:00') 
                              & (format(Time,"%H:%M:%S") <='23:59:59')) | (wday(as.Date(test$Date,'%d-%m-%Y'), label = TRUE) == 'Sun' &(format(Time,"%H:%M:%S") > '20:30:00') 
                                                                           & (format(Time,"%H:%M:%S") <='23:59:59')) )
  return(time_window)
  
}

add_newSD2data<- function(file){
  data <- file
  data$Global_active_power_SD = runSD(data$Global_active_power, n=10)
  data$Global_reactive_power_SD = runSD(data$Global_reactive_power, n=10)
  data$Voltage_SD = runSD(data$Voltage, n=10)
  data$Global_intensity_SD = runSD(data$Global_intensity, n=10)
  data$Sub_metering_1_SD = runSD(data$Sub_metering_1, n=10)
  data$Sub_metering_2_SD = runSD(data$Sub_metering_2, n=10)
  data$Sub_metering_3_SD = runSD(data$Sub_metering_3, n=10)
  data <- na.omit(data)
  return(data)
}





test <- read_data('test3.txt')
train <- read_data('TrainData.txt')


data <- rbind(test,train)

newdata <- add_newSD2data(data)



svm <- e1071::svm(data[3:5], nu=0.09, type="one-classification", kernel="polynomial")
svm
out_svm <- predict(svm)
index_svm <- which(out_svm==TRUE)
index_svm
data[index_svm,]




data$TF <- out_svm
data$TF <- as.integer(data$TF)
data$TF[data$TF == 1] <- 'red'
data$TF[data$TF == 0] <- 'blue'
#plot3d(data$Global_active_power,
#       data$Global_reactive_power,
#       data$Voltage,
#       xlab = 'Global_active_power',
#       ylab = 'Global_reactive_power',
#       zlab = 'Voltage',
#       col = data$TF,
#       size = 8)




with(data, 
     scatterplot3d(Voltage,
                   Global_active_power,
                   Global_reactive_power,
                   main="Anomaly detect",
                   xlab = 'Voltage',
                   ylab = 'Global_active_power',
                   zlab = 'Global_reactive_power',
                   pch = 16, 
                   color=TF))

legend("right", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)



new_svm <- e1071::svm(newdata[10:12], nu=0.05, type="one-classification", kernel="polynomial")
new_svm
new_out_svm <- predict(new_svm)
new_index_svm <- which(new_out_svm==TRUE)
new_index_svm
newdata[new_index_svm,]

newdata$TF <- new_out_svm
newdata$TF <- as.integer(newdata$TF)
newdata$TF[newdata$TF == 1] <- 'red'
newdata$TF[newdata$TF == 0] <- 'blue'
#plot3d(newdata$Global_active_power_SD,
#      newdata$Global_reactive_power_SD,
#      newdata$Voltage_SD,
#       xlab = 'Global_active_power',
#       ylab = 'Global_reactive_power',
#       zlab = 'Voltage',
#       col = newdata$TF,
#       size = 8)

with(newdata, 
     scatterplot3d(Global_active_power_SD,
                   Voltage_SD,
                   Global_reactive_power_SD,
                   main="Anomaly detect with Standard Deviation",
                   xlab = 'Voltage Moving Standard Deviation',
                   ylab = 'Global_active_power Moving Standard Deviation',
                   zlab = 'Global_reactive_power Moving Standard Deviation',
                   pch = 16, 
                   color=TF))

legend("right", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)

