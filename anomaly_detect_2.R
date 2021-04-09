library(EnvStats)
library(psych)
library(ggplot2)
library(chron)
library(corrplot)
library(lubridate)
library(RSQLite)
library(dplyr)
library(ggpubr)
library(factoextra)
library(depmixS4)
library(TTR)


test_data <- function(file){
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

test.1 <- test_data('test1.txt')
test.2 <- test_data('test2.txt')
test.3 <- test_data('test3.txt')
test.4 <- test_data('test4.txt')
test.5 <- test_data('test5.txt')



anomaly_Detect <- function(d,v,z){
  z <- 2
  or_data <- data.frame(Date = d$Date,Time = d$Time,Data = v)
  
  or_data$Mean <- runMean(or_data$Data, n=10)
  
  or_data$SD = runSD(or_data$Data, n=10)
  anomaly <- or_data[((or_data$Data > or_data$Mean + z * or_data$SD) | (or_data$Data < or_data$Mean - z * or_data$SD)) & !is.na(or_data$Mean),]
  
  or_data <- na.omit(or_data)

  return(anomaly)
}





#anomaly in dataset test1
anomaly_GAP.1 <- anomaly_Detect(test.1,test.1$Global_active_power,3)
anomaly_GRP.1 <- anomaly_Detect(test.1,test.1$Global_reactive_power,3)
anomaly_V.1 <- anomaly_Detect(test.1,test.1$Voltage,3)
anomaly_GI.1 <- anomaly_Detect(test.1,test.1$Global_intensity,3)
anomaly_sm1.1 <- anomaly_Detect(test.1,test.1$Sub_metering_1,3)
anomaly_sm2.1 <- anomaly_Detect(test.1,test.1$Sub_metering_2,3)
anomaly_sm3.1 <- anomaly_Detect(test.1,test.1$Sub_metering_3,3)

index <-as.integer(row.names(anomaly_GI.1))
tmp <- test.1[index,]
tmp <- subset(tmp,Date == "2009-12-07")

index1 <-as.integer(row.names(anomaly_GRP.1))
tmp1 <- test.1[index1,]
tmp1 <- subset(tmp1,Date == "2009-12-07")

test.1 <- subset(test.1,Date == "2009-12-07")


plot(test.1$Time,test.1$Global_intensity,main="Anomaly detect in 2009-12-07(Z-Score = 2)",xlab = 'Time',ylab = 'Global_intensity',pch = 16,col = 'blue',type = 'l')
points(tmp$Time,tmp$Global_intensity,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)

plot(test.1$Time,test.1$Global_reactive_power,main="Anomaly detect in 2009-12-07(Z-Score = 2)",xlab = 'Time',ylab = 'Global_reactive_power',pch = 16,col = 'blue',type = 'l')
points(tmp1$Time,tmp1$Global_reactive_power,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)



#anomaly in dataset test2
anomaly_GAP.2 <- anomaly_Detect(test.2,test.2$Global_active_power,3)
anomaly_GRP.2 <- anomaly_Detect(test.2,test.2$Global_reactive_power,3)
anomaly_V.2 <- anomaly_Detect(test.2,test.2$Voltage,3)
anomaly_GI.2 <- anomaly_Detect(test.2,test.2$Global_intensity,3)
anomaly_sm1.2 <- anomaly_Detect(test.2,test.2$Sub_metering_1,3)
anomaly_sm2.2 <- anomaly_Detect(test.2,test.2$Sub_metering_2,3)
anomaly_sm3.2 <- anomaly_Detect(test.2,test.2$Sub_metering_3,3)

index <-as.integer(row.names(anomaly_GI.2))
tmp <- test.2[index,]
tmp <- subset(tmp,Date == "2009-12-07")

index1 <-as.integer(row.names(anomaly_GRP.2))
tmp1 <- test.2[index1,]
tmp1 <- subset(tmp1,Date == "2009-12-07")

test.2 <- subset(test.2,Date == "2009-12-07")


plot(test.2$Time,test.2$Global_intensity,main="Anomaly detect in 2009-12-07(Z-Score = 2)",xlab = 'Time',ylab = 'Global_intensity',pch = 16,col = 'blue',type = 'l')
points(tmp$Time,tmp$Global_intensity,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)

plot(test.2$Time,test.2$Global_reactive_power,main="Anomaly detect in 2009-12-07(Z-Score = 2)",xlab = 'Time',ylab = 'Global_reactive_power',pch = 16,col = 'blue',type = 'l')
points(tmp1$Time,tmp1$Global_reactive_power,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)

#anomaly in dataset test3
anomaly_GAP.3 <- anomaly_Detect(test.3,test.3$Global_active_power,3)
anomaly_GRP.3 <- anomaly_Detect(test.3,test.3$Global_reactive_power,3)
anomaly_V.3 <- anomaly_Detect(test.3,test.3$Voltage,3)
anomaly_GI.3 <- anomaly_Detect(test.3,test.3$Global_intensity,3)
anomaly_sm1.3 <- anomaly_Detect(test.3,test.3$Sub_metering_1,3)
anomaly_sm2.3 <- anomaly_Detect(test.3,test.3$Sub_metering_2,3)
anomaly_sm3.3 <- anomaly_Detect(test.3,test.3$Sub_metering_3,3)

index <-as.integer(row.names(anomaly_GI.3))
tmp <- test.3[index,]
tmp <- subset(tmp,Date == "2009-12-07")

index1 <-as.integer(row.names(anomaly_GRP.3))
tmp1 <- test.3[index1,]
tmp1 <- subset(tmp1,Date == "2009-12-07")

test.3 <- subset(test.3,Date == "2009-12-07")


plot(test.3$Time,test.3$Global_intensity,main="Anomaly detect in 2009-12-07(Z-Score = 2)",xlab = 'Time',ylab = 'Global_intensity',pch = 16,col = 'blue',type = 'l')
points(tmp$Time,tmp$Global_intensity,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)

plot(test.3$Time,test.3$Global_reactive_power,main="Anomaly detect in 2009-12-07(Z-Score = 2)",xlab = 'Time',ylab = 'Global_reactive_power',pch = 16,col = 'blue',type = 'l')
points(tmp1$Time,tmp1$Global_reactive_power,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)

#anomaly in dataset test4
anomaly_GAP.4 <- anomaly_Detect(test.4,test.4$Global_active_power,3)
anomaly_GRP.4 <- anomaly_Detect(test.4,test.4$Global_reactive_power,3)
anomaly_V.4 <- anomaly_Detect(test.4,test.4$Voltage,3)
anomaly_GI.4 <- anomaly_Detect(test.4,test.4$Global_intensity,3)
anomaly_sm1.4 <- anomaly_Detect(test.4,test.4$Sub_metering_1,3)
anomaly_sm2.4 <- anomaly_Detect(test.4,test.4$Sub_metering_2,3)
anomaly_sm3.4 <- anomaly_Detect(test.4,test.4$Sub_metering_3,3)

index <-as.integer(row.names(anomaly_GI.4))
tmp <- test.4[index,]
tmp <- subset(tmp,Date == "2010-2-28")

index1 <-as.integer(row.names(anomaly_GRP.4))
tmp1 <- test.4[index1,]
tmp1 <- subset(tmp1,Date == "2009-12-07")

test.tmp <- subset(test.4,Date == "2010-2-28")

test.4 <- subset(test.4,Date == "2009-12-07")


plot(test.tmp$Time,test.tmp$Global_intensity,main="Anomaly detect in 2010-2-28",xlab = 'Time',ylab = 'Global_intensity',pch = 16,col = 'blue',type = 'l')
points(tmp$Time,tmp$Global_intensity,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)

plot(test.4$Time,test.4$Global_reactive_power,main="Anomaly detect in 2009-12-07",xlab = 'Time',ylab = 'Global_reactive_power',pch = 16,col = 'blue',type = 'l')
points(tmp1$Time,tmp1$Global_reactive_power,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)
#anomaly in dataset test5
anomaly_GAP.5 <- anomaly_Detect(test.5,test.5$Global_active_power,3)
anomaly_GRP.5 <- anomaly_Detect(test.5,test.5$Global_reactive_power,3)
anomaly_V.5 <- anomaly_Detect(test.5,test.5$Voltage,3)
anomaly_GI.5 <- anomaly_Detect(test.5,test.5$Global_intensity,3)
anomaly_sm1.5 <- anomaly_Detect(test.5,test.5$Sub_metering_1,3)
anomaly_sm2.5 <- anomaly_Detect(test.5,test.5$Sub_metering_2,3)
anomaly_sm3.5 <- anomaly_Detect(test.5,test.5$Sub_metering_3,3)

index <-as.integer(row.names(anomaly_GI.5))
tmp <- test.5[index,]
tmp <- subset(tmp,Date == "2009-12-07")

index1 <-as.integer(row.names(anomaly_GRP.5))
tmp1 <- test.5[index1,]
tmp1 <- subset(tmp1,Date == "2009-12-07")

test.5 <- subset(test.5,Date == "2009-12-07")


plot(test.5$Time,test.5$Global_intensity,main="Anomaly detect in 2009-12-07",xlab = 'Time',ylab = 'Global_intensity',pch = 16,col = 'blue',type = 'l')
points(tmp$Time,tmp$Global_intensity,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)

plot(test.5$Time,test.5$Global_reactive_power,main="Anomaly detect in 2009-12-07",xlab = 'Time',ylab = 'Global_reactive_power',pch = 16,col = 'blue',type = 'l')
points(tmp1$Time,tmp1$Global_reactive_power,col ='red',pch = 16)

legend("topright", legend = c('Anomaly','Normal'),
       col =  c("red", "blue"), pch = 16)
