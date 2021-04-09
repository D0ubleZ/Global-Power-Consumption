library(EnvStats)
library(psych)
library(ggplot2)
library(chron)
library(corrplot)
library(lubridate)
library(depmixS4)
library(RSQLite)
library(dplyr)
library(ggpubr)
library(factoextra)

data <- read.table("TrainData.txt",
                   sep = "," , header = TRUE)
data$Date <- as.POSIXlt(data$Date,format="%d/%m/%Y")
data$Time <- as.POSIXlt(strptime(paste(data$Date,data$Time), format = "%Y-%m-%d %H:%M:%S"))

time_window_Mon <- subset(data,wday(as.Date(data$Date,'%d-%m-%Y'), label = TRUE) == 'Tue' & (format(Time,"%H:%M:%S") > '16:30:00') 
                          & (format(Time,"%H:%M:%S") <='20:59:59'))

time_window_Sat <- subset(data,wday(as.Date(data$Date,'%d-%m-%Y'), label = TRUE) == 'Sat' & (format(Time,"%H:%M:%S") > '16:30:00') 
                          & (format(Time,"%H:%M:%S") <='20:59:59'))

time_window_Mon <- na.omit(time_window_Mon)
time_window_Sat <- na.omit(time_window_Sat)

new_myd <- time_window_Sat[,3:9]
M_Sat <- cor(new_myd, method = "pearson")

#correlation matrix for Sat
corrplot(M_Sat, method="color")
title(main = "population Pearson correlation Mon",
      cex.main = 2, col.main= "black",
)

#correlation matrix for Mon
new_myd <- time_window_Mon[,3:9]
M_Mon <- cor(new_myd, method = "pearson")
corrplot(M, method="color")
title(main = "population Pearson correlation Sat",
      cex.main = 2 , col.main= "black",
)
#get and plot Sat Average Time
time_window_Sat$onlyTime <- format(time_window_Sat$Time, format="%H:%M:%S")
grouped_time_Sat_mean <- aggregate(time_window_Sat[, 3:9], list(time_window_Sat$onlyTime), mean)
colnames(grouped_time_Sat_mean)[1]<-"Time"
grouped_time_Sat_mean <- na.omit(grouped_time_Sat_mean)
grouped_time_Sat_mean$Time <- as.POSIXlt(grouped_time_Sat_mean$Time,format="%H:%M:%S")

plot(grouped_time_Sat_mean$Time, grouped_time_Sat_mean$Global_active_power)
title(main = "Average Global active power Saturday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Sat_mean$Time, grouped_time_Sat_mean$Global_reactive_power)
title(main = "Average Global reactive power Saturday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Sat_mean$Time, grouped_time_Sat_mean$Voltage)
title(main = "Average Voltage Saturday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Sat_mean$Time, grouped_time_Sat_mean$Global_intensity)
title(main = "Average Global Intensity Saturday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Sat_mean$Time, grouped_time_Sat_mean$Sub_metering_1)
title(main = "Average Sub_metering_1 Saturday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Sat_mean$Time, grouped_time_Sat_mean$Sub_metering_2)
title(main = "Average Sub_metering_2 Saturday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Sat_mean$Time, grouped_time_Sat_mean$Sub_metering_3)
title(main = "Average Sub_metering_3 Saturday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

#get and plot Mon Average Time
time_window_Mon$onlyTime <- format(time_window_Mon$Time, format="%H:%M:%S")
grouped_time_Mon_mean <- aggregate(time_window_Mon[, 3:9], list(time_window_Mon$onlyTime), mean)
colnames(grouped_time_Mon_mean)[1]<-"Time"
grouped_time_Mon_mean <- na.omit(grouped_time_Mon_mean)
grouped_time_Mon_mean$Time <- as.POSIXlt(grouped_time_Mon_mean$Time,format="%H:%M:%S")

plot(grouped_time_Mon_mean$Time, grouped_time_Mon_mean$Global_active_power)
title(main = "Average Global active power Monday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Mon_mean$Time, grouped_time_Mon_mean$Global_reactive_power)
title(main = "Average Global reactive power Monday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Mon_mean$Time, grouped_time_Mon_mean$Voltage)
title(main = "Average Voltage Saturday Monday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Mon_mean$Time, grouped_time_Mon_mean$Global_intensity)
title(main = "Average Global Intensity Monday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Mon_mean$Time, grouped_time_Mon_mean$Sub_metering_1)
title(main = "Average Sub_metering_1 Monday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Mon_mean$Time, grouped_time_Mon_mean$Sub_metering_2)
title(main = "Average Sub_metering_2 Monday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

plot(grouped_time_Mon_mean$Time, grouped_time_Mon_mean$Sub_metering_3)
title(main = "Average Sub_metering_3 Monday",
      cex.main = 2,   font.main= 3, col.main= "black",
)

M <- cor(grouped_time_Mon_mean$Global_active_power, grouped_time_Mon_mean$Global_reactive_power,  method = "pearson", use = "complete.obs")
M <- cor(new_myd, method = "pearson")
corrplot(M, method="color")
title(main = "population Pearson correlation Sat",
      cex.main = 2 , col.main= "black",
)

time_window_Mon$onlyDate <- format(time_window_Mon$Date, format="%Y-%M-%D")
average_window_Mon <- aggregate(time_window_Mon[, 3:9], list(time_window_Mon$onlyDate), mean)
colnames(average_window_Mon)[1]<-"Time"
average_window_Mon <- na.omit(average_window_Mon)

mtcars.pca <- prcomp(grouped_time_Sat_mean[2:8], center = TRUE,scale. = TRUE)
summary(mtcars.pca)
str(mtcars.pca)
fviz_eig(mtcars.pca)

fviz_pca_ind(mtcars.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(mtcars.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
fviz_pca_var(mtcars.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#partion train and test
training_Sat <- subset(time_window_Sat,(format(Time,"%H:%M:%S") <'16:40:00'))
testing_Sat <- subset(time_window_Sat,(format(Time,"%H:%M:%S") > '20:50:00'))
trainng_Mon <- subset(time_window_Mon,(format(Time,"%H:%M:%S") < '16:40:00'))
testing_Mon <- subset(time_window_Mon,(format(Time,"%H:%M:%S") > '20:50:00'))

training_Sat=na.omit(training_Sat)
training_Mon=na.omit(trainng_Mon)
testing_Sat=na.omit(testing_Sat)
testing_Mon=na.omit(testing_Mon)

training <- rbind(training_Sat[,3:6],training_Mon[,3:6])
testing <- rbind(testing_Sat[,3:6],testing_Mon[,3:6])
#univariate model training (Global_reactive_power)
Mod_1 <- vector("numeric", 17)
logLik_1 <- vector("numeric", 17)
for (i in 4:20) {
  mod <- depmix(response = Global_active_power~1,data=training,nstates=i)
  fitmod <- fit(mod)
  summary(mod)
  Mod_1[i-3]<-BIC(fitmod)
  logLik_1[i-3]<-logLik(fitmod)
}
evaluate_1 <- vector("numeric", 17)
for(i in 1:17){
  evaluate_1[i]<-abs(Mod_1[i]*logLik_1[i])
}
min(evaluate_1)
#Global_reactive_power& Global_active_power
Mod_2 <- vector("numeric", 17)
logLik_2 <- vector("numeric", 17)
for(i in 4:20){
  mod <- depmix(list(Global_active_power~1,Global_reactive_power~1),data=training,nstates=i,family=list(gaussian(),gaussian()), ctimes=c(1377,1377))
  fitmod <- fit(mod)
  Mod_2[i-3]<-BIC(fitmod)
  logLik_2[i-3]<-logLik(fitmod)
}
#Voltage& Global_active_power
Mod_3 <- vector("numeric", 17)
logLik_3 <- vector("numeric", 17)
for(i in 4:20){
  mod <- depmix(list(Global_active_power~1,Voltage~1),data=training,nstates=i,family=list(gaussian(),gaussian()),ctimes=c(918,918,918))
  fitmod <- fit(mod)
  Mod_3[i-3]<-BIC(fitmod)
  logLik_3[i-3]<-logLik(fitmod)
}
evaluate_3 <- vector("numeric", 17)
for(i in 1:17){
  evaluate_3[i]<-abs(Mod_3[i]*logLik_3[i])
}
min(evaluate_3)

#Global_intensity& Global_active_power
Mod_4 <- vector("numeric", 17)
logLik_4 <- vector("numeric", 17)
for(i in 4:20){
  mod <- depmix(list(Global_active_power~1,Global_intensity~1),data=training,nstates=i,family=list(gaussian(),gaussian()),ctimes=c(918,918,918))
  fitmod <- fit(mod)
  Mod_4[i-3]<-BIC(fitmod)
  logLik_4[i-3]<-logLik(fitmod)
}
evaluate_4 <- vector("numeric", 17)
for(i in 1:17){
  evaluate_4[i]<-abs(Mod_4[i]*logLik_4[i])
}
min(evaluate_4)
#Global_intensity& Global_active_power&Voltage
Mod_5 <- vector("numeric", 17)
logLik_5 <- vector("numeric", 17)
for(i in 4:20){
  mod <- depmix(list(Global_active_power~1,Global_intensity~1,Voltage~1),data=training,nstates=i,family=list(gaussian(),gaussian(),gaussian()),ctimes=c(918,918,918))
  fitmod <- fit(mod)
  Mod_5[i-3]<-BIC(fitmod)
  logLik_5[i-3]<-logLik(fitmod)
}
evaluate_5 <- vector("numeric", 17)
for(i in 1:17){
  evaluate_5[i]<-abs(Mod_5[i]*logLik_5[i])
}
min(evaluate_5)
#Global_reactive_power& Global_active_power&Global_intensity
Mod_6 <- vector("numeric", 17)
logLik_6 <- vector("numeric", 17)
for(i in 4:20){
  mod <- depmix(list(Global_active_power~1,Global_intensity~1,Global_reactive_power~1),data=training,nstates=i,family=list(gaussian(),gaussian(),gaussian()),ctimes=c(918,918,918))
  fitmod <- fit(mod)
  Mod_6[i-3]<-BIC(fitmod)
  logLik_6[i-3]<-logLik(fitmod)
}
evaluate_6 <- vector("numeric", 17)
for(i in 1:17){
  evaluate_6[i]<-abs(Mod_6[i]*logLik_6[i])
}
min(evaluate_6)
#Global_reactive_power& Global_active_power&Voltage
Mod_7 <- vector("numeric", 17)
logLik_7 <- vector("numeric", 17)
for(i in 4:20){
  mod <- depmix(list(Global_active_power~1,Global_intensity~1,Voltage~1),data=training,nstates=i,family=list(gaussian(),gaussian(),gaussian()),ctimes=c(918,918,918))
  fitmod <- fit(mod)
  Mod_7[i-3]<-BIC(fitmod)
  logLik_7[i-3]<-logLik(fitmod)
}
evaluate_7 <- vector("numeric", 17)
for(i in 1:17){
  evaluate_7[i]<-abs(Mod_7[i]*logLik_7[i])
}
min(evaluate_7)
#testing part
mod <- depmix(response = Global_active_power~1,data=training,nstates=19)
fitmod <- fit(mod)
modNew <- setpars(mod,getpars(fitmod))
fitfm <- fit(modNew)
BIC(fitfm)
logLik(fitfm)

mod <- depmix(list(Global_active_power~1,Global_intensity~1),data=training,nstates=20,family=list(gaussian(),gaussian()),ctimes=c(918,918,918))
fitmod <- fit(mod)
modNew <- setpars(mod,getpars(fitmod))
fitfm <- fit(modNew)
BIC(fitfm)
logLik(fitfm)

mod <- depmix(list(Global_active_power~1,Global_intensity~1,Global_reactive_power~1),data=training,nstates=11,family=list(gaussian(),gaussian(),gaussian()),ctimes=c(918,918,918))
fitmod <- fit(mod)
modNew <- setpars(mod,getpars(fitmod))
fitfm <- fit(modNew)
BIC(fitfm)
logLik(fitfm)
