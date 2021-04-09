library(EnvStats)
library(psych)
library(ggplot2)
library(chron)
library(corrplot)
library(lubridate)
library(depmixS4)
#datasets
data1 <- read.table("test1.txt",
                   sep = "," , header = TRUE)

data2 <- read.table("test2.txt",
                   sep = "," , header = TRUE)
data3 <- read.table("test3.txt",
                    sep = "," , header = TRUE)
data4 <- read.table("test4.txt",
                    sep = "," , header = TRUE)
data5 <- read.table("test5.txt",
                    sep = "," , header = TRUE)

data1$Date <- as.POSIXlt(data1$Date,format="%d/%m/%Y")
data2$Date <- as.POSIXlt(data2$Date,format="%d/%m/%Y")
data3$Date <- as.POSIXlt(data3$Date,format="%d/%m/%Y")
data4$Date <- as.POSIXlt(data4$Date,format="%d/%m/%Y")
data5$Date <- as.POSIXlt(data5$Date,format="%d/%m/%Y")

data1$Time <- as.POSIXlt(strptime(paste(data1$Date,data1$Time), format = "%Y-%m-%d %H:%M:%S"))
data2$Time <- as.POSIXlt(strptime(paste(data2$Date,data2$Time), format = "%Y-%m-%d %H:%M:%S"))
data3$Time <- as.POSIXlt(strptime(paste(data3$Date,data3$Time), format = "%Y-%m-%d %H:%M:%S"))
data4$Time <- as.POSIXlt(strptime(paste(data4$Date,data4$Time), format = "%Y-%m-%d %H:%M:%S"))
data5$Time <- as.POSIXlt(strptime(paste(data5$Date,data5$Time), format = "%Y-%m-%d %H:%M:%S"))

#decide time window on weekdays
weekday_afternoon <- subset(data1, Date == "2009-12-01")
weekday_morning <- subset(data5, Date == "2009-12-05" & (format(Time,"%H:%M:%S") >= '08:00:00') & (format(Time,"%H:%M:%S") <= '11:59:00'))


plot(weekday_afternoon$Time, weekday_afternoon$Global_intensity)
title(main = "Global intensity sample afternoon Monday",
      cex.main = 1.5,   font.main= 4, col.main= "black",
)

plot(weekday_morning$Time, weekday_morning$Global_intensity)
title(main = "Global intensity sample morning Monday",
      cex.main = 1.5,   font.main= 4, col.main= "black",
)

#decide time window on weekends
weekend_afternoon <- subset(data1, Date == "2009-12-05"& (format(Time,"%H:%M:%S") >= '14:07:00') & (format(Time,"%H:%M:%S") <= '23:59:00'))
weekend_morning <- subset(data5, Date == "2009-12-05" & (format(Time,"%H:%M:%S") >= '08:00:00') & (format(Time,"%H:%M:%S") <= '11:59:00'))


plot(weekend_afternoon$Time, weekend_afternoon$Global_intensity)
title(main = "Global intensity sample afternoon Saturday",
      cex.main = 1.5,   font.main= 3, col.main= "black",
)

plot(weekend_morning$Time, weekend_morning$Global_intensity)
title(main = "Global intensity sample morning Saturday",
      cex.main = 1.5,   font.main= 3, col.main= "black",
)



#choose a weekday
weekday1 <- subset(data1, Date == "2009-12-01")
weekday2 <- subset(data2, Date == "2009-12-01")
weekday3 <- subset(data3, Date == "2009-12-01")
weekday4 <- subset(data4, Date == "2009-12-01")
weekday5 <- subset(data5, Date == "2009-12-01")

mod <- depmix(list(Global_active_power~1,Voltage~1,Global_reactive_power~1),data=weekday1,nstates=11,family=list(gaussian(),gaussian(),gaussian()), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic1 <-BIC(fitmod)
loglik1 <-logLik(fitmod)

mod <- depmix(list(Global_active_power~1,Voltage~1,Global_reactive_power~1),data=weekday2,nstates=11,family=list(gaussian(),gaussian(),gaussian()), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic2 <-BIC(fitmod)
loglik2 <-logLik(fitmod)

mod <- depmix(list(Global_active_power~1,Voltage~1,Global_reactive_power~1),data=weekday3,nstates=11,family=list(gaussian(),gaussian(),gaussian()), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic3 <-BIC(fitmod)
loglik3 <-logLik(fitmod)

mod <- depmix(list(Global_active_power~1,Voltage~1,Global_reactive_power~1),data=weekday4,nstates=11,family=list(gaussian(),gaussian(),gaussian()), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic4 <-BIC(fitmod)
loglik4 <-logLik(fitmod)

mod <- depmix(list(Global_active_power~1,Voltage~1,Global_reactive_power~1),data=weekday5,nstates=11,family=list(gaussian(),gaussian(),gaussian()), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic5 <-BIC(fitmod)
loglik5 <-logLik(fitmod)


#anomalies for week days
#weekday1
threshold1 <- 3
anomalies1 = c()
for (i in 8:nrow(weekday1)) {
  average = mean(weekday1[(i-7):(i-1),7])
  if(abs(weekday1[i,7]-average)>threshold1){
    anomalies1 = c(anomalies1,format(weekday1[i,7]))
  }
}




#weekday2
anomalies2 = c()
for (i in 8:nrow(weekday2)) {
  average = mean(weekday2[(i-7):(i-1),7])
  if(abs(weekday2[i,7]-average)>threshold1){
    anomalies2 = c(anomalies2,format(weekday2[i,7]))
  }
}
#weekday3
anomalies3 = c()
for (i in 8:nrow(weekday3)) {
  average = mean(weekday3[(i-7):(i-1),7])
  if(abs(weekday3[i,7]-average)>threshold1){
    anomalies3 = c(anomalies3,format(weekday3[i,7]))
  }
}
#weekday4
anomalies4 = c()
for (i in 8:nrow(weekday4)) {
  average = mean(weekday4[(i-7):(i-1),7])
  if(abs(weekday4[i,7]-average)>threshold1){
    anomalies4 = c(anomalies4,format(weekday4[i,7]))
  }
}
#weekday5
anomalies5 = c()
for (i in 8:nrow(weekday5)) {
  average = mean(weekday5[(i-7):(i-1),7])
  if(abs(weekday5[i,7]-average)>threshold1){
    anomalies5 = c(anomalies5,format(weekday5[i,7]))
  }
}

#choose a weekend_day
weekend_day1 <- subset(data1, Date == "2009-12-05" & (format(Time,"%H:%M:%S") >= '14:07:00') & (format(Time,"%H:%M:%S") <= '23:59:00'))
weekend_day2 <- subset(data2, Date == "2009-12-05" & (format(Time,"%H:%M:%S") >= '14:07:00') & (format(Time,"%H:%M:%S") <= '23:59:00'))
weekend_day3 <- subset(data3, Date == "2009-12-05" & (format(Time,"%H:%M:%S") >= '14:07:00') & (format(Time,"%H:%M:%S") <= '23:59:00'))
weekend_day4 <- subset(data4, Date == "2009-12-05" & (format(Time,"%H:%M:%S") >= '14:07:00') & (format(Time,"%H:%M:%S") <= '23:59:00'))
weekend_day5 <- subset(data5, Date == "2009-12-05" & (format(Time,"%H:%M:%S") >= '14:07:00') & (format(Time,"%H:%M:%S") <= '23:59:00'))


mod <- depmix(Global_intensity~1,data=weekend_day1,nstates=20,family=gaussian(), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic_weekend1 <-BIC(fitmod)
loglik_weekend1 <-logLik(fitmod)

mod <- depmix(Global_intensity~1,data=weekend_day2,nstates=20,family=gaussian(), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic_weekend2 <-BIC(fitmod)
loglik_weekend2 <-logLik(fitmod)

mod <- depmix(Global_intensity~1,data=weekend_day3,nstates=20,family=gaussian(), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic_weekend3 <-BIC(fitmod)
loglik_weekend3 <-logLik(fitmod)

mod <- depmix(Global_intensity~1,data=weekend_day4,nstates=20,family=gaussian(), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic_weekend4 <-BIC(fitmod)
loglik_weekend4 <-logLik(fitmod)

mod <- depmix(Global_intensity~1,data=weekend_day5,nstates=20,family=gaussian(), ntimes=c(197,197,199))
fitmod <- fit(mod)
#summary(mod)
bic_weekend5 <-BIC(fitmod)
loglik_weekend5 <-logLik(fitmod)



#anomalies for weekend_days
#weekend_day1

threshold2 <- 0.75
anomalies_weekend_day1 = c()
for (i in 8:nrow(weekend_day1)) {
  average = mean(weekend_day1[(i-7):(i-1),6])
  if(abs(weekend_day1[i,6]-average)>threshold2){
    anomalies_weekend_day1 = c(anomalies_weekend_day1,format(weekend_day1[i,6]))
  }
}
#weekend_day2
anomalies_weekend_day2 = c()
for (i in 8:nrow(weekend_day2)) {
  average = mean(weekend_day2[(i-7):(i-1),6])
  if(abs(weekend_day2[i,6]-average)>threshold2){
    anomalies_weekend_day2 = c(anomalies_weekend_day2,format(weekend_day2[i,6]))
  }
}
#weekend_day3
anomalies_weekend_day3 = c()
for (i in 8:nrow(weekend_day3)) {
  average = mean(weekend_day3[(i-7):(i-1),6])
  if(abs(weekend_day3[i,6]-average)>threshold2){
    anomalies_weekend_day3 = c(anomalies_weekend_day3,format(weekend_day3[i,6]))
  }
}
#weekend_day4
anomalies_weekend_day4 = c()
for (i in 8:nrow(weekend_day4)) {
  average = mean(weekend_day4[(i-7):(i-1),6])
  if(abs(weekend_day4[i,6]-average)>threshold2){
    anomalies_weekend_day4 = c(anomalies_weekend_day4,format(weekend_day4[i,6]))
  }
}
#weekend_day5
anomalies_weekend_day5 = c()
for (i in 8:nrow(weekend_day5)) {
  average = mean(weekend_day5[(i-7):(i-1),6])
  if(abs(weekend_day5[i,6]-average)>threshold2){
    anomalies_weekend_day5 = c(anomalies_weekend_day5,format(weekend_day5[i,6]))
  }
}


#multivariate
multi_train_Mod_Sat <- vector("numeric", 17)
multi_train_logLik_Sat <- vector("numeric", 17)
multi_test_Mod_Sat <- vector("numeric", 17)
multi_test_logLik_Sat <- vector("numeric", 17)

for(i in 4:16){
  mod <- depmix(list(Global_active_power~1,Voltage~1,Global_reactive_power~1),data=train_Sat,nstates=i,family=list(gaussian(),gaussian(),gaussian()), ntimes=c(10953,10953,10953))
  fitmod <- fit(mod)
  summary(mod)
  multi_train_Mod_Sat[i-3]<-BIC(fitmod)
  multi_train_logLik_Sat[i-3]<-logLik(fitmod)
  
  modNew <- depmix(list(Global_active_power~1,Voltage~1,Global_reactive_power~1),data=test_Sat,nstates=i,family=list(gaussian(),gaussian(),gaussian()), ntimes=c(2843,2843,2844))
  modNew <- setpars(modNew,getpars(fitmod))
  fitfm <- fit(modNew)
  multi_test_Mod_Sat[i-3]<-BIC(fitfm)
  multi_test_logLik_Sat[i-3]<-logLik(fitfm)
}







