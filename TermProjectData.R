library("ggplot2")
library("zoo")
library("devtools")
library("lubridate")
library("caTools")
library("dplyr")
library("EnvStats")
library("modeest")
library("pracma")
raw_date = read.csv("C:/Users/16047/Desktop/CMPT318 Term Project/TermProjectData.txt", header = TRUE)
#Format dates
raw_date$Date <- as.POSIXct(raw_date$Date, format = "%d/%m/%Y")
#Create new column that displays weekday (0:6 where 0 == Sunday)
raw_date$Day <- as.POSIXlt(raw_date$Date)$wday
#Set Time structure using POSIXlt
raw_date$Time <- as.POSIXct(raw_date$Time, format = "%H:%M:%S")
raw <- df[format(raw_date$Date,'%Y') != "2006" & format(raw_date$Date,'%Y') != "2009", ]
for(i in 1:ncol(raw)) {
  raw[is.na(raw[,i]), i] <- mean(raw[,i], na.rm = TRUE)
}


#PLOTTING Global_active_power AVERAGE FOR EACH MINUTE IN THREE YEARS
timeAvg<-aggregate(raw$Global_active_power, by = list(raw$Time), mean)
colnames(timeAvg) <- c("Time", "avgGlobal_active_power")
ggplot(timeAvg, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()+geom_line()

#PLOTTING Global_reactive_power AVERAGE FOR EACH MINUTE IN THREE YEARS
timeAvg<-aggregate(raw$Global_reactive_power, by = list(raw$Time), mean)
colnames(timeAvg) <- c("Time", "avgGlobal_reactive_power")
ggplot(timeAvg, aes(x = Time, y = avgGlobal_reactive_power))+geom_point()+geom_smooth()+geom_line()

#PLOTTING Voltage AVERAGE FOR EACH MINUTE IN THREE YEARS
timeAvg<-aggregate(raw$Voltage, by = list(raw$Time), mean)
colnames(timeAvg) <- c("Time", "avgVoltage")
ggplot(timeAvg, aes(x = Time, y = avgVoltage))+geom_point()+geom_smooth()+geom_line()

#PLOTTING Voltage AVERAGE FOR EACH MINUTE IN THREE YEARS
timeAvg<-aggregate(raw$Global_intensity, by =list(raw$Time), mean)
colnames(timeAvg) <- c("Time", "avgGlobal_intensity")
ggplot(timeAvg, aes(x = Time, y = avgGlobal_intensity))+geom_point()+geom_smooth()+geom_line()


#MORNINGS (06:00 - 12:00)
morning <- raw[hour(raw$Time) >= 6 & hour(raw$Time) < 10,]

#MORNING Global_active_power AVERAGED FOR EACH DATE
morning_avgDate<-aggregate(morning$Global_active_power, by = list(morning$Date), mean)
colnames(morning_avgDate) <- c("Date", "avgGlobal_active_power")
ggplot(morning_avgDate, aes(x = Date, y = avgGlobal_active_power))+geom_point()+geom_smooth()

#MORNING Global_reactive_power AVERAGE FOR EACH DATE
morning_avgDate<-aggregate(morning$Global_reactive_power, by = list(morning$Date), mean)
colnames(morning_avgDate) <- c("Date", "avgGlobal_reactive_power")
ggplot(morning_avgDate, aes(x = Date, y = avgGlobal_reactive_power))+geom_point()+geom_smooth()

#MORNING Voltage AVERAGE FOR EACH DATE
morning_avgDate<-aggregate(morning$Voltage, by = list(morning$Date), mean)
colnames(morning_avgDate) <- c("Date", "avgVoltage")
ggplot(morning_avgDate, aes(x = Date, y = avgVoltage))+geom_point()+geom_smooth()

#MORNING Global_intensity AVERAGE FOR EACH DATE
morning_avgDate<-aggregate(morning$Global_intensity, by =list(morning$Date), mean)
colnames(morning_avgDate) <- c("Date", "avgGlobal_intensity")
ggplot(morning_avgDate, aes(x = Date, y = avgGlobal_intensity))+geom_point()+geom_smooth()

#NIGHT (13:00 - 24:00)
night <- raw[hour(raw$Time) >= 13 & hour(raw$Time) < 24,]

#NIGHT Global_active_power AVERAGE FOR EACH DATE
night_avgDate<-aggregate(night$Global_active_power, by = list(night$Date), mean)
colnames(night_avgDate) <- c("Date", "avgGlobal_active_power")
ggplot(night_avgDate, aes(x = Date, y = avgGlobal_active_power))+geom_point()+geom_smooth()

#NIGHT Global_reactive_power AVERAGE FOR EACH DATE
night_avgDate<-aggregate(night$Global_reactive_power, by = list(night$Date), mean)
colnames(night_avgDate) <- c("Date", "avgGlobal_reactive_power")
ggplot(night_avgDate, aes(x = Date, y = avgGlobal_reactive_power))+geom_point()+geom_smooth()

#NIGHT Voltage AVERAGE FOR EACH DATE
night_avgDate<-aggregate(night$Voltage, by = list(night$Date), mean)
colnames(night_avgDate) <- c("Date", "avgVoltage")
ggplot(night_avgDate, aes(x = Date, y = avgVoltage))+geom_point()+geom_smooth()

#NIGHT Global_intensity AVERAGE FOR EACH DATE
night_avgDate<-aggregate(night$Global_intensity, by =list(night$Date), mean)
colnames(night_avgDate) <- c("Date", "avgGlobal_intensity")
ggplot(night_avgDate, aes(x = Date, y = avgGlobal_intensity))+geom_point()+geom_smooth()

data = raw_data
startTime = "10:00:00"
endTime = "13:00:00"
dayOfWeek = "3"
data$Date = as.Date(data$Date, format = "%d/%m/%Y") #Format dates
data$Global_active_power = scale(data$Global_active_power) #Scale desired variable
data$Global_reactive_power = scale(data$Global_reactive_power)
data$Voltage = scale(data$Voltage)
data$Global_intensity = scale(data$Global_intensity)
data = data[data$Time >= startTime,] #Cut times before start time
data = data[data$Time <= endTime,] #Cut times after end time
data = data[wday(data$Date) == 1,] #Cut days not on chosen day
raw_data[is.na(raw_data)] = 0
raw_data.pca = prcomp(raw_data[,c(3:9)], center = TRUE, scale. = TRUE)
summary(raw_data.pca)
str(raw_data.pca)
ggbiplot(raw_data.pca)
pca.var = pca$sdev^2 #Calculate variance of principal components
pca.var.per = round(pca.var/sum(pca.var)*100, 1) #Convert variance to percentages
barplot(pca.var.per, main="PCA Component Contributions Percentages", xlab = "Principal Component", ylab = "Percent Variation") #plot variances
loadingScores = raw_data.pca$rotation[,1] #get loading scores of principal components
componentRanks = sort(abs(loadingScores), decreasing = TRUE) #sort the absolute value of principal components to find the amount that each feature contributes to variance
componentRanks

str(raw_data.pca)
ggbiplot(standardizeData.pca) # show the PCA graph 
trainData = data[c(1, 3, 6)] # data only contain Global_active_power AND Global_intensity with DATE
testData = data[c(1, 3, 6)]

trainData = trainData[trainData$Date >= "2006-12-16",]
trainData = trainData[trainData$Date <= "2008-12-31",]
trainData # train-data
testData = testData[testData$Date >= "2009-01-01",]
testData = testData[testData$Date <= "2009-12-31",]
testData # test-data

data = data[c(3,6)]
timelength = 3 * 60 +1 # 3 hours 
times = rep(timelength, nrow(trainData)/timelength)
set.seed(2)
BIC_list = vector() # vector 
LogLike_list = vector() # vector 
# test which one is the best model 
for(i in 4:24){
  model = depmix(list(Global_active_power~1,Global_intensity~1), data = trainData, nstates = i,
                 family = list(gaussian(), gaussian()), ntimes = times)
  fm = fit(model)
  bic = BIC(fm)
  append(BIC_List, bic)
  logLik = logLik(fm)
  append(LogLike_List, logLik)
}
#normalizing 
logNorm = scale(LogLike_list)
bicNorm = scale(BIC_list) 
x = seq(4, 16, 1) 
plot(x,logNorm,type="l",main ="BIC vs Log-Likelihood",col="red", xlab = "States", ylab = "Value") 
lines(x,bicNorm,col="blue")

times = rep(timelength, nrow(testData)/timelength)
model = depmix(list(Global_active_power~1,Global_intensity~1), data = testData, nstates = 9,
               family = list(gaussian(), gaussian()), ntimes = times) 
fm = fit(model)
bic = BIC(fm)
logLik = logLik(fm)


# --------Anomaly Data 1------------

AnomalyData1 = read.csv("C:/Users/16047/Desktop/CMPT318 Term Project/DataWithAnomalies1.txt", header = TRUE)

AnomalyData1$Date = as.Date(AnomalyData1$Date, format = "%d/%m/%Y") #Format dates
AnomalyData1$Global_active_power = scale(AnomalyData1$Global_active_power) #Scale desired variable
AnomalyData1$Global_intensity = scale(AnomalyData1$Global_intensity) #Scale desired variable
AnomalyData1 = AnomalyData1[AnomalyData1$Time >= startTime,] #Cut times before start time
AnomalyData1 = AnomalyData1[AnomalyData1$Time <= endTime,] #Cut times after end time
AnomalyData1 = AnomalyData1[wday(AnomalyData1$Date) == 1,] #Cut days not on chosen day
AnomalyData1 # all the Wednesday with time 10:00 to 13:00

trainAnomalyData1 = AnomalyData1[c(1, 3, 6)] # data only contain Global_active_power AND Global_intensity with DATE
testAnomalyData1 = AnomalyData1[c(1, 3, 6)]

trainAnomalyData1 = trainAnomalyData1[trainAnomalyData1$Date >= "2009-12-01",]
trainAnomalyData1 = trainAnomalyData1[trainAnomalyData1$Date <= "2010-09-01",]
trainAnomalyData1 # train-data
testAnomalyData1 = testAnomalyData1[testAnomalyData1$Date >= "2010-09-01",]
testAnomalyData1 = testAnomalyData1[testAnomalyData1$Date <= "2010-11-26",]
testAnomalyData1 # test-data

times = rep(timelength, nrow(trainAnomalyData1)/timelength)
set.seed(2)
model = depmix(list(Global_active_power~1,Global_intensity~1), data = trainAnomalyData1, nstates = 9,
               family = list(gaussian(), gaussian()), ntimes = times)   # we think it's the best model  
fm2 = fit(model)
logLik = logLik(fm2)

times = rep(timelength, nrow(testAnomalyData1)/timelength)
model = depmix(list(Global_active_power~1,Global_intensity~1), data = testAnomalyData1, nstates = 9,
               family = list(gaussian(), gaussian()), ntimes = times)
fm3 = fit(model)
logLik = logLik(fm3)



# --------Anomaly Data 2------------

AnomalyData2 = read.csv("C:/Users/16047/Desktop/CMPT318 Term Project/DataWithAnomalies2.txt", header = TRUE)

AnomalyData2$Date = as.Date(AnomalyData2$Date, format = "%d/%m/%Y") #Format dates
AnomalyData2$Global_active_power = scale(AnomalyData2$Global_active_power) #Scale desired variable
AnomalyData2$Global_intensity = scale(AnomalyData2$Global_intensity) #Scale desired variable
AnomalyData2 = AnomalyData2[AnomalyData2$Time >= startTime,] #Cut times before start time
AnomalyData2 = AnomalyData2[AnomalyData2$Time <= endTime,] #Cut times after end time
AnomalyData2 = AnomalyData2[wday(AnomalyData2$Date) == 1,] #Cut days not on chosen day
AnomalyData2 # all the Wednesday with time 10:00 to 13:00

trainAnomalyData2 = AnomalyData2[c(1, 3, 6)] # data only contain Global_active_power AND Global_intensity with DATE
testAnomalyData2 = AnomalyData2[c(1, 3, 6)]

trainAnomalyData2 = trainAnomalyData2[trainAnomalyData2$Date >= "2009-12-01",]
trainAnomalyData2 = trainAnomalyData2[trainAnomalyData2$Date <= "2010-09-01",]
trainAnomalyData2 # train-data
testAnomalyData2 = testAnomalyData2[testAnomalyData2$Date >= "2010-09-01",]
testAnomalyData2 = testAnomalyData2[testAnomalyData2$Date <= "2010-11-26",]
testAnomalyData2 # test-data

times = rep(timelength, nrow(trainAnomalyData2)/timelength)
set.seed(2)
model = depmix(list(Global_active_power~1,Global_intensity~1), data = trainAnomalyData2, nstates = 9,
               family = list(gaussian(), gaussian()), ntimes = times)   # we think it's the best model  
fm4 = fit(model)
logLik = logLik(fm4)

times = rep(timelength, nrow(testAnomalyData2)/timelength)
model = depmix(list(Global_active_power~1,Global_intensity~1), data = testAnomalyData2, nstates = 9,
               family = list(gaussian(), gaussian()), ntimes = times)
fm5 = fit(model)
logLik = logLik(fm5)


# --------Anomaly Data 3------------

AnomalyData3 = read.csv("C:/Users/16047/Desktop/CMPT318 Term Project/DataWithAnomalies3.txt", header = TRUE)

AnomalyData3$Date = as.Date(AnomalyData3$Date, format = "%d/%m/%Y") #Format dates
AnomalyData3$Global_active_power = scale(AnomalyData3$Global_active_power) #Scale desired variable
AnomalyData3$Global_intensity = scale(AnomalyData3$Global_intensity) #Scale desired variable
AnomalyData3 = AnomalyData3[AnomalyData3$Time >= startTime,] #Cut times before start time
AnomalyData3 = AnomalyData3[AnomalyData3$Time <= endTime,] #Cut times after end time
AnomalyData3 = AnomalyData3[wday(AnomalyData3$Date) == 1,] #Cut days not on chosen day
AnomalyData3 # all the Wednesday with time 10:00 to 13:00

trainAnomalyData3 = AnomalyData3[c(1, 3, 6)] # data only contain Global_active_power AND Global_intensity with DATE
testAnomalyData3 = AnomalyData3[c(1, 3, 6)]

trainAnomalyData3 = trainAnomalyData3[trainAnomalyData2$Date >= "2009-12-01",]
trainAnomalyData3 = trainAnomalyData3[trainAnomalyData2$Date <= "2010-09-01",]
trainAnomalyData3 # train-data
testAnomalyData3 = testAnomalyData3[testAnomalyData3$Date >= "2010-09-01",]
testAnomalyData3 = testAnomalyData3[testAnomalyData3$Date <= "2010-11-26",]
testAnomalyData3 # test-data

times = rep(timelength, nrow(trainAnomalyData3)/timelength)
set.seed(2)
model = depmix(list(Global_active_power~1,Global_intensity~1), data = trainAnomalyData3, nstates = 9,
               family = list(gaussian(), gaussian()), ntimes = times)   # we think it's the best model  
fm6 = fit(model)
logLik = logLik(fm6)

times = rep(timelength, nrow(testAnomalyData3)/timelength)
model = depmix(list(Global_active_power~1,Global_intensity~1), data = testAnomalyData3, nstates = 9,
               family = list(gaussian(), gaussian()), ntimes = times)
fm7 = fit(model)
logLik = logLik(fm7)
