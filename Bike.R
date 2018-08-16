# Remove Lists
rm(list = ls())

# Set Working Directory
setwd("D:/Bike Renting")

# Get Working Directory
getwd()

# Load Data
bike_data <- read.csv("day.csv", header = T)

# Load Library
library(tidyr)
library(plyr)
library(psych)
library(ggplot2)
library(rpart)
library(usdm)
library(rpart)
library(MASS)
library(MLmetrics)
library(DMwR)

# Explore Data
str(bike_data)

# Missing Value Analysis
sum(is.na(bike_data))

# Split 'dteday' column into new Day column
bike_data <- separate(data = bike_data,col = "dteday",into = c("year","month","day"))
bike_data$year <- NULL
bike_data$month <- NULL

## Convert day to int from 
bike_data$day <- as.numeric(as.character(bike_data$day))

#Multi Histogram for finding out Outlier

multi.hist(bike_data$temp, main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")

multi.hist(bike_data$atemp, main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")

multi.hist(bike_data$hum, main = NA, dcol = c("blue", "red"),  # Outlier
           dlty = c("solid", "solid"), bcol = "linen")

multi.hist(bike_data$windspeed, main = NA, dcol = c("blue", "red"),   # Outlier
           dlty = c("solid", "solid"), bcol = "linen")

multi.hist(bike_data$cnt, main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")

# We can see some skewness in 'hum'  and  'windspeed'
y = table(bike_data$yr)
# Plot BoxPlot For them and lets go with Outlier Analysis

numeric_index = sapply(bike_data,is.numeric)
numeric_data = bike_data[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of cnt for",cnames[i])))
}
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn16,ncol=3)

# hum and windspeed have outliers, Lets remove those values

val_hum = bike_data$hum[bike_data$hum %in% boxplot.stats(bike_data$hum)$out]
val_windspeed = bike_data$windspeed[bike_data$windspeed %in% boxplot.stats(bike_data$windspeed)$out]
val_holiday = bike_data$holiday[bike_data$holiday %in% boxplot.stats(bike_data$holiday)$out]

## Remove
bike_data = bike_data[which(!bike_data$hum %in% val_hum),]
bike_data = bike_data[which(!bike_data$windspeed %in% val_windspeed),]
bike_data = bike_data[which(!bike_data$holiday %in% val_holiday),]

##
boxplot(bike_data$hum)
multi.hist(bike_data$hum, main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")

boxplot(bike_data$windspeed)
multi.hist(bike_data$windspeed, main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")

#Visualization

##Int value
ggplot(bike_data, aes(x = bike_data$day, y = bike_data$cnt )) + geom_col(show.legend = TRUE)
ggplot(bike_data, aes(x = bike_data$season, y = bike_data$cnt )) + geom_col(show.legend = TRUE)
ggplot(bike_data, aes(x = bike_data$yr, y = bike_data$cnt )) + geom_col(show.legend = TRUE )
ggplot(bike_data, aes(x = bike_data$mnth, y = bike_data$cnt )) + geom_col(show.legend = TRUE )
ggplot(bike_data, aes(x = bike_data$holiday, y = bike_data$cnt )) + geom_col(show.legend = TRUE )
ggplot(bike_data, aes(x = bike_data$weekday, y = bike_data$cnt )) + geom_col(show.legend = TRUE )
ggplot(bike_data, aes(x = bike_data$workingday, y = bike_data$cnt )) + geom_col(show.legend = TRUE )
ggplot(bike_data, aes(x = bike_data$weathersit, y = bike_data$cnt )) + geom_col(show.legend = TRUE )

##Numeric Value
ggplot(bike_data, aes(x = bike_data$temp, y = bike_data$cnt )) + geom_jitter() + geom_smooth(method = loess, formula = y ~ x)

ggplot(bike_data, aes(x = bike_data$atemp, y = bike_data$cnt )) + geom_jitter() + geom_smooth(method = loess, formula = y ~ x)

ggplot(bike_data, aes(x = bike_data$hum, y = bike_data$cnt )) + geom_jitter() + geom_smooth(method = loess, formula = y ~ x)

ggplot(bike_data, aes(x = bike_data$windspeed, y = bike_data$cnt )) + geom_jitter() + geom_smooth(method = loess, formula = y ~ x)

##Lets make towards Model
# Drop some nouse variable
df <- bike_data[, -c(1,14:15)]

vifcor(df[-13], th = 0.9)
df$atemp <- NULL # Remove this highly collinear one
vif(df)

# Linear Regression
lm_model = lm(cnt ~., data = df)

summary(lm_model)

# p  0.05 Accept NULL Hypothesis and can say that this variable not relevant to us

predictions_LR = predict(lm_model, df[,1:11])
library(MLmetrics)
MAPE(df[,12], predictions_LR)
# 83%
# Decision Tree Regression

n = nrow(df)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
#train_index = sample(1:nrow(bike_data), 0.8 = nrow(bike_data))
train = df[trainIndex ,]
test = df[-trainIndex ,]


# r part for decision tree regreession

fit = rpart(cnt ~ ., data = train, method = 'anova')

# predictions = predict(fit, test[,-12])
predictions_LR = predict(fit, test[,-12])
library(MLmetrics)

mape = function(y, yhat){
  mean(abs((y - yhat)/y)) * 100
}


mape(test[,12], predictions_LR)
# 81%
#############################################

library(DMwR)
regr.eval(test[,11], predictions_LR, stats = c('mae', 'mape', 'rmse', 'mse'))

regr.eval(test[,11], predictions_LR, stats = c('mse'))

