# Remove Lists
rm(list = ls())

# Set Working Directory
setwd("D:/bike")

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
#
# # Split 'dteday' column into new Day column
bike_data <- separate(data = bike_data,col = "dteday",into = c("year","month","day"))
bike_data$year <- NULL
bike_data$month <- NULL

## Convert day to int from
bike_data$day <- as.numeric(as.character(bike_data$day))
#
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
#gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
#gridExtra::grid.arrange(gn13,gn14,gn16,ncol=3)
gridExtra::grid.arrange(gn11,gn12,ncol=2)

# hum and windspeed have outliers, Lets remove those values
outlier_val <- c('hum' , 'windspeed')
#
for(i in outlier_val){
  val = bike_data[,i][bike_data[,i] %in% boxplot.stats(bike_data[,i])$out]
  #print(length(val))
  bike_data[,i][bike_data[,i] %in% val] = NA
}
#
bike_data$hum[is.na(bike_data$hum)] = mean(bike_data$hum, na.rm = T)
bike_data$windspeed[is.na(bike_data$windspeed)] = mean(bike_data$windspeed, na.rm = T)
#

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

# Correlation graph
library(corrgram)
corrgram(bike_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
vifcor(bike_data[,-16], th = 0.9)

# Lets make towards Model
# Drop some nouse variable
#df <- bike_data[, -c(1,11,14:15)]
df <- bike_data[, -c(1:2,11,14:15)]

vifcor(df[-11], th = 0.9)

vif(df)

# Linear Regression
lm_model = lm(cnt ~., data = df)

summary(lm_model)

# p  0.05 Accept NULL Hypothesis and can say that this variable not relevant to us

predictions_LR = predict(lm_model, df[,1:10])
library(MLmetrics)
MAPE(df[,11], predictions_LR)

# 83%

write.csv(predictions_LR, file = 'D:/bike/countpredictLR.csv')
# Decision Tree Regression

n = nrow(df)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = df[trainIndex ,]
test = df[-trainIndex ,]


# r part for decision tree regreession

fit = rpart(cnt ~ ., data = train, method = 'anova')

# predictions = predict(fit, test[,-12])
predictions_TR = predict(fit, test[,-11])
library(MLmetrics)



MAPE(test[,11], predictions_TR)
# 81%
write.csv(predictions_TR, file = 'D:/bike/countpredictTR.csv')
#############################################END##################################################



