#Clear Global Environment
rm(list = ls())

#Load all required packages
library(ggplot2)
library(DMwR)
library(corrgram)
library(DataCombine)
library(rpart)
library(randomForest)
library(inTrees)
#library(usdm)
library(caret)
library(lubridate)


#Set Working Directory
setwd("C:/Users/Kiran/Desktop/Project - 1")

#Load Data
Data = read.csv('day.csv', header = T, sep =",")

#Drop First column - instant - does not contribute to data analysis
BikeRent = Data[,-1]

##########Exploratory Data Analysis##########
#Look at the actual structure of the data
str(BikeRent)

#Consider following variables and convert them into Factor
Cat_Var = c('season', 'yr', 'mnth', 'holiday', 'weekday',
            'workingday', 'weathersit')

#"dteday" would be treated separately

#Loop to convert Cat_Var into factor
for (i in Cat_Var) {
  BikeRent[,i] = as.factor(BikeRent[,i])
}

#Save the continuous variables in Con_Var
Con_Var = c('temp', 'atemp', 'hum', 'windspeed', 'casual',
            'registered', 'cnt')

#Loop to convert into numeric
for (i in Con_Var) {
  BikeRent[,i] = as.numeric(BikeRent[,i])
}

str(BikeRent)

##########Missing Value Analysis##########
#Check for existence of missing values if any

sum(is.na(BikeRent))

#No missing values found


##########Outlier Analysis##########
#Make a copy of pre-processed data
BikeRent1 = BikeRent
BikeRent = BikeRent1

#Loop to get grids of boxplots of "cnt" vs. rest of Con_Var
for (i in 1:length(Con_Var))
   {
     assign(paste0("gn",i), ggplot(aes_string(x = "cnt", y = (Con_Var[i])), data = BikeRent)+ 
             stat_boxplot(geom = "errorbar", width = 0.5) +
             geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=20,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(x="cnt", y=Con_Var[i])+
              ggtitle(paste("Box plot of Count for",Con_Var[i])))
   }

###Plotting grids of boxplots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)     #No Outliers
gridExtra::grid.arrange(gn3,gn4,gn5,ncol=3) #Outliers in hum, windspeed & casual
gridExtra::grid.arrange(gn6,gn7,ncol=2)     #No Outliers

#Select variables with outliers
Out_Var = c('hum','windspeed','casual')  #Variables with outliers


#Loop to remove outliers
# for (i in Out_Var) {
#   print(i)
#   val = BikeRent[,i][BikeRent[,i] %in% boxplot.stats(BikeRent[,i])$out]
#   print(length(val))
# # BikeRent = BikeRent[which(!BikeRent[,i] %in% val),]
# 
#  }

#55 observations could be removed

#Loop to impute outliers with NA 
#and replace them with respective mean & median values
for (i in Out_Var) {
  val = BikeRent[,i][BikeRent[,i] %in% boxplot.stats(BikeRent[,i])$out]
  BikeRent[,i][BikeRent[,i] %in% val] = NA
  print(length(val))
#By Mean  
#  BikeRent[,i][is.na(BikeRent[,i])] = mean(BikeRent[,i], na.rm = T)
  
#By Median  
#  BikeRent[,i][is.na(BikeRent[,i])] = median(BikeRent[,i], na.rm = T)
   
  }

#knnImputation method
BikeRent = knnImputation(BikeRent, k = 5)

#Conclusion: knnImputation gives the best results


##########Feature Selection##########
##Feature Engineering

##  1. combine "holiday" & "workingday" to extract "day"
for (i in 1:nrow(BikeRent)) {
  if((BikeRent[i,"holiday"]==0) & (BikeRent[i,"workingday"]==0) ){
    BikeRent[i,"day"] = "weekend"
  } else {
    if((BikeRent[i,"holiday"]==0) & (BikeRent[i,"workingday"]==1) ){
      BikeRent[i,"day"] = "workingday"
    } else {
    if ((BikeRent[i,"holiday"]== 1)) {
      BikeRent[i,"day"] = "holiday"
    }

  }
}
}
 
# #convert "day" into factor type with levels
BikeRent$day = factor(BikeRent$day,
                           levels = c("weekend", "workingday", "holiday"),
                           labels = c(1,2,3))


##2. Extract "week_in_month" form "dteday"

#extract year, month & date from "dteday"
dd = ymd(BikeRent$dteday)

#loop to group dates into "week_in_month"
for (i in 1:nrow(BikeRent)) {
  if(day(dd)[i] <= 7){
    BikeRent[i,"week_in_month"] = "First Week"
  } else {
    if((7<day(dd)[i]) & (day(dd)[i]<=14)){
      BikeRent[i,"week_in_month"] = "Second Week"
    } else {
      if ((14<day(dd)[i]) & (day(dd)[i]<=21)) {
        BikeRent[i,"week_in_month"] = "Third Week"
      }
      else{
        if ((21<day(dd)[i]) & (day(dd)[i]<=28)){
          BikeRent[i,"week_in_month"] = "Fourth Week"
        }
        else{
          if ((28<day(dd)[i]) & (day(dd)[i]<=31)){
            BikeRent[i, "week_in_month"] = "Fifth Week"
          }
        }
      }
      
    }
  } 
}

#convert "week_in_month" into factor type with levels
BikeRent$week_in_month = factor(BikeRent$week_in_month,
                                levels = c("First Week", "Second Week", "Third Week", "Fourth Week", "Fifth Week"),
                                labels = c(1, 2, 3, 4, 5))


##Correlation Analysis
#Correlation plot
corrgram(BikeRent[,Con_Var], order = F, upper.panel = panel.pie,
         text.panel = panel.txt, main = "Correlation Plot")


#Conclusion
#"temp" & "atemp" are highly correlated and
#comparatively "temp" is less correlated with "cnt", so discarding "atemp"
#"casual" & "registered" summation results in "cnt", so as per the problem statement, its enough if we analyse "cnt" w.r.t other variables. Hence discarding "casual" & "registered"


#ANOVA Test
ANO_Test = aov(cnt~ yr + season + mnth + week_in_month + day +
                 weathersit, data = BikeRent)

summary(ANO_Test)

#Conclusion
#reject null hypothesis - all means are not equal, so consider all variables
#alternatively, p-values of all variables are less than 0.05, hence all are considered

#Dimesion Reduction
BikeData = subset(BikeRent, select = -c(dteday, holiday, weekday,
                                    workingday, atemp,
                                    registered, casual))

#Rearrange Columns
BikeData = BikeData[,c(2,1,3,10,9,4,5,6,7,8)]

##Feature Scaling 
#Normalisation of "cnt" ##All other continuous variables are already normalised

BikeData[,"cnt"] = (BikeData[,"cnt"] - min(BikeData[,"cnt"]))/(max(BikeData[,"cnt"])-min(BikeData[,"cnt"]))
  

##########(Regression) Model Development##########
#clean the Environment
rmExcept("BikeData")

#Divide data into train & test
train_index = sample(1:nrow(BikeData), 0.8*nrow(BikeData))
train = BikeData[train_index,]
test = BikeData[-train_index,]
  
  
#### 1. Decision Tree Regression
DT_Reg = rpart(cnt~., data = train, method = "anova" )
#Predict test data 
Predictions_DT = predict(DT_Reg, test[,-10])
#Check error metrics
regr.eval(test[,10], Predictions_DT, stats = c("mae", "mse", "mape", "rmse"))
#Following are the values. these will change each time we run train and test lines of code
#mae = 0.07523593; #mse = 0.01003967
#mape = 0.17539468; #rmse = 0.10019813

#### 2. Random Forest Regression
RF_Reg = randomForest(cnt~., train, ntree = 100)

##Extract rules fromn RF
#transform RF object to an "inTrees" format
treeList = RF2List(RF_Reg)

#Extract rules
Ext_Rules = extractRules(treeList, train[,-10])

#Visualize some rules
Ext_Rules[1:2,]

#Make rules more readable
ReadRules = presentRules(Ext_Rules,colnames(train))
ReadRules[1:2,]

#Get rule metrics
ruleMetric = getRuleMetric(Ext_Rules, train[,-10], train$cnt)

#Evaulate few rules
ruleMetric[1:2,]

#Predict test data
Predictions_RF = predict(RF_Reg, test[,-10])

#Check error metrics
regr.eval(test[,10], Predictions_RF, stats = c("mae", "mse", "mape", "rmse"))
#Following are the values. these will change each time we run train and test lines of code
#mae = 0.04972769; #mse = 0.00445285
#mape = 0.11703345; #rmse = 0.06672968


## 3. Multiple-Linear Regression
Lin_Reg = lm(cnt~ ., data = train)
#summary(Lin_Reg)
#Predict test data
Predictions_LR = predict(Lin_Reg, test[,-10])
#Check error metrics
regr.eval(test[,10], Predictions_LR, stats = c("mae", "mse", "mape", "rmse"))
#Following are the values. these will change each time we run train and test lines of code
#mae = 0.06296960; #mse = 0.00713127
#mape = 0.14784436; #rmse = 0.08444685


## 4. KNN Regression
KNN_Reg = knnreg(train[,1:9], train$cnt, k=7)
#Predict test data
Predictions_KNN = predict(KNN_Reg, test[,-10])
#Check error metrics
regr.eval(test[,10], Predictions_KNN, stats = c("mae", "mse", "mape", "rmse"))
#Following are the values. these will change each time we run train and test lines of code
#mae = 0.064291173; #mse = 0.007082668
#mape = 0.156274075; #rmse = 0.084158586



########## Conclusion ##########
##Random Forest model with number of trees = 100,
#has given the best results w.r.t every error metric.
#So for the given data set and problem statement,
#Random Forest suits the best.