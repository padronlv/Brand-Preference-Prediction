#install.packages("gdata")
#install.packages("readxl")
#install.packages ("arules")
#install.packages("rpart")
#install.packages("rattle")
setwd("C:/Users/VPL/Desktop/Ubiqum - Data Analysis/Course 2/Task 2")
#Load Libraries
library(readxl)
library(readr)
library(ggplot2)
library(caret)
library(corrplot)
library(arules)
library(rattle)

#----------------------------------------------------------------------------------
#import data
data_survey<- read_excel("Survey_Key_and_Complete_Responses_excel.xlsx",sheet = 2)
data_survey_firstpart <- data_survey
data_survey_incomplete <- read.csv("SurveyIncomplete.csv")

#----------------------------------------------------------------------------------
#Data exploration and tuning.
summary(data_survey)
data_survey$brand <- factor(data_survey$brand)
levels(data_survey$brand) <- c("ACER", "SONY")
data_survey$elevel <- factor(data_survey$elevel)
data_survey$car <- factor(data_survey$car)
data_survey$zipcode <- factor(data_survey$zipcode)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_survey$salary <- normalize(data_survey$salary)
data_survey$credit <- normalize(data_survey$credit)
data_survey$age <- normalize(data_survey$age)

# data_survey_joined$age <- discretize(data_survey_joined$age, method = "interval", categories = 3, labels = NULL, ordered = F,
#                        onlycuts = F)

data_survey_Sony <- data_survey[which(data_survey$brand == "SONY"), ]
data_survey_Acer <- data_survey[which(data_survey$brand == "ACER"), ]
hist(data_survey_Sony$age)
hist(data_survey_Acer$age)
hist(data_survey_Sony$salary)
hist(data_survey_Acer$salary)
hist(data_survey_Sony$credit)
hist(data_survey_Acer$credit)

summary(data_survey)
plot(data_survey$brand,data_survey$age)
#qqnorm(data_survey$elevel)

#----------------------------------------------------------------------------------
#seed
set.seed(123)


#----------------------------------------------------------------------------------
#Datapartition
Data_Partition <- createDataPartition(data_survey$brand, p = .75, list = FALSE)
training <- data_survey[Data_Partition,]
testing <- data_survey[-Data_Partition,]

#----------------------------------------------------------------------------------
#10 fold cross validation
Control_RepeatedCV <- trainControl(method = "repeatedcv", number = 4, repeats = 2)
#fitControl <- trainControl(method = "cv", number = 10)

#----------------------------------------------------------------------------------
#train knn 
KNNBrands <- train(brand~salary + age, data = training, method = "knn", trControl=Control_RepeatedCV, tuneLength = 30)
KNNBrands

#train RFBrands
RFBrands <- train(brand~., data = training, method = "rpart", trControl=Control_RepeatedCV, tuneLength = 5)
RFBrands

plot(RFBrands$finalModel)
text(RFBrands$finalModel)


#predictor variables
predictors(KNNBrands)

#make predictions
testPredKNNBrands <- predict(KNNBrands, testing)

#performace measurment
postResample(testPredKNNBrands, testing$brand)

#plot predicted verses actual
plot(testPredKNNBrands,testing$brand, main="PREDICTION VS REALITY",
     xlab="PREDICTION", ylab="REALITY")

#----------------------------------------------------------------------------------
#prediction in the incomplete table
data_survey_incomplete$brand <- predict(KNNBrands, data_survey_incomplete)

#join tables
data_survey_joined <- rbind(data_survey_firstpart, data_survey_incomplete)

#----------------------------------------------------------------------------------
#bussines analysis and plots
data_survey_joined$age <- discretize(data_survey_joined$age, method = "interval", categories = 3, labels = NULL, ordered = F,
                       onlycuts = F)

data_survey_joined$brand <- factor(data_survey_joined$brand, levels = c(0,1), labels = c("Acer", "Sony"))

ggplot(data= data_survey_joined, aes(x=brand,y=salary))+geom_point()+geom_jitter()
ggplot(data= data_survey_joined, aes(x=brand,y=age))+geom_point()+geom_jitter()
ggplot(data= data_survey_joined, aes(x=brand,y=elevel))+geom_point()+geom_jitter()
ggplot(data= data_survey_joined, aes(x=brand,y=car))+geom_point()+geom_jitter()
ggplot(data= data_survey_joined, aes(x=brand,y=credit))+geom_point()+geom_jitter()
ggplot(data= data_survey_joined, aes(x=brand,y=zipcode))+geom_point()+geom_jitter()

ggplot(data= data_survey_joined, aes(x=brand, color =brand))+geom_bar()

ggplot(data= data_survey_joined, aes(x=age,fill=brand))+geom_bar()
ggplot(data= data_survey_joined, aes(x=salary,fill=brand))+geom_bar()
ggplot(data= data_survey_joined, aes(x=zipcode,y=salary,color=brand))+geom_point()+geom_jitter()
ggplot(data= data_survey_joined, aes(x=age,y=salary,color=brand))+geom_point()+geom_jitter()

