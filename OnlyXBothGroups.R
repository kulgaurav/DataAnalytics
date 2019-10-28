library(partykit)
library(rpart)
library(sqldf)

setwd("C:\\Users\\Kul\\Downloads\\18 oct\\Classes\\Data Analytics")

data <- read.csv("Project Data.csv", header=TRUE, sep=",")
str(data)
#attach(data)

data_new <-  sqldf('select Response,X1,X2,X3,X4,X5,X6,X7 from data')
str(data_new)
trainIdx <- sample(1:nrow(data_new), 0.8*nrow(data_new))
train <- data_new[trainIdx,]
test <- data[-trainIdx,]

d_model_1 <- rpart(train$Response ~ . , data = train, method = 'class',  control=rpart.control(minsplit=5, minbucket=5, maxdepth=8))
plot(as.party(d_model_1))
print(d_model_1)


opt <- which.min(d_model_1$cptable [, "xerror"])
cp <- d_model_1$cptable [opt,"CP"]
DT_Model_1_pruned <- prune(d_model_1, cp=cp)
plot(as.party(DT_Model_1_pruned))



t_predict = predict(DT_Model_1_pruned, test, type = 'class')
#print(t_predict)
#t_predict <- ifelse(t_predict==1,0,1)
t_given = data_new['Response']
confMat <- table(test$Response,t_predict)
#print(confMat)
accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy)