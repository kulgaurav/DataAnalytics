setwd("~/DataAnalyticsProject/DataAnalytics")

data <- read.csv(file = 'Data_For_KNN_Impute.csv', header = TRUE, sep=',')

str(data) #Structure of object 'data'

head(data)

summary(data) #will also list number of NA's along with quartiles

library('VIM') #For Visualization and Imputaion of Missing Values
#VIM: provides kNN: k-Nearest Neighbour Imputation

#kNN(Data, variable= "Genre", k=5)
#--- Data: data.frame or matrix
#--- variable: variables were missing values should be imputed
#-------- can use 'c' to combine column names to get as as a vector or list
#--- k: Number of nearest neighbours used


#Imputing using other columns of data
imputeOneCOlumn <- kNN(data, variable = 'Genre', k = 5)
summary(imputeOneCOlumn)


#Impute multiple continuous  variables
imputeMultCont <- kNN(data, variable = c('Profitability','Rotten.Tomatoes..'), k =6)
summary(imputeMultCont)
