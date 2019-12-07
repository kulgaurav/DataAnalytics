#Assumption: Missing data are MAR: Missing At Random
#--> Missing is completely at random for each categories of another variable
#--> Probability a variable is missing depends only on available information
#--> For instance, if Xij is missing => X1..Xn can be used for the prediction of missing.

library(mice) #Multivariate Imputation via Chained Equations

library(VIM)


data <- read.csv(file = "Project Data.csv")
summary(data)

md.pattern(data)

data_plot <- aggr(data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


#method: method to be used for imputing
#--> pmm: Predictive Mean Matching: For continuous variables 
#--> logreg: Logistic Regresssion: For binary variables
#m: m imputed data sets
#maxit: max no. of iterations taken to impute

#TODO: pmm and logreg distribution 
imputed_Data <- mice(data, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

#Selecting a data set out of m generated
fullData <- complete(imputed_Data, 2)
#TODO: combine results from m sets


write.csv(fullData, file = "ImputedData.csv")
