library(partykit)
library(rpart)
library(sqldf)


data <- read.csv("Project Data.csv", header=TRUE, sep=",")
str(data)


#Getting data sets with X, Y, XY
data_all_X <-  sqldf('select Response,X1,X2,X3,X4,X5,X6,X7 from data ')
data_all_Y <-  sqldf('select Response,Y1,Y2,Y3,Y4,Y5,Y6,Y7 from data ')
data_all_XY  <-  sqldf('select Response,X1,X2,X3,X4,X5,X6,X7,Y1,Y2,Y3,Y4,Y5,Y6,Y7 from data ')


#Getting data sets with X, Y, XY and only Group=0 rows
data_0_X <-  sqldf('select Response,X1,X2,X3,X4,X5,X6,X7 from data where "Group" == 0 ')
data_0_Y  <-  sqldf('select Response,Y1,Y2,Y3,Y4,Y5,Y6,Y7 from data where "Group" == 0  ')
data_0_XY  <-  sqldf('select Response,X1,X2,X3,X4,X5,X6,X7,Y1,Y2,Y3,Y4,Y5,Y6,Y7 from data where "Group" == 0 ')


#Getting data sets with X, Y, XY and only Group=1 rows
data_1_X <-  sqldf('select Response,X1,X2,X3,X4,X5,X6,X7 from data where "Group" == 1')
data_1_Y  <-  sqldf('select Response,Y1,Y2,Y3,Y4,Y5,Y6,Y7 from data where "Group" == 1')
data_1_XY  <-  sqldf('select Response,X1,X2,X3,X4,X5,X6,X7,Y1,Y2,Y3,Y4,Y5,Y6,Y7 from data where "Group" == 1 ')




#Adding all kind of data sets to list
all_data_choices <- list(data_all_X, data_all_Y, data_all_XY, data_0_X, data_0_Y, data_0_XY, data_1_X, data_1_Y, data_1_XY)



#Empty list to hold accuracy for each data sets.
acc <- list()
i=1


#Iterating the list for datasets one by one and performing the model and predict
for(data_new in all_data_choices){
  
  #Splitting data in test and train with 20:80 ratio
  trainIdx <- sample(1:nrow(data_new), 0.8*nrow(data_new))
  train <- data_new[trainIdx,]
  test <- data_new[-trainIdx,]
  
  #Preparing the model for classification
  d_model_1 <- rpart(Response ~ . , data = train, method = 'class',  control=rpart.control(minsplit=5, minbucket=5, maxdepth=8))
  plot(as.party(d_model_1))
  
  #Prunning the model for better accuracy
  opt <- which.min(d_model_1$cptable [, "xerror"])
  cp <- d_model_1$cptable [opt,"CP"]
  DT_Model_1_pruned <- prune(d_model_1, cp=cp)
  #plot(as.party(DT_Model_1_pruned))
  
  #Predicting the test data
  t_predict = predict(DT_Model_1_pruned, test, type = 'class')

  #Taking out test data's available response
  t_given = data_new['Response']
  
  #Calculating accuracy with confidence matrix
  confMat <- table(test$Response,t_predict)
  accuracy <- sum(diag(confMat))/sum(confMat)
  
  #Adding accuracy to list
  acc[i] <- accuracy
  i = i+1
  print(accuracy)
  
}


#Index of highest accuracy
idx_best = which.max(acc)


#Getting which model performed best
best_model <-  switch (idx_best,
  "All X data with both groups",
  "All Y data with both groups",
  "All XY data with both groups",
  
  
  "X data with 0 group",
  "Y data with 0 group",
  "XY data with 0 group",
  
  "X data with 1 group",
  "Y data with 1 group",
  "XY data with 1 group"
)

print(paste0("Best model is: " , best_model))


#Fetching all plots at same place
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:\\Users\\Kul\\Downloads\\18 oct\\Classes\\Data Analytics\\Plots")


plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, "C:\\Users\\Kul\\Downloads\\18 oct\\Classes\\Data Analytics\\Plots", row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0("C:\\Users\\Kul\\Downloads\\18 oct\\Classes\\Data Analytics\\Plots\\", 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)


