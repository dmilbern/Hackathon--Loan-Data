# 2. mice 
# 1. mean, mode
# 3. H2o => glrm
# rf, caret
# combine incomes

# Step 3
library("mice")

setwd("/Users/admin/Documents/Courses/Advanced_Business_Analytics/Hackathon")

train.data <- read.csv("train_u6lujuX_CVtuZ9i.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?","NAN"))

test.data <- read.csv("test_Y3wMUE5_7gLdaTN.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))


# Step 4: Drop the following variables from the data: "cand_id", "last_name", "first_name", "twitterbirth", "facebookdate", "facebookjan", "youtubebirth".
names(train.data)
names(test.data)
train.selected <- train.data[-c(1)]
test.selected <- test.data[-c(1)]
names(train.selected)

# Step 5: Convert the following variables into factor variables using function as.factor(): “twitter”, “facebook”, “youtube”, “cand_ici”, and “gen_election”.
ix <- c(1,2,4,5,11,12)
ix.test <- c(1,2,4,5,11)

train.selected[,ix] <- lapply(train.selected[,ix],as.factor)
test.selected[,ix.test] <- lapply(test.selected[,ix.test],as.factor)

iy <- c(3,6,7,8,9,10)
train.selected[,iy] <- lapply(train.selected[,iy],as.numeric)
test.selected[,iy] <- lapply(test.selected[,iy],as.numeric)

train.selected$totalIncome <- train.selected$ApplicantIncome + train.selected$CoapplicantIncome
train.selected <- train.selected[,-c(6,7)]

test.selected$totalIncome <- test.selected$ApplicantIncome + test.selected$CoapplicantIncome
test.selected <- test.selected[,-c(6,7)]

# Step 7: Remove all of the observations with any missing values using function complete.cases()
mice.model <- mice(train.selected)
nomissed.train.data <- complete(mice.model)
mice.model <- mice(test.selected)
nomissed.test.data <- complete(mice.model)
#train.selected[complete.cases(train.selected),]; dim(nomissed.train.data)


library(mlbench)
library(caret)

# load the dataset
#data(nomissed.train.data)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=10)

# train the LVQ model
set.seed(7)
modelLvq <- train(Loan_Status~., data=nomissed.train.data, method="lvq", trControl=control)
# train the GBM model
set.seed(7)
modelGbm <- train(Loan_Status~., data=nomissed.train.data, method="gbm", trControl=control, verbose=FALSE)

# train the SVM model
set.seed(7)
modelSvm <- train(Loan_Status~., data=nomissed.train.data, method="svmRadial", trControl=control)

# train the RF model
set.seed(7)
modelRF <- train(Loan_Status~., data=nomissed.train.data, method="rf", trControl=control)

# train the RF model
set.seed(7)
modelNN <- train(Loan_Status~., data=nomissed.train.data, method="nnet", trControl=control)

# train the RF model
set.seed(7)
modelXgbT <- train(Loan_Status~., data=nomissed.train.data, method="xgbTree", trControl=control)

# train the RF model
set.seed(7)
modelKNN <- train(Loan_Status~., data=nomissed.train.data, method="knn", trControl=control)

# collect resamples
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm, RF=modelRF, NN=modelNN, XgbT=modelXgbT,KNN=modelKNN))
# summarize the distributions

summary(results)


pred <- predict(modelNN, nomissed.test.data,type="raw")
head(predicted_values)

#threshold = 0.5 
#threshold <- 0.5
#pred <- factor( ifelse(predicted_values[,2] > threshold, "Y", "N") )

result <- test.data
result$Loan_Status <- pred
end.result <- result[,c(1,13)]

#end.result[is.na(end.result$Loan_Status),2] <- "N"
write.csv(end.result,"submition_nn.csv")




# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)












######### JUNKS #############
# Step 8:	Randomly assign 70% of the observations to train_data and the remaining observations to test_data (Refer to Module 6 for the code). 
library(randomForest)
set.seed(214325);rf <- randomForest(Loan_Status~., data=nomissed.train.data, ntree=70, importance = T, na.action=na.exclude);rf # needs to be optimizer

predicted_values <- predict(rf, nomissed.test.data,type= "prob")
head(predicted_values)

# threshold = 0.5 
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, "Y", "N") )

result <- test.data
result$Loan_Status <- pred
end.result <- result[,c(1,13)]

#end.result[is.na(end.result$Loan_Status),2] <- "N"
write.csv(end.result,"submition.csv")

#confusionMatrix(pred, nomissed.train.data$Loan_Status, positive = "N")





#train_data_idx <- createDataPartition(nomissed.train.data$Loan_Status,p = 0.7, list = F)

#train_data <- nomissed.data[train_data_idx,];dim(train_data)
#test_data <- nomissed.data[-train_data_idx,];dim(test_data)

# Step 9:	Use train_data to build a random forest classifier with 10 trees. Use library(randomForest). 










mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=80,  stepFactor=1.5, improve=0.01, trace=TRUE, importance = T, plot=TRUE, na.action=na.exclude)

# 9.7. Use your recommended number of trees and mtry value to build a new random forest classifier using train_data. What is OOB estimate of error rate? 
set.seed(214325);rf <- randomForest(gen_election~., data=train_data, mtry = 10, ntree=70, na.action=na.exclude, importance = T);rf # needs to be optimizer



# randomForest
library(randomForest)
selected.col <- c("Gender", "Married", "Dependents", "Education",        "Self_Employed", "ApplicantIncome",   "CoapplicantIncome", "LoanAmount",
                  "Loan_Status"  
)

train.data[train.data == ""] <- NA
train.data[train.data == NaN] <- NA
train.data[train.data == Inf] <- NA

train.data.complete <- train.data[complete.cases(train.data),]


train.data.sample <- train.data[1:4,c(2:4,13)]


rf <- 
  randomForest(as.factor(Loan_Status)~., data = train.data.sample, ntree=70, importance = T, na.action=na.exclude);

rf # needs to be optimizer




# submit -> id, label