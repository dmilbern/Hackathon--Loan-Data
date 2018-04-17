install.packages("caret")
library(caret)
library(mice)
file<- read.csv("train_u6lujuX_CVtuZ9i.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?","NAN"),
                stringsAsFactors = F)
Mice_model<-mice(file)
Imp_train<-complete(Mice_model)
Imp_train<- Imp_train[,2:13]
set.seed(825)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
gbmFit1 <- train(Loan_Status~., data = Imp_train, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 na.action = na.pass)
gbmFit1
plot(gbmFit1)
summary(Imp_train)
summary(file)
file$Loan_Status <- as.factor(file$Loan_Status)


test.data <- read.csv("test_Y3wMUE5_7gLdaTN.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
