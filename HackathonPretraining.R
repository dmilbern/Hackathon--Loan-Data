library(data.table)
library(readr)
library(sqldf)
library(dplyr)

getwd()
setwd("/Users/davidmilbern/Documents/ABA- Hackathon- Loan Approval")
#file <- readr::read_csv("train_u6lujuX_CVtuZ9i.csv")
file<- read.csv("train_u6lujuX_CVtuZ9i.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?","NAN"),
                stringsAsFactors = F)
#############
#MICE PACKAGE FOR PREPOSSING DATA
install.packages("mice")
library(mice)
Mice_model<-mice(file)
Imp_train<-(Mice_model)


#########

summary(file)
#test.data <- read.csv("test_Y3wMUE5_7gLdaTN.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
sum(is.na(file$Gender))
#13 Null - will do Mode
#file$Gender[is.na(file$Gender)] <- Male
sum(is.na(file$Married))
#married has 3 null, will do mode
sum(is.na(file$Education))
#Education has 0 
sum(is.na(file$Self_Employed))
#32 null
sum(is.na(file$ApplicantIncome))

#0 null
sum(is.na(file$CoapplicantIncome))
#Loan amount has 0 
sum(is.na(file$LoanAmount))
sum(is.nan(file$LoanAmount))
#22 null

sum(is.na(file$Loan_Amount_Term))
#14 Null
sum(is.na(file$Credit_History))
#50 null
sum(is.na(file$Property_Area))
#0 null
sum(is.na(file$Loan_Status))
#0 null
########FILL WITH MEAN
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
file$ApplicantIncome <- lapply(file$ApplicantIncome, NA2mean)
file$CoapplicantIncome <- lapply(file$CoapplicantIncome, NA2mean)
file$LoanAmount <- lapply(file$LoanAmount, NA2mean)
#FILL WITH MODE
# NA2mode <- function(x) replace(x, is.na(x), mode(x))
# mode(file$Gender)
file$Gender <- lapply(file$Gender, Mode)
file$Married <- lapply(file$Married, Mode)
file$Education <- lapply(file$Education, Mode)
file$Self_Employed <- lapply(file$Self_Employed, Mode)
file$Loan_Amount_Term <- lapply(file$Loan_Amount_Term, Mode)
file$Credit_History <- lapply(file$Credit_History, Mode)
file$Property_Area <- lapply(file$Property_Area, Mode)
# install.packages("modeest")
# library(modeest)
# 
# cat<-c("Gender","Married","Education","Self_Employed","Loan_Amount_Term","Credit_History","Property_Area")
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
# for i in cat{
#   temp<-Mode(i)
#   return temp
# }
write.csv(file, file="hackathontrain.csv",row.names=FALSE)
