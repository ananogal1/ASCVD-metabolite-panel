#Random Forest script: 
#Author: Ana Nogal
#email: For any query contact: ana.nogal_macho@kcl.ac.uk
#Date: 30/04/2021
#Comments: The parts of the code indicated with "#!!!' means that a modification with your dataset is required.


#Load libraries
library(writexl)
library(openxlsx)
library(readxl)
library(readr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(stringr)
library(randomForest)#random forest
library(caret) #confussion matrix
library(Metrics) #AUC
library(ROCR) #roc
library(readxl)
library(BradleyTerry2) #BT
library(doBy)
library(tibble)



###############################################DATA PREPARATION #########################################
#Requirements:
##Name your dataset: data. 
##The column containing the response variable of your dataset should have been named 'response'
#!!!
##eg: data <- data %>% mutate(response=ascvd)
data <- #dataset

#RF just works with complete cases
row.has.na <- apply(data, 1, function(x){any(is.na(x))})
data_no_NA <- data[!row.has.na, ]
dim(data_no_NA)

#delete the predictors with variance cero
var0<-data_no_NA %>% select(-response) %>%
  nearZeroVar(saveMetrics = TRUE)
var0<-as.data.frame(var0)
var0$variables<-rownames(var0)
var0$variables[which(var0$zeroVar=='TRUE')] #check if any has a variance 0

#if there is any uncomment the following two lines to delete them: 
#!!!
#var0_pred<-as.vector(var0$variables[which(var0$zeroVar=='TRUE')])
#data_no_NA<-data_no_NA[,-which(colnames(data_no_NA) %in% var0_pred)]


#Create a training and a test set
# Training set will cotain 80 % of the total data. If this percentage wants to be modified: change the value of 'p'.
# Set a random seed for reproducibility
set.seed(1200)
train_indices <- createDataPartition(data_no_NA[,'response'], p = 0.8, list = FALSE)
# Subset the data frame to training indices only
train <- data_no_NA[train_indices, ]
# Exclude the training indices to create the test set
test <- data_no_NA[-train_indices, ]

#check that the distribution is similar in the test and the train (for categorical variables)
prop.table(table(train$response))
prop.table(table(test$response))

#Save the train and test set dataset.
#Modify the 4 next lines for your directory.
#!!!
name_y='serum'
response_name='ascvd'
name_train<-paste('01.Created_data/00.training_test/',name_y,'/train_',response_name,".xlsx",sep='')
name_test<-paste('01.Created_data/00.training_test/',name_y,'/test_',response_name,".xlsx",sep='')
write.xlsx(train,name_train)
write.xlsx(test,name_test)

#########################################################################################################


###############################################RANDOM FOREST #########################################
#Load the training and test set.
#Modify the 4 next lines for your directory.
#!!!
type_predictors<-'serum' 
name_response<-'ascvd'
train<-read_excel(paste("01.Created_data/00.training_test/",type_predictors,"/train_",name_response,".xlsx",sep=""))
test<-read_excel(paste("01.Created_data/00.training_test/",type_predictors,"/test_",name_response,".xlsx",sep=""))

#If the response variable is categorical, run just : type_response<-'categorical' , if not just run type_response<-'continuous'
#!!!
type_response<-'categorical'
type_response<-'continuous'

seed=281

if(type_response=='categorical'){
  train=train %>% mutate(response=as.factor(response))
  test=test %>% mutate(response=as.factor(response))
  metric='Accuracy'
} else {
  train=train %>% mutate(response=as.numeric(response))
  test=test %>% mutate(response=as.numeric(response))
  metric='RMSE'
}

#FIND THE BEST HYPERPARAMETERS
#trainControl
number = 5
repeats=3

seeds_control <- vector(mode = "list", length = (number * repeats) + 1)
for (i in 1:(number * repeats)) {
  seeds_control[[i]] <- sample.int(1000, nrow(350))
}
seeds_control[[(number * repeats) + 1]] <- sample.int(1000, 1)

#.Set Train Control 
trControl <- trainControl(method = "adaptive_cv", #Alternatives: "repeatedcv", "cv" , adaptive_cv
                          adaptive= list(min=10, #minimum number of resamples used for each hyperparameter. default=5. The larger we set min, the slower the resampling process will be but we increase our likelihood of finding the optimal hyperparameter combination. 
                                         alpha=0.05, #this value is not going to significantly change the results
                                         method='gls', #Alternative: BT (BradleyTerry2)
                                         complete=F),
                          number = number,
                          repeats=repeats,
                          search = "random", #grid or random
                          seeds = seeds_control,
                          allowParallel = TRUE)

#Train the model and find the best hyperparameters
rf_default <-train(response~., data=train, method = "ranger",
                   metric=metric,
                   trControl=trControl,
                   tuneLength=3,
                   verbose=F)
rf_default$finalModel
#Store the best mtry and min node size  (hyperparameters) values in variables 
best_mtry<-rf_default[["bestTune"]][["mtry"]]
best_nodesize<-rf_default[["bestTune"]][["min.node.size"]]

#Choose the ntree (hyperparameter) with the lowest OOB error or mse error /max rsq
#This value is not going to highly modify the results
oob_err<-c()
mse_err<-c()
for (j in c(1:2)){
  for (i in c(1000,1500,2000)){
    model_RF<-randomForest(formula = (response) ~., data = train, importance =TRUE,ntree=i,mtry=best_mtry,nodesize=best_nodesize,proximity=T) 
    if (type_response=='categorical'){
      OOB_err<-model_RF$err.rate
      OOB<-OOB_err[nrow(OOB_err),1]
      all<-cbind(j,i,OOB)
      oob_err<-rbind(oob_err,all)
    } else {
      mse_df<- as.data.frame(model_RF[["mse"]])
      mse_last<-mse_df$`model_RF[["mse"]]`[nrow(mse_df)]
      rsq_df<- as.data.frame(model_RF[["rsq"]])
      rsq_last<-(rsq_df$`model_RF[["rsq"]]`[nrow(rsq_df)])*100
      all<-cbind(j,i,mse_last,rsq_last)
      mse_err<-rbind(mse_err,all)
    }
  }
}

oob_err<-as.data.frame(oob_err)
mse_err<-as.data.frame(mse_err)

for (z in c(1000,1500,2000)){
  if (type_response=='categorical'){
    oob_err$mean[oob_err$i==z]<-mean(oob_err$OOB[oob_err$i==z])
  } else {
    mse_err$mean_mse[mse_err$i==z]<-mean(mse_err$mse_last[mse_err$i==z])
    mse_err$mean_rsq[mse_err$i==z]<-mean(mse_err$rsq_last[mse_err$i==z])
  }
}


#Build model using the best hyperparameters
if (type_response=='categorical'){
  ntree=unique(oob_err$i[which(oob_err$mean==(min(oob_err$mean)))])
} else {
  ntree=unique(mse_err$i[which((mse_err$mean_mse==(min(mse_err$mean_mse))) & (mse_err$mean_rsq==(max(mse_err$mean_rsq)))) ])
}

set.seed(seed)
model_RF<-randomForest(formula = response ~., data = train, importance =TRUE,ntree=ntree,mtry=best_mtry,nodesize=best_nodesize,proximity=T) 
print(model_RF)

#Variable importance
#.....................importance of the variables
importance_RF<-randomForest::importance(model_RF)#check which variables give the best values (arent sorted)
importance_RF<-data.frame(importance_RF)

if (type_response=='categorical'){
  importance_RF<-orderBy(~-MeanDecreaseAccuracy, data=importance_RF)
} else {
  importance_RF<-orderBy(~-IncNodePurity, data=importance_RF)
}
View(importance_RF)
#varImpPlot(model_RF)

#Check how many variables are relevant to the model 
filter_var<-rfcv(select(train,-response),train$response,
                 scale="log",cv.fold=5,step=0.5,ntree=ntree)
plot(filter_var$n.var, filter_var$error.cv, log="x", type="o", lwd=2)
filter_var$error.cv
