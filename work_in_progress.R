##################### 
# Download the data # 
#####################
library(RCurl)
URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
X <- getURL(URL, ssl.verifypeer = FALSE)
training <- read.csv(textConnection(X))

URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
Y <- getURL(URL, ssl.verifypeer = FALSE)
testing_final <- read.csv(textConnection(Y))

# clean up the workspace 
rm(list=c("URL","X","Y"))

###############################
# Subset the data             #             
# testing and training for CV # 
###############################
library(caret)
inTrain <- createDataPartition(y=training$classe, p=0.8, list=FALSE)
training <- training[inTrain,]
testing <- training[-inTrain,]
rm(inTrain)

#################### 
# Inspect the data # 
####################
dim(training)
dim(testing)

# This looks *fairly* balanced between different outcomes
# A is a little more common 
summary(training$classe)
summary(training$classe)/nrow(training)

# Let's view an overall summary of the data 
summary(training)

# I probably don't want to predict with this
# It is just an observation # (1-19622)
summary(training$X)

# remove 'em!
training$X <- NULL 
testing$X <- NULL 
testing_final$X <- NULL 

# there are a lot of missing variables 
# e.g., many of the variables related to "forearm" are missing 19216/19622 observations
# in almost all of the cases where there are NAs - # NAs = 19216
# You can see that it's mostly variables that are statistics of other columns
# e.g., kurtosis, skewness, var, min, max, etc. 

# Let's remove those variables with tons of missing values 
# Use regular expressions to find all of the columns that you don't want 
var_names <- grep("^(var_|stddev_|avg_|min_|max_|skewness_|kurtosis_|amplitude_)",names(training))

# remove those columns from the training and testing sets and the final testing set too 
training02 <- training[,-var_names]
testing02 <- testing[,-var_names]
testing_final02 <- testing_final[,-var_names]

#################
# modelFit01    #
# Model fitting #
# Random forest #
#################
library(caret)

# run the model 
set.seed(1235)
modelFit01 <- train(classe ~ . -X, 
                  method="rf",
                  verbose=TRUE,
                  importance=TRUE,
                  data=training)

# model summary incl. OOB error rate and confusion matrix 
print(modelFit01)

# look at the final model 
modelFit01$finalModels

# look at the variable importance 
varImp(modelFit01)

# confusion matrix on the test data 
confusionMatrix(training$Classe, predict(modelFit01,testing))

# predict new values
pred_01 <- predict(modelFit01, testing)

######################
# modelFit02         #
# Model fitting      #
# Random forest      #
# PCA pre-processing # 
######################
library(caret)

# run the model 
set.seed(1235)
modelFit02 <- train(classe ~ . -X, 
                    method="rf",
                    verbose=TRUE,
                    importance=TRUE,
                    preProcess="pca",
                    data=training)

# model summary incl. OOB error rate and confusion matrix 
print(modelFit02)

# look at the final model 
modelFit02$finalModels

# look at the variable importance 
varImp(modelFit02)

# confusion matrix on the test data 
confusionMatrix(testing$Classe, predict(modelFit02,testing))

# predict new values
pred_02 <- predict(modelFit02, testing)

######################
# modelFit03         #
# Model fitting      #
# Random forest      #
# boot632 resampling #
######################
# boot632 adjusts for the fact that samples are repeatedly re-sampled
# reduces bias

library(caret)

# define the training control options 
my_control03 <- trainControl(method="boot632")

# run the model 
set.seed(1235)
modelFit03 <- train(classe ~ . -X, 
                    method="rf",
                    verbose=TRUE,
                    importance=TRUE,
                    trControl=my_control03,
                    data=training)

# model summary incl. OOB error rate and confusion matrix 
print(modelFit03)

# look at the final model 
modelFit03$finalModels

# look at the variable importance 
varImp(modelFit03)

# confusion matrix on the test data 
confusionMatrix(testing$Classe, predict(modelFit03,testing))

# predict new values
pred_03 <- predict(modelFit03, testing)

###########################
# modelFit04              #
# Model fitting           #
# Random forest           #
# smaller set of Xs       #
# proper testing/training #
###########################
library(caret)

# run the model 
set.seed(1235)
modelFit04 <- train(classe ~ . , 
                    method="rf",
                    verbose=TRUE,
                    importance=TRUE,
                    data=training02)

# model summary incl. OOB error rate and confusion matrix 
print(modelFit04)

# look at the final model 
#summary(modelFit04$finalModel)

# look at the variable importance 
varImp(modelFit04)

# predict new values
pred_04 <- predict(modelFit04, testing02)

# confusion matrix on the test data 
confusionMatrix(pred_04, testing02$classe)


###########################
# modelFit05              #
# Model fitting           #
# Random forest           #
# smaller set of Xs       #
# proper testing/training #
# boot632 resampling      #
###########################
library(caret)

# run the model 
set.seed(1235)

# define the training control options 
my_control05 <- trainControl(method="boot632")

modelFit05 <- train(classe ~ . , 
                    method="rf",
                    verbose=TRUE,
                    importance=TRUE,
                    trControl=my_control05,
                    data=training02)

# model summary incl. OOB error rate and confusion matrix 
print(modelFit05)

# look at the final model 
modelFit05$finalModels

# look at the variable importance 
varImp(modelFit05)

# predict new values
pred_05 <- predict(modelFit05, testing02)

# confusion matrix on the test data 
confusionMatrix(pred_05, testing02$classe)


##################### 
# Final predictions # 
#####################
predictions_final_04 <- predict(modelFit05,newdata=testing_final02)
predictions_final_04
predictions_final_05 <- predict(modelFit05,newdata=testing_final02)
predictions_final_05


########### 
# Answers # 
########### 
# create a vector to hold your answers 
answers <- c("B", "A", "B", "A", "A", "E", "D", "B", "A", "A",
             "B", "C", "B", "A", "E", "E", "A", "B", "B", "B")

# define a function to write each output file
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

# Set the wd to where you want these files to go 
setwd("C:/Users/Mike/Desktop/Dropbox/Coursera/Practical Machine Learning/PML_project/answers")

# save the files 
pml_write_files(answers)

# set wd back
setwd("C:/Users/Mike/Desktop/Dropbox/Coursera/Practical Machine Learning/PML_project")