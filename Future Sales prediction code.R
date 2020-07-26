#########################################
   ########--- Transitowne ---#######
#########################################

#### Future Sales Prediction Model ######

#####---- Simon Business School ---######

######################################
### Anjali Shastri
######################################


cat("\014") #clears screen
graphics.off() # clears old plots
rm(list=ls())  # removing the old files

# Installing the required packages
library(caTools)
library(gbm)
library(ROCR)

#Setting the working directory
#setwd("C:/Project/leads_data")

########### Defined a function to calculate the ROC Plot ##############
# Function Starts here
rocPlot <- function(train_observed, train_predicted, test_observed, test_predicted) {
  pred_tr <- prediction( train_predicted, train_observed)
  perf_tr <- performance(pred_tr,"tpr","fpr")
  auc_tr <- performance(pred_tr,"auc")
  pred_tst <- prediction( test_predicted, test_observed)
  perf_tst <- performance(pred_tst,"tpr","fpr")
  auc_tst <- performance(pred_tst,"auc")  
  p <- plot(perf_tr,col="blue", main = "ROC Plot")
  par(new = TRUE)
  q <- plot(perf_tst,col="red", main = "ROC Plot")
  grid() 
  legend("topleft", legend = c(paste("Train (AUC =", toString(100*round(auc_tr@y.values[[1]], digits = 4)),"%)"), 
                               paste("Test (AUC =", toString(100*round(auc_tst@y.values[[1]], digits = 4)),"%)")), 
         col = c("blue","red"), lwd = 0.5)
  
  return(list(p, q))
}


# The data was cleaned in the Excel itself. Steps to which can me found the the ppt
# Loading the clean data file 
data <-  read.csv("final_data.csv")

#Splitting the data into training and testing set - 70% and 30% 
set.seed(956)
split <- sample.split(data$Leads_VIN, SplitRatio = 0.70)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)


#################logistic regression model #######################
# Building the Logistic regression model
log_model <- glm (Leads_VIN ~ ., data = data, family = binomial)
summary(log_model)


# Training set predict
predict_train <- predict(log_model, train_data[,final_var_logistic] ,type = 'response')

# Testing set predict
predict_test <- predict(log_model, test_data[,final_var_logistic] ,type = 'response')

# Call this function for ROC plot
ROC_plot = rocPlot(train_data$cfpb_loan,predict_train,test_data$cfpb_loan,predict_test)


# Building the GBM model
############### Gradinet Boosting Decision Tree ####################
gbmModel = gbm(Leads_VIN ~ .,
                     data = data,
                     n.trees =2000,
                     shrinkage = .01,
                     n.minobsinnode = 10,
                     interaction.depth = 4
                    #cv.folds = 2
)


# Getting the summary of the model to get the Variable Significane
summary(gbmModel)

# Training set predict
train_predict_prob <- predict(gbmModel, train_data, type = 'response', n.trees = 2000)

# Testing set predict
test_predict_prob <- predict(gbmModel, test_data, type = 'response', n.trees = 2000)


# Call this function for ROC plot
ROC_plot = rocPlot(train_data$Leads_VIN,train_predict_prob,test_data$Leads_VIN,test_predict_prob)


# Extracting the Output - Test data along with the probabilities
test_data$probabilities <- test_predict_prob
write.csv("test_data_with_prob.csv", test_data)

### Our suggestion here is to target the leads with highest probabilities given by the model to get converted into customers.

