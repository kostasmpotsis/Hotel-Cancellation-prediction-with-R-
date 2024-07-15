install.packages("corrplot")####for corelation plot 
library(corrplot)
library(MASS)#for the lasso regrassion 
install.packages("xlsx")#to read the package 
library(xlsx)
install.packages("leaps")#for models 
library(leaps)
install.packages("caret")#for models 
library(caret)
install.packages("devtools")#for models 
library(devtools)
install.packages("car")
library(car)
library(PerformanceAnalytics)#for the corelation plot 
install.packages("PerformanceAnalytics")
install.packages("Matrix")
library(glmnet)#for the lasso 
library(heplots)
install.packages("heplots")#for plots 
library(ROSE)
install.packages('ROCR')# for the ROC 
library(ROCR)
library(class)
install.packages("pROC")# for the roc 
library(pROC)
install.packages('randomForest')
library(randomForest)
install.packages('e1071')
library(e1071)

# Replace "your_file.xlsx" with the path to your Excel file
data <- read.xlsx(file.choose(), sheetIndex = 1)
head(data)
dim(data)
table(data$market.segment.type)  
str(data)
data=(data[,-c(1,16)])

#not_canceled take value 1 
#canceled take value 0 
data$booking_statusK=ifelse(data$booking.status=="Canceled",0,1)
data$average.price=as.numeric(data$average.price)


summary(data)
par(mfrow=c(3,3))
label_size <- 2.5 # Adjust as needed
main_title_size <- 2  # Adjust as needed

# Example barplot with adjusted sizes
barplot(table(data$type.of.meal), col="orange", main="Type of Meal", cex.names=label_size, cex.main=main_title_size)
barplot(table(data$room.type), col="orange", main="Room Type", cex.names=label_size, cex.main=main_title_size)
barplot(table(data$market.segment.type), col="orange", main="Market Segment Type", cex.names=label_size, cex.main=main_title_size)
barplot(table(data$booking.status), col="orange", main="Booking Status", cex.names=label_size, cex.main=main_title_size)
barplot(table(data$number.of.adults), col="blue", main="Number of Adults", cex.names=label_size, cex.main=main_title_size)
barplot(table(data$number.of.children), col="blue", main="Number of Children", cex.names=label_size, cex.main=main_title_size)
barplot(table(data$number.of.weekend.nights), col="blue", main="Number of Weekend Nights", cex.names=label_size, cex.main=main_title_size)


par(mfrow=c(4,3))
main_title_size <- 2  # Adjust as needed

hist(data$number.of.adults, main="Number of Adults", cex.main=main_title_size)
hist(data$number.of.weekend.nights, main="Number of Weekend Nights", cex.main=main_title_size)
hist(data$number.of.week.nights, main="Number of Week Nights", cex.main=main_title_size)
hist(data$number.of.children, main="Number of Children", cex.main=main_title_size)
hist(data$car.parking.space, main="Car Parking Space", cex.main=main_title_size)
hist(data$lead.time, main="Lead Time", cex.main=main_title_size)
hist(data$repeated, main="Repeated", cex.main=main_title_size)
hist(data$P.C, main="P.C", cex.main=main_title_size)
hist(data$P.not.C, main="P.not.C", cex.main=main_title_size)
hist(as.numeric(data$average.price), main="Average Price", cex.main=main_title_size)
hist(data$special.requests, main="Special Requests", cex.main=main_title_size)




par(mfrow=c(3,2))
# Increase the size of the main titles
main_title_size <- 2  # Adjust as needed

# Create a stacked bar plot for special requests vs. booking status
barplot(special_booking_table, col=c("red", "blue"), legend=c("Canceled", "Not Canceled"),
        xlab="Special Requests", ylab="Frequency", main="Special Requests vs. Booking Status",
        beside=TRUE,cex.main=main_title_size)  # beside=TRUE creates a stacked bar plot

# Create boxplots with larger main titles
boxplot(data$number.of.week.nights ~ data$booking.status, main="Week Nights vs Booking Status",
        col="blue", cex.main=main_title_size)

boxplot(data$lead.time ~ data$booking.status, main="Booking Status vs Lead Time",
        col="blue", cex.main=main_title_size)

boxplot(data$number.of.weekend.nights ~ data$booking.status, main="Weekend Nights vs Booking Status",
        col="blue", cex.main=main_title_size)

boxplot(as.numeric(data$average.price) ~ data$booking.status, main="Average Price vs Booking Status",
        col="blue", cex.main=main_title_size)













library(glmnet)
head(data)
x=data[,-c(15,16)]
y=(data[,16])
as.matrix(x)
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(as.matrix(x), as.numeric(y), family = "binomial", alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(as.matrix(x), as.numeric(y), alpha = 1, lambda = best_lambda)
coef(best_model)





head(data)

data=data[,-c(5,7,9)]
data$booking.status=factor(data$booking.status)

data=data[,-12]
M=cor(data)
corrplot(M, method="number")
pairs(M)



set.seed(2000) 

# defining training control
# as cross-validation and 
# value of K equal to 10


# printing model performance metrics
# along with other details
print(model)

head(data)
data$booking_statusK=factor(data$booking_statusK)
head(train_balanced)
# Perform random_forest for classification and k cross validation based 
#on the maximum accuracy define the best k 
set.seed(2000)
results=NULL
indexes = createDataPartition(data$booking_statusK, p = .8, list = F)
train = data[indexes, ]
test = data[-indexes, ]
pinakas <- data.frame()
# Define the values of k to iterate over
kvals <- c(4, 6, 8, 10)
for (i in kvals) {
  # Set up train control with current k value
  train_control <- trainControl(method = "cv", number = i)  
  model <- train(booking_statusK ~ ., 
                 data = train, 
                 method = "rf",
                 trControl = train_control)  
  # Extract accuracy and k value, then bind them to pinakas
  result <- data.frame(Accuracy = max(model$results$Accuracy), Number = model$control$number)
  pinakas <- rbind(pinakas, result)
}
# Print the results data frame
print(pinakas)







############best k is 10 for random forest #################

train_control <- trainControl(method = "cv", number =10)
indexes = createDataPartition(data$booking_statusK, p = .8, list = F)
train = data[indexes, ]
test = data[-indexes, ]
model <- train(booking_statusK ~., 
               data =train, 
               method = "rf",
               trControl = train_control)



plot(model)
p1=predict(model,test)
cm=confusionMatrix(p1,test$booking_statusK)# 0.80% 

data.frame(Accuracy = cm$overall["Accuracy"],
           Sensitivity = cm$byClass["Sensitivity"],
           Specificity = cm$byClass["Specificity"])







#error rate for random forest 
varImp(model)
plot(varImp(model),main="variable_importance_for_random_forest")
head(train_balanced[,-c(8,9,7)])

p1=predict(model,test,type="prob")
pred_obj <- prediction(p1[,2], test$booking_statusK)
roc_curve <- performance(pred_obj, "tpr", "fpr")
plot(roc_curve, main = "ROC Curve for Random Forest Model", col = "blue")













##############knn for classification ##############

set.seed(2000)
# Perform knn for classification
#i put by hand different values for different fold 
#fold2,fold4,fold6,fold8,fold10

train=scale(train[,1:11])
train_control <- trainControl(method = "cv", number =2)
train_control <- trainControl(method = "cv", number =4)
train_control <- trainControl(method = "cv", number =6)
train_control <- trainControl(method = "cv", number =8)
train_control <- trainControl(method = "cv", number =10)

head(train)
model <- train(factor(booking_statusK) ~ ., 
               data = train, 
               method = "knn",
               trControl = train_control,
               preProc=c("center","scale"),
               tuneGrid = data.frame(k = c(3,5,7,9,10)))
plot(model)
fold2=cbind(model$results[model$results$Accuracy==max(model$results$Accuracy),],
            model$control$number)
fold4=cbind(model$results[model$results$Accuracy==max(model$results$Accuracy),],
            model$control$number)
fold6=cbind(model$results[model$results$Accuracy==max(model$results$Accuracy),],
            model$control$number)
fold8=cbind(model$results[model$results$Accuracy==max(model$results$Accuracy),],
            model$control$number)
fold10=cbind(model$results[model$results$Accuracy==max(model$results$Accuracy),],
             model$control$number)

rbind(fold2,fold4,fold6,fold8,fold10)
#best accuracy is k=6 and fold=6  

#best model for knn######
train_control <- trainControl(method = "cv", number =6)


model <- train(factor(booking_statusK) ~ ., 
               data = train, 
               method = "knn",
               trControl = train_control,
               preProc=c("center","scale"),
               tuneGrid = data.frame(k =6))

predictions <- predict(model,test)
# Calculate confusion matrix


cm <- confusionMatrix(predictions, factor(test$booking_statusK))
cm

#The Kappa statistic measures the agreement between the model's 
#predictions and the actual outcomes, adjusted for chance

data.frame(Accuracy = cm$overall["Accuracy"],
           Sensitivity = cm$byClass["Sensitivity"],
           Specificity = cm$byClass["Specificity"])

plot(model,main="error_rate_for_KNN_method")
varImp(model)
plot(varImp(model))


p1=predict(model,test)
confusionMatrix(p1,test$booking_statusK)
#error rate for random forest 


p1=predict(model,test,type="prob")
pred_obj <- prediction(p1[,2], test$booking_statusK)

# Compute ROC curve
roc_curve <- performance(pred_obj, "tpr", "fpr")

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for kNN Model", col = "blue")


#knn with importance variable only 

head(train_balanced[,c(2,8)])


















set.seed(90000)
##glm classification 
pinakas <- data.frame()
# Define the values of k to iterate over
kvals <- c(4, 6, 8, 10)
for (i in kvals) {
  # Set up train control with current k value
  train_control <- trainControl(method = "cv", number = i)  
  model <- train(factor(booking_statusK) ~ ., 
                 data = train, 
                 method = "glm",
                 trControl = train_control)  
  # Extract accuracy and k value, then bind them to pinakas
  result <- data.frame(Accuracy = max(model$results$Accuracy), Number = model$control$number)
  pinakas <- rbind(pinakas, result)
}
# Print the results data frame
print(pinakas)



train_control <- trainControl(method = "cv", number = 10)  
# Train GLM model
model <- train(factor(booking_statusK) ~ ., 
               data = train, 
               method = "glm",
               trControl = train_control)  

p1=predict(model,test)
confusionMatrix(p1,factor(test$booking_statusK))
varImp(model)
plot(varImp(model))



pred <- prediction(log.prediction.rd,test[,12])
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#AUC is more appropriate for imbalanced data.
unlist(slot(performance(pred, "auc"), "y.values"))







###svm classification##############
model1=svm(booking_statusK~.,data=train)
summary(model1)
head(data3)
plot(model1,data=train, number.of.weekend.nights~number.of.children)
pred=predict(model1,test)
cm1=table(pred,test[,12])
accuracy <- sum(diag(cm1))/sum(cm1)

#kernel linear 
model2=svm(booking_statusK~.,data=train,kernel="linear")
summary(model2)
pred=predict(model2,test)
cm2=table(pred,test[,12])
accuracy <- sum(diag(cm2))/sum(cm2)

#kernel polynomial 
model2=svm(booking_statusK~.,data=train,kernel="polynomial")
summary(model2)
pred=predict(model2,test)
cm2=table(pred,test[,12])
accuracy <- sum(diag(cm2))/sum(cm2)
1-accuracy #missclasification error 


### different choices
score<-NULL
for (cost in c(0.5,1,1.5) ) {
  for (gamma in c(0.7,1,1.2,1.5,2)) {
    
    svm_model <- svm(booking_statusK~.,data=train,kernel="polynomial",gamma=gamma, cost=cost)
    pred<- predict(svm_model,test)
    score<- c(score, sum(diag(table(pred,test[,12])))/(dim(test)[1]))
  }}


