library(ggplot2)
library(lattice)
library(caret)
library(ellipse)

# 1. Load the dataset
training <- read.csv("train_agnpsr_3fgl.csv")
test <- read.csv("test_agnpsr_3fgl.csv")

# 2. Summary the dataset
head(training)
names(training)
sapply(training, class)

# Get rid of the useless X (the first column) 
test <- test[, 2:37]
training <- training[, 2:37]

# Change type of "pulsarness" from character to factor.
training$pulsarness <- factor(training$pulsarness)
test$pulsarness <- factor(test$pulsarness)

summary(training)

#Training <- cbind(training$Spectral_Index, training$Variability_Index, training$Flux_Density, training$Unc_Energy_Flux100, training$Signif_Curve, training$hr12, training$hr23, training$hr34, training$hr45, training$pulsarness)
#Test <- cbind(test$Spectral_Index, test$Variability_Index, test$Flux_Density, test$Unc_Energy_Flux100, test$Signif_Curve, test$hr12, test$hr23, test$hr34, test$hr45, test$pulsarness)


# 3.Evaluating some algorithms

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Delete useless attributes
useless <- c(6, 18, 19, 22, 23, 24, 25, 27, 28, 29, 30, 31)
training <- training[, -useless]
test <- test[, -useless]

# a) linear algorithms
set.seed(7)
fit.lda <- train(pulsarness~., data=training, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(pulsarness~., data=training, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(pulsarness~., data=training, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(pulsarness~., data=training, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(pulsarness~., data=training, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

print(fit.svm)

# estimate skill of SVM on the test dataset
predictions <- predict(fit.svm, test)
confusionMatrix(predictions, test$pulsarness)





# 4.The deepnet package

library(deepnet)

y <- as.matrix(training[, 24])
y[which(y=="Non-Pulsar")] <- 0
y[which(y=="Pulsar")] <- 1
y <- as.numeric(y)
x <- as.numeric(as.matrix(training[, 1:23]))
x <- matrix(as.numeric(x), ncol=23)

y_test <- as.matrix(test[, 24])
y_test[which(y_test=="Non-Pulsar")] <- 0
y_test[which(y_test=="Pulsar")] <- 1
x_test <- as.numeric(as.matrix(test[, 1:23]))
x_test <- matrix(as.numeric(x_test), ncol=23)


dep_accu <- function(i){
  set.seed(7)
  nn <- nn.train(x, y, hidden = c(i), hidden_dropout = 0, visible_dropout = 0)
  yy <- nn.predict(nn, x_test)
  print(head(yy))
  
  yhat <- matrix(0, length(yy), 1)
  yhat[which(yy > mean(yy))] <- 1
  yhat[which(yy <= mean(yy))] <- 0
  cm <- table(y_test, yhat)
  print(cm)
  
  yhat <- factor(yhat)
  y_test <- factor(y_test)
  a <- confusionMatrix(yhat, y_test)
  
  return(a$overall[1])
}

dep_kap <- function(i){
  set.seed(7)
  nn <- nn.train(x, y, hidden = c(i), hidden_dropout = 0, visible_dropout = 0)
  yy <- nn.predict(nn, x_test)
  print(head(yy))
  
  yhat <- matrix(0, length(yy), 1)
  yhat[which(yy > mean(yy))] <- 1
  yhat[which(yy <= mean(yy))] <- 0
  cm <- table(y_test, yhat)
  print(cm)
  
  yhat <- factor(yhat)
  y_test <- factor(y_test)
  a <- confusionMatrix(yhat, y_test)
  
  return(a$overall[2])
}

accu_dep <- (0)
kapp_dep <- (0)
for (i in c(1:30)) {
  accu_dep[i] <- dep_accu(i)
  kapp_dep[i] <- dep_kap(i)
}

x_0 <- c(1:30)
df_1 <- cbind.data.frame(x_0, accu_dep, kapp_dep)

p_line <- ggplot(df_1) + geom_line(aes(x=df_1[, 1], y=accu_dep), color="blue") + geom_line(aes(x=df_1[, 1], y=kapp_dep)) + xlab("hidden neurons") + ylab("")
p_line

# 5.The neuralnet package

library(neuralnet)

neu_accu <- function(j){
  df <- data.frame(cbind(x,y))
  set.seed(7)
  nn <- neuralnet(y~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23,data=df,hidden = j, stepmax = 10^7)
  yy <- predict(nn, x_test)
  
  yhat <- matrix(0,length(y_test),1)
  yhat[which(yy > mean(yy))] <- 1
  yhat[which(yy <= mean(yy))] <- 0
  cm <- table(y_test,yhat)
  
  yhat <- factor(yhat)
  y_test <- factor(y_test)
  b <- confusionMatrix(yhat, y_test)
  
  return(b$overall[1])
}

neu_kapp <- function(j){
  df <- data.frame(cbind(x,y))
  set.seed(7)
  nn <- neuralnet(y~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23,data=df,hidden = j, stepmax = 10^7)
  yy <- predict(nn, x_test)
  
  yhat <- matrix(0,length(y_test),1)
  yhat[which(yy > mean(yy))] <- 1
  yhat[which(yy <= mean(yy))] <- 0
  cm <- table(y_test,yhat)
  
  yhat <- factor(yhat)
  y_test <- factor(y_test)
  b <- confusionMatrix(yhat, y_test)
  
  return(b$overall[2])
}

accu_neu <- (0)
kapp_neu <- (0)
for (j in c(1:30)) {
  accu_neu[j] <- neu_accu(j)
  kapp_neu[j] <- neu_kapp(j)
}

x_0 <- c(1:30)
df_2 <- cbind.data.frame(x_0, accu_neu, kapp_neu)

p_line_1 <- ggplot(df_2) + geom_line(aes(x=df_2[, 1], y=accu_neu), color="blue") + geom_line(aes(x=df_1[, 1], y=kapp_neu)) + xlab("hidden neurons") + ylab("")
p_line_1

