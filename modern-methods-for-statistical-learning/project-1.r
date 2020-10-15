library(MASS)
library(lattice)

accuracy_test <- function(table){
  accuracy <- sum(diag(table)) / sum(table)
  return(accuracy)
}
#currently not working! Problem with logistic, vs pure classification
trainingerrors <- function(training.data, model, type){
  if (type == "logistic"){
    predictions.lg <- predict(model, data = training.data, type = "response")
    predictions <- ifelse(predictions.lg > 0.5, 1, 0)
    training.table <- table(training.data$Y, predictions)
  }
  else{
    predictions <- predict(model, newdata = training.data, type = "class")
    training.table <- table(training.data$Y, predictions$class)
  }
  return(training.table)
}
testingerrors <- function(test.data, model, type){
  if (type == "logistic"){
    predictions.lg <- predict(model, newdata = test.data, type = "response")
    predictions <- ifelse(predictions.lg > 0.5, 1, 0)
    test.table <- table(test.data$Y, predictions)
  }
  else{
    predictions <- predict(model, newdata = test.data, type = "class")  
    test.table <- table(test.data$Y, predictions$class)
  }
  return(test.table)
}
#-------------------- DATA SIMULATION-------------------------
set.seed(2)
M <- 300
N <- 1000

fracMN <- M/N

mupos <- matrix(c(3,2), nrow = 2, ncol = 1)
sigmapos <- matrix(c(4,0,0,5), nrow = 2, ncol = 2)
Xpos <- mvrnorm(M, mupos, sigmapos)
Y <- rep(1, M)
posdata <- data.frame(Y, Xpos)

muneg <- matrix(c(0,-2), nrow = 2, ncol = 1)
sigmaneg <- matrix(c(3,0,0,2), nrow = 2, ncol = 2)
Xneg <- mvrnorm(N-M, muneg, sigmaneg)
Y <- rep(0, N-M)
negdata <- data.frame(Y, Xneg)

data <- rbind(posdata, negdata)

colnames(data) <- c("Y", "X1", "X2")
xyplot(X1 ~ X2, data, groups = Y, pch= 20)

plot(Y ~ X1, data)
plot(Y ~ X2, data)

data2  <- data.frame(0, nrow = N, ncol = 3) 
data3  <- matrix(0, nrow = N, ncol = 3) 

for (i in 1:N){
  
  if (i <= M){
    
    data2[i,1] <- 1
    data2[i,2] <- rexp(1, rate = 10)
    data2[i,3] <- rnorm(1, mean = 10, sd = 2)
  }
  else{
    data2[i,1] <- 0
    data2[i,2] <- rexp(1, rate = 20)
    data2[i,3] <- rnorm(1, mean = 4, sd = 1)
  }
}


for (i in 1:N){
  if (i <= M){
    
    data3[i,1] <- 1
    data3[i,2] <- rpois(1, lambda = 30)
    data3[i,3] <- rexp(1, rate = 5)
  }
  else{
    data3[i,1] <- 0
    data3[i,2] <- rpois(1, lambda = 15 )
    data3[i,3] <- rexp(1, rate = 20)
  }
}


colnames(data2) <- c("Y", "X1", "X2")
colnames(data3) <- c("Y", "X1", "X2")
data2 <- as.data.frame(data2)
data3 <- as.data.frame(data3)

xyplot(X1 ~ X2, data, groups = Y, par.settings = list(superpose.symbol = list(col = c("lightblue","orange"), pch = 19)), main = "Case A")
xyplot(X1 ~ X2, data2, groups = Y, par.settings = list(superpose.symbol = list(col = c("lightblue","orange"), pch = 19)), main = "Case B")
xyplot(X1 ~ X2, data3, groups = Y, par.settings = list(superpose.symbol = list(col = c("lightblue","orange"), pch = 19)), main = "Case C")


#---------------------Splitting the data------------------------

smp_size <- floor(0.7 * nrow(data))
train_index <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_index, ]
test <- data[-train_index, ]

train2 <- data2[train_index, ]
test2 <- data2[-train_index, ]

train3 <- data3[train_index, ]
test3 <- data3[-train_index, ]

#---------------------LDA------------------------
LDA <- lda(Y ~ X1 + X2, data = train)

train_LDA.table <- trainingerrors(train, LDA, "")
test_LDA.table <- testingerrors(test, LDA, "")

training_accuracy.lda <- accuracy_test(train_LDA.table)
testing_accuracy.lda <- accuracy_test(test_LDA.table)

sprintf("Training accuracy of LDA: %s",training_accuracy.lda)
sprintf("Training Error of LDA: %s", 1 - training_accuracy.lda)
print("Training Confusion Matrix LDA")
print(train_LDA.table)

sprintf("Testing accuracy of LDA: %s",testing_accuracy.lda)
sprintf("Testing Error of LDA: %s", 1 - testing_accuracy.lda)
print("Testing Confusion Matrix LDA")
print(test_LDA.table)

#---------------------QDA------------------------
QDA <- qda(Y ~ X1 + X2, data = train)

training_qda.table <- trainingerrors(train, QDA,"")
test_qda.table <- testingerrors(test, QDA, "")

training_accuracy.qda <- accuracy_test(training_qda.table)
testing_accuracy.qda <- accuracy_test(test_qda.table)

sprintf("Training accuracy of QDA: %s",training_accuracy.qda)
sprintf("Training Error of QDA: %s", 1 - training_accuracy.qda)
print("Training Confusion Matrix QDA")
print(training_qda.table)

sprintf("Testing accuracy of QDA: %s",testing_accuracy.qda)
sprintf("Testing Error of LDA: %s", 1 - testing_accuracy.qda)
print("Testing Confusion Matrix QDA")
print(test_qda.table)

#---------------------Logistic Regression------------------------
LG <- glm(Y ~ X1 + X2, data = train, family = binomial)

training_lg.table <- trainingerrors(train, LG, "logistic")
test_lg.table <- testingerrors(test, LG, "logistic")

training_accuracy.lg <- accuracy_test(training_lg.table)
testing_accuracy.lg <- accuracy_test(test_lg.table)

sprintf("Training accuracy of Logistic: %s",training_accuracy.lg)
sprintf("Training Error of Logistic: %s", 1 - training_accuracy.lg)
print("Training Confusion Matrix Logistic")
print(training_lg.table)

sprintf("Testing accuracy of Logistic: %s",testing_accuracy.lg)
sprintf("Testing Error of Logistic: %s", 1 - testing_accuracy.lg)
print("Testing Confusion Matrix Logistic")
print(test_lg.table)

#----------------------CASE B-------------------------------

#---------------------LDA------------------------
LDA <- lda(Y ~ X1 + X2, data = train2)

train_LDA.table <- trainingerrors(train2, LDA, "")
test_LDA.table <- testingerrors(test2, LDA, "")

training_accuracy.lda <- accuracy_test(train_LDA.table)
testing_accuracy.lda <- accuracy_test(test_LDA.table)

sprintf("Training accuracy of LDA: %s",training_accuracy.lda)
sprintf("Training Error of LDA: %s", 1 - training_accuracy.lda)
print("Training Confusion Matrix LDA")
print(train_LDA.table)

sprintf("Testing accuracy of LDA: %s",testing_accuracy.lda)
sprintf("Testing Error of LDA: %s", 1 - testing_accuracy.lda)
print("Testing Confusion Matrix LDA")
print(test_LDA.table)

#---------------------QDA------------------------
QDA <- qda(Y ~ X1 + X2, data = train2)

training_qda.table <- trainingerrors(train2, QDA,"")
test_qda.table <- testingerrors(test2, QDA, "")

training_accuracy.qda <- accuracy_test(training_qda.table)
testing_accuracy.qda <- accuracy_test(test_qda.table)

sprintf("Training accuracy of QDA: %s",training_accuracy.qda)
sprintf("Training Error of QDA: %s", 1 - training_accuracy.qda)
print("Training Confusion Matrix QDA")
print(training_qda.table)

sprintf("Testing accuracy of QDA: %s",testing_accuracy.qda)
sprintf("Testing Error of LDA: %s", 1 - testing_accuracy.qda)
print("Testing Confusion Matrix QDA")
print(test_qda.table)

#---------------------Logistic Regression------------------------
LG <- glm(Y ~ X1 + X2, data = train2, family = binomial)

training_lg.table <- trainingerrors(train2, LG, "logistic")
test_lg.table <- testingerrors(test2, LG, "logistic")

training_accuracy.lg <- accuracy_test(training_lg.table)
testing_accuracy.lg <- accuracy_test(test_lg.table)

sprintf("Training accuracy of Logistic: %s",training_accuracy.lg)
sprintf("Training Error of Logistic: %s", 1 - training_accuracy.lg)
print("Training Confusion Matrix Logistic")
print(training_lg.table)

sprintf("Testing accuracy of Logistic: %s",testing_accuracy.lg)
sprintf("Testing Error of Logistic: %s", 1 - testing_accuracy.lg)
print("Testing Confusion Matrix Logistic")
print(test_lg.table)

#----------------------CASE C-------------------------------

#---------------------LDA------------------------
LDA <- lda(Y ~ X1 + X2, data = train3)

train_LDA.table <- trainingerrors(train3, LDA, "")
test_LDA.table <- testingerrors(test3, LDA, "")

training_accuracy.lda <- accuracy_test(train_LDA.table)
testing_accuracy.lda <- accuracy_test(test_LDA.table)

sprintf("Training accuracy of LDA: %s",training_accuracy.lda)
sprintf("Training Error of LDA: %s", 1 - training_accuracy.lda)
print("Training Confusion Matrix LDA")
print(train_LDA.table)

sprintf("Testing accuracy of LDA: %s",testing_accuracy.lda)
sprintf("Testing Error of LDA: %s", 1 - testing_accuracy.lda)
print("Testing Confusion Matrix LDA")
print(test_LDA.table)

#---------------------QDA------------------------
QDA <- qda(Y ~ X1 + X2, data = train3)

training_qda.table <- trainingerrors(train3, QDA,"")
test_qda.table <- testingerrors(test3, QDA, "")

training_accuracy.qda <- accuracy_test(training_qda.table)
testing_accuracy.qda <- accuracy_test(test_qda.table)

sprintf("Training accuracy of QDA: %s",training_accuracy.qda)
sprintf("Training Error of QDA: %s", 1 - training_accuracy.qda)
print("Training Confusion Matrix QDA")
print(training_qda.table)

sprintf("Testing accuracy of QDA: %s",testing_accuracy.qda)
sprintf("Testing Error of LDA: %s", 1 - testing_accuracy.qda)
print("Testing Confusion Matrix QDA")
print(test_qda.table)

#---------------------Logistic Regression------------------------
LG <- glm(Y ~ X1 + X2, data = train3, family = binomial)

training_lg.table <- trainingerrors(train3, LG, "logistic")
test_lg.table <- testingerrors(test3, LG, "logistic")

training_accuracy.lg <- accuracy_test(training_lg.table)
testing_accuracy.lg <- accuracy_test(test_lg.table)

sprintf("Training accuracy of Logistic: %s",training_accuracy.lg)
sprintf("Training Error of Logistic: %s", 1 - training_accuracy.lg)
print("Training Confusion Matrix Logistic")
print(training_lg.table)

sprintf("Testing accuracy of Logistic: %s",testing_accuracy.lg)
sprintf("Testing Error of Logistic: %s", 1 - testing_accuracy.lg)
print("Testing Confusion Matrix Logistic")
print(test_lg.table)