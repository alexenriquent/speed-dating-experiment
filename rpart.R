library(rpart)
library(rpart.plot)
library(partykit)
library(ROCR)

data.format <- function(data) {
  data$dec_o <- as.character(data$dec_o) 
  data$dec_o[data$dec_o == "1"] <- "yes"
  data$dec_o[data$dec_o == "0"] <- "no"
  data
}

confusion.matrix <- function(prediction, truth) {
  xtab <- (table(prediction, truth))
  confusion <- matrix(rev(as.vector(xtab)), ncol = 2, byrow = TRUE)
  colnames(confusion) <- c("Pred:yes", "Pred:no")
  rownames(confusion) <- c("Actual:yes", "Actual:no")
  confusion <- as.table(confusion)
}

confusion.analysis <- function(matrix) {
  tp <- matrix[1, 1]
  fn <- matrix[1, 2]
  fp <- matrix[2, 1]
  tn <- matrix[2, 2]
  
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
}

data <- read.csv("data/dataset.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data.format(data)

train <- data[2001:nrow(data), ]
test <- data[1:2000, ]

tree <- dec_o ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o
rpart <- rpart(tree, data = train, method = "class")

rparty <- as.party(rpart)
plot(rparty, type = "simple")

prediction <- predict(rpart, test, type = "class")
truth <- test$dec_o

roc <- prediction(predict(rpart, newdata = test, type = "prob")[, 2], test$dec_o)
plot(performance(roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

confusion <- confusion.matrix(prediction, truth)
confusion

male <- data[data$gender == 1, ]
male.train <- male[1001:nrow(male), ]
male.test <- male[1:1000, ]

male.tree <- dec_o ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o
male.rpart <- rpart(male.tree, data = male.train, method = "class")

male.rparty <- as.party(male.rpart)
plot(male.rparty, type = "simple")

male.prediction <- predict(male.rpart, male.test, type = "class")
male.truth <- male.test$dec_o

male.roc <- prediction(predict(male.rpart, newdata = male.test, type = "prob")[, 2], male.test$dec_o)
plot(performance(male.roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

male.confusion <- confusion.matrix(male.prediction, male.truth)
male.confusion

female <- data[data$gender == 0, ]
female.train <- female[1001:nrow(female), ]
female.test <- female[1:1000, ]

female.tree <- dec_o ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o
female.rpart <- rpart(female.tree, data = female.train, method = "class")

female.rparty <- as.party(female.rpart)
plot(female.rparty, type = "simple")

female.prediction <- predict(female.rpart, female.test, type = "class")
female.truth <- female.test$dec_o

female.roc <- prediction(predict(female.rpart, newdata = female.test, type = "prob")[, 2], female.test$dec_o)
plot(performance(female.roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

female.confusion <- confusion.matrix(female.prediction, female.truth)
female.confusion
