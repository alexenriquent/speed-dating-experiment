library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(partykit)
library(ROCR)

data.format <- function(data) {
  data$dec_o <- as.character(data$dec_o) 
  data$dec_o[data$dec_o == "1"] <- "yes"
  data$dec_o[data$dec_o == "0"] <- "no"
  data
}

data <- read.csv("dataset.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data.format(data)

training.data <- data[2001:nrow(data), ]
test.data <- data[1:2000, ]

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

tree <- dec_o ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o + gender
rpart <- rpart(tree, data = training.data, method = "class")

rparty <- as.party(rpart)
plot(rparty, type = "simple")

prediction <- predict(rpart, test.data, type = "class")
truth <- test.data$dec_o

roc <- prediction(predict(rpart, newdata = test.data, type = "prob")[, 2], test.data$dec_o)
plot(performance(roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

confusion <- confusion.matrix(prediction, truth)
confusion
