library(rpart)
library(rpart.plot)
library(partykit)
library(ROCR)

source("R/analysis.R")
source("R/utilities.R")

data <- read.csv("data/dataset.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data.format(data)

female <- data[data$gender == 0, ]
female.train <- female[1001:nrow(female), ]
female.test <- female[1:1000, ]

female.tree <- dec_o ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o
female.rpart <- rpart(female.tree, data = female.train, method = "class", control = rpart.control(cp = 0, maxdepth = 4))
female.rpart.pruned <- rpart(female.tree, data = female.train, method = "class")

female.prediction <- predict(female.rpart.pruned, female.test, type = "class")
female.truth <- female.test$dec_o

female.roc <- prediction(predict(female.rpart.pruned, newdata = female.test, type = "prob")[, 2], female.test$dec_o)
plot(performance(female.roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

female.rparty <- as.party(female.rpart.pruned)
plot(female.rparty, type = "simple")

female.confusion <- confusion.matrix(female.prediction, female.truth)
female.confusion
