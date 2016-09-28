library(rpart)
library(rpart.plot)
library(partykit)
library(ROCR)

source("R/analysis.R")
source("R/utilities.R")

data <- read.csv("data/dataset.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data.format(data)

male <- data[data$gender == 1, ]
male.train <- male[1001:nrow(male), ]
male.test <- male[1:1000, ]

male.tree <- dec_o ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o
male.rpart <- rpart(male.tree, data = male.train, method = "class")
male.rpart.extended <- rpart(male.tree, data = male.train, method = "class", control = rpart.control(cp = 0, maxdepth = 4))

male.rparty <- as.party(male.rpart)
plot(male.rparty, type = "simple")

male.roc <- prediction(predict(male.rpart, newdata = male.test, type = "prob")[, 2], male.test$dec_o)
plot(performance(male.roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

male.prediction <- predict(male.rpart, male.test, type = "class")
male.truth <- male.test$dec_o

male.confusion <- confusion.matrix(male.prediction, male.truth)

rpart.analysis(male.rpart)
confusion.analysis(male.confusion)
