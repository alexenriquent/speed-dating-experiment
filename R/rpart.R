library(rpart)
library(rpart.plot)
library(partykit)
library(ROCR)

source("R/analysis.R")
source("R/utilities.R")

data <- read.csv("data/dataset.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data.format(data)

train <- data[2001:nrow(data), ]
test <- data[1:2000, ]

tree <- dec_o ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o
rpart <- rpart(tree, data = train, method = "class", control = rpart.control(cp = 0, maxdepth = 4))
rpart.pruned <- rpart(tree, data = train, method = "class")

prediction <- predict(rpart.pruned, test, type = "class")
truth <- test$dec_o

roc <- prediction(predict(rpart.pruned, newdata = test, type = "prob")[, 2], test$dec_o)
plot(performance(roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

rparty <- as.party(rpart.pruned)
plot(rparty, type = "simple")

confusion <- confusion.matrix(prediction, truth)

rpart.analysis(rpart.pruned)
confusion.analysis(confusion)
