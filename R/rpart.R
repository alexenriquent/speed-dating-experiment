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
rpart <- rpart(tree, data = train, method = "class")
rpart.extended <- rpart(tree, data = train, method = "class", control = rpart.control(cp = 0, maxdepth = 4))

rparty <- as.party(rpart)
plot(rparty, type = "simple")

roc <- prediction(predict(rpart, newdata = test, type = "prob")[, 2], test$dec_o)
plot(performance(roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

prediction <- predict(rpart, test, type = "class")
truth <- test$dec_o

confusion <- confusion.matrix(prediction, truth)

rpart.analysis(rpart)
confusion.analysis(confusion)
