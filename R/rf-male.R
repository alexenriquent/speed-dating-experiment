library(randomForest)
library(ROCR)

source("R/analysis.R")

data <- read.csv("data/dataset.csv", header = TRUE, stringsAsFactors = FALSE)

male <- data[data$gender == 1, ]
male.train <- male[1001:nrow(male), ]
male.test <- male[1:1000, ]

male.rf <- randomForest(as.factor(dec_o) ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o, data = train, importance = TRUE)

plot(male.rf, log = "y")
varImpPlot(male.rf)

male.roc <- prediction(predict(male.rf , newdata = male.test, type = "prob")[, 2], male.test$dec_o)
plot(performance(male.roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

male.prediction <- predict(male.rf, male.test)
male.truth <- male.test$dec_o

male.confusion <- confusion.matrix(male.prediction, male.truth)

rf.analysis(male.rf)
confusion.analysis(male.confusion)
