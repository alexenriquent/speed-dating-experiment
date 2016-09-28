library(randomForest)
library(ROCR)

source("R/analysis.R")

data <- read.csv("data/dataset.csv", header = TRUE, stringsAsFactors = FALSE)

female <- data[data$gender == 0, ]
female.train <- female[1001:nrow(female), ]
female.test <- female[1:1000, ]

female.rf <- randomForest(as.factor(dec_o) ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o, data = train, importance = TRUE)

plot(female.rf, log = "y")
varImpPlot(female.rf)

female.roc <- prediction(predict(female.rf , newdata = female.test, type = "prob")[, 2], female.test$dec_o)
plot(performance(female.roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

female.prediction <- predict(female.rf, female.test)
female.truth <- female.test$dec_o

female.confusion <- confusion.matrix(female.prediction, female.truth)

rf.analysis(female.rf)
confusion.analysis(female.confusion)
