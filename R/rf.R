library(randomForest)
library(ROCR)

source("R/analysis.R")

data <- read.csv("data/dataset.csv", header = TRUE, stringsAsFactors = FALSE)

train <- data[2001:nrow(data), ]
test <- data[1:2000, ]

rf <- randomForest(as.factor(dec_o) ~ attr_o + intel_o + sinc_o + fun_o + amb_o + shar_o, data = train, importance = TRUE)

plot(rf, log = "y")
legend("top", colnames(rf$err.rate), col = 1:3, cex = 0.8, fill = 1:3)
varImpPlot(rf)

roc <- prediction(predict(rf, newdata = test, type = "prob")[, 2], test$dec_o)
plot(performance(roc, "tpr", "fpr"), colorize = TRUE)
abline(0, 1, lty = 3)

prediction <- predict(rf, test)
truth <- test$dec_o

confusion <- confusion.matrix(prediction, truth)

rf.analysis(rf)  
confusion.analysis(confusion)
