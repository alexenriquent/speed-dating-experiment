confusion.matrix <- function(prediction, truth) {
  xtab <- (table(prediction, truth))
  confusion <- matrix(rev(as.vector(xtab)), ncol = 2, byrow = TRUE)
  colnames(confusion) <- c("Pred:yes", "Pred:no")
  rownames(confusion) <- c("Actual:yes", "Actual:no")
  confusion <- as.table(confusion)
}

confusion.analysis <- function(matrix) {
  TP <- matrix[1, 1]
  FN <- matrix[1, 2]
  FP <- matrix[2, 1]
  TN <- matrix[2, 2]

  ACC <- (TP + TN) / (TP + TN + FP + FN)
  TPR <- TP / (TP + FN)
  TNR <- TN / (FP + TN)
  FPR <- FP / (FP + TN)
  FNR <- FN / (FN + TP)
  PPV <- TP / (TP + FP)
  NPV <- TN / (TN + FN)
  FDR <- FP / (FP + TP)
  F1 <- (2 * TPR * PPV) / (TPR + PPV)
  
  cat('\n')
  print(matrix)
  cat(paste("\nAccuracy:\t\t   ", ACC,
            "\nSensitivity/Recall:\t   ", TPR,
            "\nSpecificity:\t\t   ", TNR,
            "\nFall-out:\t\t   ", FPR,
            "\nMiss rate:\t\t   ", FNR,
            "\nPrecision:\t\t   ", PPV,
            "\nNegative predictive value: ", NPV,
            "\nFalse discovery rate:\t   ", FDR,
            "\nF-measure:\t\t   ", F1, '\n'))
}

rpart.analysis <- function(rpart) {
  cat('\n')
  print(rpart)
  cat('\n')
}