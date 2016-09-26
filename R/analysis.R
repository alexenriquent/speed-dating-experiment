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
  cat(paste("Accuracy:", accuracy))
}