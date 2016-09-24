library(corrplot)

data <- read.csv("dataset.csv", header = TRUE, stringsAsFactors = FALSE)

attributes <- data[, 25:30]
male_attributes <- data[data$gender == 1, 25:30]
female_attributes <- data[data$gender == 0, 25:30]

correlation <- cor(attributes, use = "complete.obs")
male_correlation <- cor(male_attributes, use = "complete.obs")
female_correlation <- cor(female_attributes, use = "complete.obs")

plot.correlation <- function(correlation, title) {
  corrplot(correlation, method = "shade", title = title, 
           order = "hclust", addrect = 2, mar = c(0, 0, 2, 0), 
           tl.col = "black", addCoef.col = "black")
}

plot.correlation(correlation, "Attribute Correlation")
plot.correlation(male_correlation, "Men's Attributes (Rated by Female Participants)")
plot.correlation(female_correlation, "Women's Attributes (Rated by Male Participants)")

correlation_diff <- max(abs(male_correlation - female_correlation))
print(correlation_diff)