data.format <- function(data) {
  data$dec_o <- as.character(data$dec_o) 
  data$dec_o[data$dec_o == "1"] <- "yes"
  data$dec_o[data$dec_o == "0"] <- "no"
  data
}