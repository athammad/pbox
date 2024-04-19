


probCI <- function(probabilities, alpha = 0.05) {
  # Calculate the confidence interval around the probabilities
  lower <- quantile(probabilities, alpha / 2)
  upper <- quantile(probabilities, 1 - alpha / 2)
  c(lower, upper)
}
