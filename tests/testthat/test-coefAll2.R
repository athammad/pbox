library(testthat)        # load testthat package
library(pbox)

# Test case for extracting coefficients without deviance
test_that("coefficients are correctly extracted without deviance", {
  # Create a mock object similar to what coefAll2 would expect
  mock_obj <- list(parameters = c("mu", "sigma", "nu", "tau"),
                   mu = 0.5, sigma = 0.1, nu = 2, tau = 3)
  class(mock_obj) <- "mockModel" # Assume there's an appropriate class for this object

  # Expected output
  expected <- list(mu = 0.5, sigma = 0.1, nu = 2, tau = 3)

  # Run the coefAll2 function
  result <- coefAll2(mock_obj)

  # Test if the result matches the expected output
  expect_equal(result, expected)
})

# Test case for extracting coefficients with deviance
test_that("coefficients and deviance are correctly extracted with deviance", {
  # Add a method to compute deviance for mockModel
  deviance.mockModel <- function(obj) {
    return(0.01)  # Mocked deviance value
  }

  # Create a mock object similar to what coefAll2 would expect, with deviance
  mock_obj <- list(parameters = c("mu", "sigma", "nu", "tau"),
                   mu = 0.5, sigma = 0.1, nu = 2, tau = 3)
  class(mock_obj) <- "mockModel"

  # Expected output with deviance
  expected <- list(mu = 0.5, sigma = 0.1, nu = 2, tau = 3, deviance = 0.01)

  # Run the coefAll2 function with deviance = TRUE
  result <- coefAll2(mock_obj, deviance = TRUE)

  # Test if the result matches the expected output with deviance
  expect_equal(result, expected)
})
