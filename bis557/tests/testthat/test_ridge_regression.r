library(testthat)
library(MASS)

context("Test the output of ridge regression function in homework 2.")

test_that("You ridge_regression() function works.", {
  # First simulate data for ridge regression
  set.seed(725)
  n <- 200
  p <- 4
  N <- 500
  M <- 20
  beta <- c(1, -1, 0.5, 0)
  mu <- rep(0, p)

  Sigma <- matrix(0.9, nrow = p, ncol = p)
  diag(Sigma) <- 1

  X <- MASS::mvrnorm(n, mu, Sigma)
  y <- X %*% beta + rnorm(n, sd = 5)
  data <- as.data.frame(cbind(y, X))
  colnames(data) <- c("y","x1","x2","x3","x4")

  fit_ridge_regression <- ridge_regression(X, y, lambda = 0.1)
  fit_lm_ridge <- lm.ridge(y~.-1, data, lambda = 0.1)

  expect_equivalent(fit_ridge_regression$coefficients, fit_lm_ridge$coef,
                    tolerance = 1e-1)
})

