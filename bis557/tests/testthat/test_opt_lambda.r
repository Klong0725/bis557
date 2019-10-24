library(testthat)
library(MASS)
#library(bis557)
context("Test the output of optimizing lambda in homework 2.")

test_that("You opt_lambda() function works.", {
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

  # Use opt_lambda to find the optimal lambda
  fit_opt_lambda <- opt_lambda(X, y, n_folds = 5, lambda_vals = exp(seq(-2, 5, 0.1)))
  fit_cv_glmnet <- cv.glmnet(X, y, nfolds=5, lambda = exp(seq(-2, 5, 0.1)))
#
#   lambda_vals <- exp(seq(-2, 5, 0.1))
#   for (i in 1:length(lambda_vals)) {
#     fit_lm_ridge <- lm.ridge(y~.-1, data, lambda = lambda_vals[i])
#   }
  expect_equivalent(fit_cv_glmnet$lambda.min, fit_opt_lambda$lambda,
                    tolerance = 0.9)

})

