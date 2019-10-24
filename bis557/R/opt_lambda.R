#--------------hw2--------------#
#-----------Keyi Long-----------#
#--------Due: 2019/10/23--------#

#' Implment cross validation for optimizing the ridge parameter
#'
#' @description This function can select the optimal lambda in ridge regression and export coefficients
#' @import stats
#' @import MASS
#' @import glmnet
#' @param X numeric data matrix of predictors in a ridge regression model
#' @param y the response vector
#' @param n_folds the number of cross validation folds
#' @param lambda_vals a list of hyperparameter that controls the penalty term
#' @return The optimal lambda and coefficients of the fitted ridge regression model
#'
#' @export

opt_lambda <- function(X, y, n_folds = 5, lambda_vals = exp(seq(-2, 5, 0.1))){
  N <-nrow(X)
  sub_test <- split(sample(1:N, replace = FALSE), 1:n_folds)
  error <- NULL
  mse <- NULL

  for (i in 1:length(lambda_vals)) {
    error_sum <- 0
    for (k in 1:n_folds) {
      # use train set to fit ridge regression
      fit_ridge <- ridge_regression(X[-sub_test[[k]],], y[-sub_test[[k]],], lambda_vals[k])$coefficients
      # use test set to calculate error
      y_hat <- X[sub_test[[k]],] %*% fit_ridge
      error_sum <- error_sum + (y_hat - y[sub_test[[k]],])^2
    }
    mse <- error_sum/n_folds
  }

  opt_lambda <- lambda_vals[which.min(mse)]
  beta <- ridge_regression(X, y, opt_lambda)$coefficients

  # Obtain results
  ret <- list(coefficients = beta, lambda = opt_lambda)
  return(ret)
}

