#--------------hw2--------------#
#-----------Keyi Long-----------#
#--------Due: 2019/10/23--------#

# Implement a ridge regression that takes into account colinear (or nearly colinear) regression variables.

#' Implement ridge regression
#'
#' @description This function can conduct ridge regression and export coefficients
#' @import stats
#' @import MASS
#' @param X numeric data matrix of predictors in a ridge regression model
#' @param y the response vector
#' @param lambda a hyperparameter that controls the penalty term
#' @return Components of the fitted ridge regression model
#'
#' @export


ridge_regression <- function(X, y, lambda=0) {
  # Singular Value Decomposition(SVD)
  svd_result <- svd(X)
  U_matrix <- svd_result$u
  V_matrix <- svd_result$v
  d <- svd_result$d

  # construct D as a diagnal matrix
  D_matrix <- diag(d/(d^2 + lambda))

  # solve beta
  beta <- V_matrix%*%D_matrix%*%t(U_matrix)%*%y
  rownames(beta) <- colnames(X)

  # obtain output: components of ridge regression
  output <- list(coefficients = beta, lambda = lambda)
  class(output) <- "ridge_regression"
  return(output)
}

