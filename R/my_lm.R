
#' My linear model function.
#'
#' This function mimics R's \code{lm()} function.
#' @param formula - a regression formula
#' @param data - a data set.
#'
#' @return a table with coefficient estimates, standard errors,
#' t-values, and t-test results for each variable.
#'
#' @examples
#' my_lm(mpg ~ hp, mtcars)
#' my_lm(mpg ~ wt, mtcars)
#'
#' @export
my_lm <- function(formula, data){

  # Extract relevant data as matrices.
  X_extract <- model.matrix(formula, data)
  extract_Frame <- model.frame(formula, data)
  Y_extract <- model.response(extract_Frame)

  # Compute regression coefficients.
  beta_hat <- solve( t(X_extract) %*% X_extract ) %*%
    t(X_extract) %*% Y_extract

  # Initialize table for function output.
  return_Table <- matrix(NA, ncol = 4, nrow = nrow(beta_hat))
  colnames(return_Table) <- c("Estimate", "Std. Error",
                              "t value", "Pr(>|t|)")
  row.names(return_Table) <- row.names(beta_hat)

  # Compute degrees of freedom.
  df <- nrow(X_extract) - ncol(extract_Frame)

  # Transpose beta_hat coefficients.
  t_betahat <- t(beta_hat)

  # Create matrix of equal dimensions to X_extract, fill with
  # values from t_beta_hat.
  t_betahat_long <- matrix(ncol = ncol(t_betahat),
                           nrow = nrow(X_extract))

  for (col in 1:ncol(t_betahat_long)){
    t_betahat_long[, col] <- rep(t_betahat[col], nrow(t_betahat))
  }

  # Matrix-multiply X and beta, create 'sum' column that sums each row.
  xTimesBeta <- as.data.frame(t_betahat_long * X_extract)
  xTimesBetaWithSums <- dplyr::mutate(.data = xTimesBeta, sum = NA)

  for (row in 1:nrow(xTimesBetaWithSums)){
    xTimesBetaWithSums[row, ncol(xTimesBetaWithSums)] <- sum(as.vector(as.numeric(xTimesBeta[row,])))
  }

  # Compute variance.
  YminusBetaX <- Y_extract - xTimesBetaWithSums$sum
  YminusBetaXSquared <- YminusBetaX^2
  sum_forVar_Z <- sum(YminusBetaXSquared) / df

  # Compute standard error.
  xtx_Z <- solve( t(X_extract) %*% X_extract )
  termToBeRooted_Z <- xtx_Z*sum_forVar_Z

  # Apply computed values to return table.
  return_Table[,1] <- beta_hat[,1]
  return_Table[,2] <- diag(sqrt(abs(termToBeRooted_Z)))
  return_Table[,3] <- return_Table[,1] / return_Table[,2]
  return_Table[,4] <- 2*pt(abs(return_Table[,3]), df = df, lower.tail = F)

  return(return_Table)

}
