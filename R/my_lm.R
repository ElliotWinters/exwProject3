
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
  
  df <- nrow(X_extract) - ncol(extract_Frame)
  
  t_betahat <- t(beta_hat)
  
  # Multiply each element in the X matrix by its coefficient.
  X_extract2 <- X_extract
  
  for (i in 1:(ncol(X_extract))){
    X_extract2[,i] <- X_extract[,i]*t_betahat[1,i]
  }
  
  # Duplicate Y columns so as to match dimensions of X.
  Y_extract2 <- matrix(nrow = length(Y_extract),
                       ncol = ncol(X_extract))
  for (col in 1:ncol(Y_extract2)){
    Y_extract2[,col] <- Y_extract
  }
  
  # Compute variance.
  YMinusBetaX_Z <- Y_extract2 - X_extract2 - t_betahat[,1]
  YMinusBetaXSquared_Z <- (YMinusBetaX_Z)^2
  sum_forVar_Z <- sum(YMinusBetaXSquared_Z[,-1]) / df 
  
  # Compute standard error. 
  xtx_Z <- solve( t(X_extract) %*% X_extract )
  termToBeRooted_Z <- xtx_Z*sum_forVar_Z
  diag(sqrt(termToBeRooted_Z))
  
  # Apply computed values to return table.
  return_Table[,1] <- beta_hat[,1]
  return_Table[,2] <- diag(sqrt(termToBeRooted_Z))
  return_Table[,3] <- return_Table[,1] / return_Table[,2]
  return_Table[,4] <- 2*pt(abs(return_Table[,3]), df = df, lower.tail = F)
  
  return(return_Table)
  
}