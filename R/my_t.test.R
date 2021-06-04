
#' My T-Test Function
#'
#' This function mimics R's \code{t.test()} function.
#' @param x, a numeric vector.
#' @param alternative, specifying the alternative hypothesis
#' @param mu, the null hypothesized mean.
#'
#' @return a table including the test statistic, degrees of freedom,
#' the alternative type, and the p-value for the test.
#'
#' @examples
#' my_t.test(rnorm(20, mean = 0, sd = 1), "two.sided", 1)
#' my_t.test(rnorm(20, mean = 0, sd = 1), "less", 3)
#'
#' @keywords inference
#'
#' @export
my_t.test <- function(x, alternative, mu){

  # Accepted inputs for 'alternative'.
  accepted_Alt <- c("two.sided", "less", "greater")

  # Checks value of 'alternative' input, with informative error.
  if (alternative %in% accepted_Alt == F){
    stop("Error, 'alternative' argument can only accept
         'two.sided', 'less', or 'greater'.")
  }

  # Calculate relevant metrics.
  s_Error <- sd(x) / sqrt(length(x))
  t_Statistic <- round((mean(x) - mu) / s_Error, 6)
  deg_Freedom <- length(x) - 1

  # Computes p-value depending on the value of 'alternative'.
  if (alternative == "two.sided"){
    p_Val <- 2 * pt(q = t_Statistic, df = deg_Freedom, lower.tail = T)
  } else if (alternative == "less"){
    p_Val <- pt(q = t_Statistic, df = deg_Freedom, lower.tail = T)
  } else {
    p_Val <- pt(q = t_Statistic, df = deg_Freedom, lower.tail = F)
  }

  p_Val <- round(p_Val, 4)

  # Creates a table of computed values for function output.
  test_Results <- list(t_Statistic, deg_Freedom,
                    alternative, p_Val)
  names(test_Results) <- c("test_stat", "df",
                           "alternative", "p_val")

  return(test_Results)
}
