
#' k-nearest Neighbors Cross Validation.
#' @param  train an input data frame,
#' @param cl the true class value of the training data
#' @param k_nn an integer of the number of neighbors
#' @param  k_cv an integer of the number of folds.
#'
#' @return a list of two objects: \code{class}; a vector of predicted classes
#'   for each observation, and \code{cv_err}; the numeric misclassification
#'   error.
#'
#' @examples
#' my_knn_cv(my_penguins_train, my_penguins_class, 3, 5)
#' my_knn_cv(my_penguins_train, my_penguins_class, 5, 5)
#'
#' @keywords prediction
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){

  # Randomly assign each observation to one of the k_cv folds,
  # add fold as new variable to data.

  fold <- sample(rep(1:k_cv, length = nrow(train)))

  train_withFolds <- data.frame(train, "fold" = fold,
                                "species" = cl$species)

  # Initialize dataframe for model output.
  knn_predictions <- data.frame()

  # Iterates through each fold and adds their predicted classes to the
  # running list of all predictions.
  for (i in 1:k_cv){
    trainx <- dplyr::filter(train_withFolds, fold != i)
    testx <- dplyr::filter(train_withFolds, fold == i)

    clx <- trainx$species

    class <- as.data.frame(class::knn(trainx[, -6], testx[, -6], clx, k = k_nn))

    knn_predictions <- rbind(knn_predictions, class)
  }

  # Arrange training data for comparison with predicted output.
  sorted_folds <- dplyr::arrange(train_withFolds, fold)
  counter <- 0

  # Counts number of incorrect predictions, then calculate frequency of
  # incorrect predictions.
  for (j in 1:nrow(train)){
    if (sorted_folds[j, ncol(sorted_folds)] != knn_predictions[j,]){
      counter <- counter + 1
    }
    cv_error <- counter / nrow(train)
  }

  # Saves output as list object and return it.
  return_list <- list(knn_predictions, cv_error)
  return(return_list)
}
