
#' Random Forest cross-validation for penguin data set.
#' @param k an integer of the number of folds.
#'
#' @return the numeric cross-validation error; the mean squared error
#'   of predicted and actual penguin body mass.
#'
#' @examples
#' my_rf_cv(2)
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k){

  # Randomly assign each observation to one of the k_cv folds,
  # add fold as new variable to data.

  penguins_train <- dplyr::select(my_penguins[c(-4, -272),],  3:6)

  penguins_class <- my_penguins[c(-4, -272), 1]

  fold <- sample(rep(1:k, length = nrow(penguins_train)))

  penguinsTrain_withFolds <- data.frame(penguins_train, "fold" = fold,
                                        "species" = penguins_class$species)

  rf_predictions <- data.frame()
  rf_formula <- body_mass_g ~ bill_length_mm + bill_depth_mm +
    flipper_length_mm

  # Iterate through each fold, training a random forest model of
  # 100 trees to predict body mass.
  for (i in 1:k){

    rf_train <- dplyr::filter(penguinsTrain_withFolds, fold != i)

    rf_test <- dplyr::filter(penguinsTrain_withFolds, fold == i)

    rf_model <- randomForest::randomForest(rf_formula,
                             data = rf_train, ntree = 100)

    rf_foldPredict <- predict(rf_model, rf_test[, -4])

    rf_predictions <- rbind(rf_predictions, rf_foldPredict)
  }

  # Arrange training data for comparison with predicted output.

  sorted_folds <- dplyr::arrange(penguinsTrain_withFolds, fold)

  # Initialize empty variables for loops.
  allFold_predictions <- vector()
  sumSquaredError <- 0

  # Combines predictions for each fold into a single vector for comparison.
  for (j in 1:k){
    allFold_predictions <- append(allFold_predictions,
                                  as.numeric(as.vector(rf_predictions[j, ])))
  }

  # Extract true body mass vector from fold-sorted penguin data.
  true_Mass <- as.numeric(as.vector(sorted_folds$body_mass_g))

  # Calculate MSE for predicted and actual body mass.
  for (m in 1:nrow(sorted_folds)){
    diff <- (allFold_predictions[m] - true_Mass[m])
    sumSquaredError <- sumSquaredError + (diff^2)
  }

  avg_MSE <- sumSquaredError / nrow(sorted_folds)

  # Return average MSE.
  return(avg_MSE)
}
