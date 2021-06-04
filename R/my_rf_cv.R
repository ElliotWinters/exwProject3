
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

  penguins_train <- dplyr::select(my_penguins[c(-4, -272),],  3:6)

  penguins_class <- my_penguins[c(-4, -272), 1]

  fold <- sample(rep(1:k, length = nrow(penguins_train)))

  penguinsTrain_withFolds <- data.frame(penguins_train, "fold" = fold,
                                        "species" = penguins_class$species)

  rf_predictions <- list()
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

    rf_predictions[[i]] <- rf_foldPredict
  }

  # Arrange training data for comparison with predicted output.
  sorted_folds <- dplyr::arrange(penguinsTrain_withFolds, fold)

  # Create a list separating each observation by its assigned fold.
  mse_byFold <- list()
  for (n in 1:k){
    mse_byFold[[n]] <- dplyr::filter(sorted_folds, fold == n)
  }

  # Column-bind folds to each list of predictions.
  for (a in 1:k){
    current_Fold <- dplyr::filter(sorted_folds, fold == a)
    rf_predictions[[a]] <- cbind(rf_predictions[[a]], current_Fold$fold)
  }

  # Initialize variables for loops.
  avg_MSE <- 0
  foldSquaredError <- 0
  sumSquaredError <- 0

  # For each fold, calculate squared error as the squared difference
  # between predicted and actual body mass.
  for (x in 1:k){
    for (y in 1:nrow(rf_predictions[[x]])){
      diff <- (rf_predictions[[x]][y] - mse_byFold[[x]][y, 4])
      foldSquaredError <-(diff^2)
    }
    sumSquaredError <- sumSquaredError + foldSquaredError
  }

  # Calculate average MSE; total MSE divided by number of folds.
  avg_MSE <- sumSquaredError / nrow(penguins_train)
}
