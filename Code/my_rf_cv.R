#' Random Forest Cross-Validation function
#'
#' This function does Cross-Validation Random Forest prediction.
#'
#' @param train A training data frame from my_penguins.
#' @param k_cv An int representing the number of folds in cv.
#' @keywords prediction
#'
#' @return A number of the mean rate of cross-validation the MSE of predictions.
#'
#' @examples
#' train <- na.omit(my_penguins) %>% dplyr::select(body_mass_g, bill_length_mm,
#'                                                 bill_depth_mm,
#'                                                 flipper_length_mm)
#' my_rf_cv(train, 5)
#'
#' @export
my_rf_cv <- function(train, k_cv) {
  n <- nrow(train)
  inds <- sample(rep(1: k_cv, length = n))
  train["fold"] <- inds
  MSEs <- 1: k_cv
  for (i in 1: k_cv) {
    data_train <- train %>% dplyr::filter(fold != i) %>% dplyr::select(!fold)
    data_test <- train %>% dplyr::filter(fold == i) %>% dplyr::select(!fold)
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm +
                                          bill_depth_mm + flipper_length_mm,
                                          data = data_train, ntree = 100)
    prediction <- predict(model, dplyr::select(data_test, !body_mass_g))
    MSEs[i] <- sum((prediction - data_test$body_mass_g) ^ 2) / length(prediction)
  }
  return(mean(MSEs))
}
