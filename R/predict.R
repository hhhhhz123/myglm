#' Predict from Logistic Fit
#'
#' Predict probabilities from fitted model.
#'
#' @param object A logistic_fit object
#' @param newdata New data matrix (optional)
#' @param ... Not used
#'
#' @return Vector of predicted probabilities
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(100), ncol = 2)
#' y <- rbinom(50, 1, 0.5)
#' fit <- logistic_fit(X, y)
#' pred <- predict(fit)
#'
#' @export
predict.logistic_fit <- function(object, newdata = NULL, ...) {

  if (is.null(newdata)) {
    return(object$fitted.values)
  }

  # Add intercept to new data
  if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
  newdata <- cbind(1, newdata)

  # Predict
  pred <- compute_probs(object$coefficients, newdata)
  return(pred)
}
