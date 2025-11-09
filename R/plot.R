# NOTE: ggplot2 uses non-standard evaluation, which causes R CMD check to give a NOTE about undefined variables.
# https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
utils::globalVariables(c("Index", "Probability", "Observed", "Coefficient", "Value", "Method"))

#' Plot Fitted Values
#'
#' Plot fitted probabilities vs observed values.
#'
#' @param fit A logistic_fit object
#'
#' @return A ggplot object
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(100), ncol = 2)
#' y <- rbinom(50, 1, 0.5)
#' fit <- logistic_fit(X, y)
#' plot_fitted(fit)
#'
#' @import ggplot2
#' @export
plot_fitted <- function(fit) {

  df <- data.frame(
    Index = 1:length(fit$y),
    Probability = fit$fitted.values,
    Observed = factor(fit$y, labels = c("Class 0", "Class 1"))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Index, y = Probability, color = Observed)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed") +
    ggplot2::labs(title = "Fitted Probabilities", x = "Index", y = "Probability") +
    ggplot2::theme_minimal()
}


#' Compare with GLM
#'
#' Compare coefficients with glm().
#'
#' @param fit_ours Our logistic_fit object
#' @param fit_glm A glm object
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
plot_comparison <- function(fit_ours, fit_glm) {

  coef_ours <- fit_ours$coefficients
  coef_glm <- stats::coef(fit_glm)

  n <- length(coef_ours)
  df <- data.frame(
    Coefficient = rep(c("Intercept", paste0("X", 1:(n-1))), 2),
    Value = c(coef_ours, coef_glm),
    Method = rep(c("Ours", "GLM"), each = n)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Coefficient, y = Value, fill = Method)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "Coefficient Comparison") +
    ggplot2::theme_minimal()
}
