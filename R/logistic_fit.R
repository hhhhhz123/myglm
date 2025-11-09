#' Fit Binary Logistic Regression
#'
#' Fits binary logistic regression using IRWLS algorithm.
#'
#' @param X Matrix of predictors (n x p)
#' @param y Binary response vector (0 or 1)
#' @param max_iter Maximum iterations (default 100)
#' @param tol Convergence tolerance (default 1e-8)
#'
#' @return A list with:
#'   \item{coefficients}{Estimated coefficients}
#'   \item{fitted.values}{Fitted probabilities}
#'   \item{iterations}{Number of iterations}
#'   \item{converged}{Did it converge?}
#'   \item{X}{Design matrix with intercept}
#'   \item{y}{Response vector}
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(100), ncol = 2)
#' y <- rbinom(50, 1, 0.5)
#' fit <- logistic_fit(X, y)
#' print(fit)
#'
#' @export
logistic_fit <- function(X, y, max_iter = 100, tol = 1e-8) {

  # Input checks
  if (!is.matrix(X)) X <- as.matrix(X)
  if (!all(y %in% c(0, 1))) stop("y must be 0 or 1")
  if (nrow(X) != length(y)) stop("X and y must have same length")

  # Add intercept
  X <- cbind(1, X)

  # Initialize
  beta <- rep(0, ncol(X))

  # IRWLS algorithm
  for (iter in 1:max_iter) {

    # Compute probabilities
    p <- compute_probs(beta, X)

    # Compute score and Fisher info
    score <- compute_score(X, y, p)
    fisher <- compute_fisher(X, p)

    # Update beta
    beta_new <- beta + solve(fisher) %*% score

    # Check convergence
    if (is_converged(beta, beta_new, tol)) {
      beta <- beta_new
      converged <- TRUE
      break
    }

    beta <- beta_new
  }

  # Check if converged
  if (iter == max_iter) {
    converged <- FALSE
    warning("Did not converge")
  }

  # Final fitted values
  fitted <- compute_probs(beta, X)

  # Return results
  result <- list(
    coefficients = as.vector(beta),
    fitted.values = fitted,
    iterations = iter,
    converged = converged,
    X = X,
    y = y
  )

  class(result) <- "logistic_fit"
  return(result)
}


#' Print method
#' @param x A logistic_fit object
#' @param ... Not used
#' @export
print.logistic_fit <- function(x, ...) {
  cat("Binary Logistic Regression\n\n")
  cat("Coefficients:\n")
  coef_names <- c("(Intercept)", paste0("X", 1:(length(x$coefficients) - 1)))
  names(x$coefficients) <- coef_names
  print(x$coefficients)
  cat("\nConverged:", x$converged, "\n")
  cat("Iterations:", x$iterations, "\n")
}
