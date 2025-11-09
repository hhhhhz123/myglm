# Helper functions for logistic regression

# Compute probabilities from linear predictor
compute_probs <- function(beta, X) {
  eta <- X %*% beta
  p <- 1 / (1 + exp(-eta))
  return(as.vector(p))
}

# Compute score (gradient)
compute_score <- function(X, y, p) {
  score <- t(X) %*% (y - p)
  return(as.vector(score))
}

# Compute Fisher information
compute_fisher <- function(X, p) {
  W <- diag(as.vector(p * (1 - p)))
  fisher <- t(X) %*% W %*% X
  return(fisher)
}

# Check if converged
is_converged <- function(beta_old, beta_new, tol) {
  max(abs(beta_new - beta_old)) < tol
}
