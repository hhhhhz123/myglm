test_that("logistic_fit works", {
  set.seed(123)
  X <- matrix(rnorm(100), ncol = 2)
  y <- rbinom(50, 1, 0.5)

  fit <- logistic_fit(X, y)

  expect_true(fit$converged)
  expect_equal(length(fit$coefficients), 3)
})

test_that("matches glm", {
  set.seed(456)
  X <- matrix(rnorm(100), ncol = 2)
  y <- rbinom(50, 1, 0.5)

  fit_ours <- logistic_fit(X, y)
  fit_glm <- glm(y ~ X, family = binomial)

  expect_equal(as.numeric(fit_ours$coefficients), as.numeric(coef(fit_glm)), tolerance = 1e-5)})

test_that("predict works", {
  set.seed(789)
  X <- matrix(rnorm(100), ncol = 2)
  y <- rbinom(50, 1, 0.5)

  fit <- logistic_fit(X, y)
  pred <- predict(fit)

  expect_length(pred, 50)
  expect_true(all(pred >= 0 & pred <= 1))
})
