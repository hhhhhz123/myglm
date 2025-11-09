# myglm

Binary Logistic Regression using IRWLS Algorithm (Manual implementation of IRWLS algorithm with score function and Fisher information)

## Algorithm
1. Initialize β = 0
2. Repeat until convergence:
   - Compute fitted probabilities: p = 1/(1 + exp(-Xβ))
   - Compute score (gradient): S = X'(y - p)
   - Compute Fisher information: I = X'diag(p(1-p))X
   - Update: β_new = β + I^(-1)S
3. Check convergence: |β_new - β| < tolerance

## Installation
```r
# Install from GitHub
devtools::install_github("hhhhhz123/myglm", build_vignettes = TRUE)
```

## Main Functions

- `logistic_fit()` - Fit binary logistic regression using IRWLS
- `predict()` - Predict probabilities for new observations  
- `plot_fitted()` - Visualize fitted probabilities vs observed values
- `plot_comparison()` - Compare coefficients with `glm()`

## Quick Example
```r
library(myglm)

# Create data
set.seed(123)
X <- matrix(rnorm(100), ncol = 2)
y <- rbinom(50, 1, 0.5)

# Fit model
fit <- logistic_fit(X, y)
print(fit)

# Predict
pred <- predict(fit)
head(pred)

# Plot
plot_fitted(fit)
```

## Compare with glm()
```r
# Fit both
fit_ours <- logistic_fit(X, y)
fit_glm <- glm(y ~ X, family = binomial)

# Compare coefficients
all.equal(fit_ours$coefficients, coef(fit_glm))
```

## Documentation

View the vignette:
```r
browseVignettes("myglm")
```

## Author

He Zhang - Biostat 625, University of Michigan

