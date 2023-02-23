testthat::test_that("linalg and optim least-sq coincide", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = 'linear')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'linear', option = list(mle_solver = 'BFGS')
  )
  testthat::expect_true(are_all_close(
    via_linalg_out$coef, via_bfgs_out$coef, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

# Test analytic gradient calculation by comparing it against a numerical one
testthat::test_that("Numerical and my gradients least-sq coincide", {

  #' Calculate analytic gradient
  approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3),...) {
    numerical_grad <- rep(0, length(x))
    e <- diag(x=dx, nrow=length(x))
    for (i in 1:length(x)){
      numerical_grad[i] <- (func(x+e[i,],...)-func(x-e[i,],...))/(2*dx)
    }
    return(numerical_grad)
  }

  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  beta <- data$coef_true
  grad_analytic <-
    grad_numeric <- approx_grad(log_likelihood, beta,
                                design = design, outcome = outcome)
  testthat::expect_true(are_all_close(grad_analytic, grad_numeric,
                                      abs_tol = 1e-2, rel_tol = 1e-2))
})
