#' Compute log-likelihood for linear model
log_likelihood <- function(design, beta, outcome, noise_var = 1){
  residual = outcome - design %*% beta
  return(-crossprod(residual)/(2*noise_var))
}

#' Compute numeric gradient of log-likelihood for linear model
numeric_grad <- function(design, beta, outcome, noise_var = 1){
  residual = outcome - design %*% beta
  return(crossprod(design, residual)/noise_var)
}

#' Optimization via BFGS
optim_bfgs <- function(init_beta, design, outcome, noise_var = 1){
  optim_result <- optim(init_beta, log_likelihood, numeric_grad,
                        design=design, outcome=outcome, method='BFGS')
  return(optim_result$par)
}
