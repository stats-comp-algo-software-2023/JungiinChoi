#' @export

hiper_glm <- function(design, outcome, model = "linear", solver = NULL){

  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }

  hglm_out <- list()
  class(hglm_out) <- "hglm"

  if (model == "linear"){
    # via pseudo-inverse (X'X)^{-1}X'Y
    beta = solve(crossprod(design), crossprod(design, outcome))

    if (solver == "BFGS"){
      init_beta = beta
      # via BFGS
      beta = optim_bfgs(init_beta, design, outcome)
    }
  } else if (model == "logit"){
    # not yet implemented
  }
  hglm_out$coef = beta
  hglm_out$fitted.values = crossprod(t(design), beta)
  hglm_out$residuals = outcome - hglm_out$fitted.values
  return(hglm_out)
}
