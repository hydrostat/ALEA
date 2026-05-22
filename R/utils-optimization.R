# Internal optimization helpers for maximum-likelihood estimation.

alea_bad_objective_internal <- function() {
  .Machine$double.xmax^0.25
}

safe_negloglik_internal <- function(x, para, density_fun) {
  log_density <- tryCatch(
    density_fun(x, para = para, log = TRUE),
    error = function(e) rep(-Inf, length(x))
  )

  if (length(log_density) != length(x) || any(!is.finite(log_density))) {
    return(alea_bad_objective_internal())
  }

  value <- -sum(log_density)

  if (!is.finite(value)) {
    alea_bad_objective_internal()
  } else {
    value
  }
}

make_mle_fit_result_internal <- function(para,
                                         distribution,
                                         opt,
                                         engine = "stats::optim") {
  check_finite_parameter_result(para, distribution, "mle")

  list(
    parameters = para,
    convergence = list(
      converged = isTRUE(opt$convergence == 0),
      method = "mle",
      engine = engine,
      message = opt$message %||% paste0(
        toupper(distribution),
        " parameters estimated by maximum likelihood."
      ),
      code = opt$convergence,
      value = opt$value
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}

mle_optim_internal <- function(par, fn, lower = -Inf, upper = Inf, ...) {
  stats::optim(
    par = par,
    fn = fn,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = list(maxit = 5000),
    ...
  )
}
