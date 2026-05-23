test_that("Phase 13 GOF statistics match direct Gumbel recalculation", {
  x <- c(
    42.1, 39.4, 51.7, 48.3, 55.2,
    60.1, 46.8, 53.9, 58.4, 62.7,
    49.5, 57.8, 64.3, 52.6, 59.9,
    61.5, 67.2, 54.8, 63.1, 69.4
  )
  
  fit <- alea_fit(
    x,
    distribution = "gum",
    method = "lmom"
  )
  
  gof <- alea_gof(
    fit,
    statistics = c("ks", "cvm", "ad", "loglik", "aic", "bic")
  )
  
  params <- coef(fit)
  
  xi <- unname(params[["xi"]])
  alpha <- unname(params[["alpha"]])
  
  n <- length(x)
  n_parameters <- length(params)
  
  x_sorted <- sort(x)
  
  z_sorted <- (x_sorted - xi) / alpha
  u <- exp(-exp(-z_sorted))
  
  eps <- .Machine$double.eps
  u_clamped <- pmin(pmax(u, eps), 1 - eps)
  
  i <- seq_len(n)
  
  ks_reference <- max(
    max(i / n - u),
    max(u - (i - 1) / n)
  )
  
  cvm_reference <- sum((u - (2 * i - 1) / (2 * n))^2) + 1 / (12 * n)
  
  ad_reference <- -n - mean(
    (2 * i - 1) * (
      log(u_clamped) + rev(log1p(-u_clamped))
    )
  )
  
  z <- (x - xi) / alpha
  density <- (1 / alpha) * exp(-z - exp(-z))
  
  loglik_reference <- sum(log(density))
  
  aic_reference <- -2 * loglik_reference + 2 * n_parameters
  bic_reference <- -2 * loglik_reference + log(n) * n_parameters
  
  reference <- data.frame(
    statistic = c("ks", "cvm", "ad", "loglik", "aic", "bic"),
    estimate = c(
      ks_reference,
      cvm_reference,
      ad_reference,
      loglik_reference,
      aic_reference,
      bic_reference
    )
  )
  
  gof_df <- as.data.frame(gof)
  
  expect_s3_class(fit, "alea_fit")
  expect_s3_class(gof, "alea_gof")
  
  expect_setequal(
    gof_df$statistic,
    c("ks", "cvm", "ad", "loglik", "aic", "bic")
  )
  
  for (stat in reference$statistic) {
    observed <- gof_df$estimate[gof_df$statistic == stat]
    expected <- reference$estimate[reference$statistic == stat]
    
    expect_equal(
      observed,
      expected,
      tolerance = 1e-10,
      ignore_attr = TRUE
    )
  }
  
  expect_identical(
    as.character(gof_df$distribution),
    rep("gum", nrow(gof_df))
  )
  
  expect_identical(
    as.character(gof_df$method),
    rep("lmom", nrow(gof_df))
  )
  
  expect_true(all(is.finite(gof_df$estimate)))
  expect_true(all(gof_df$n == length(x)))
  expect_true(all(gof_df$n_parameters == n_parameters))
})