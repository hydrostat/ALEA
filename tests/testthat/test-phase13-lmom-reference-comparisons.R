test_that("Phase 13 lmom reference return levels match ALEA-R outputs", {
  skip_if_not_installed("lmom")
  
  x <- c(
    42.1, 39.4, 51.7, 48.3, 55.2,
    60.1, 46.8, 53.9, 58.4, 62.7,
    49.5, 57.8, 64.3, 52.6, 59.9,
    61.5, 67.2, 54.8, 63.1, 69.4
  )
  
  return_period <- c(2, 5, 10, 25, 50, 100)
  probability <- 1 - 1 / return_period
  
  compare_lmom_reference <- function(distribution, fit_fun, qua_fun) {
    fit <- alea_fit(
      x,
      distribution = distribution,
      method = "lmom"
    )
    
    rl <- alea_return_level(
      fit,
      return_period = return_period
    )
    
    lmoments <- lmom::samlmu(x)
    para <- fit_fun(lmoments)
    reference <- qua_fun(probability, para)
    
    expect_s3_class(fit, "alea_fit")
    expect_s3_class(rl, "alea_return_level")
    
    expect_identical(
      as.character(rl$distribution),
      rep(distribution, length(return_period))
    )
    
    expect_identical(
      as.character(rl$method),
      rep("lmom", length(return_period))
    )
    
    expect_equal(
      rl$probability,
      probability,
      tolerance = 1e-12,
      ignore_attr = TRUE
    )
    
    expect_equal(
      rl$return_level,
      reference,
      tolerance = 1e-8,
      ignore_attr = TRUE
    )
    
    expect_true(all(is.finite(rl$return_level)))
    expect_true(all(diff(rl$return_level) > 0))
  }
  
  compare_lmom_reference(
    distribution = "gev",
    fit_fun = lmom::pelgev,
    qua_fun = lmom::quagev
  )
  
  compare_lmom_reference(
    distribution = "gpa",
    fit_fun = lmom::pelgpa,
    qua_fun = lmom::quagpa
  )
  
  compare_lmom_reference(
    distribution = "gum",
    fit_fun = lmom::pelgum,
    qua_fun = lmom::quagum
  )
  
  compare_lmom_reference(
    distribution = "pe3",
    fit_fun = lmom::pelpe3,
    qua_fun = lmom::quape3
  )
})