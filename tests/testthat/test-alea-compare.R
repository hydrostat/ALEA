test_that("alea_compare fits multiple distributions for one method", {
  set.seed(1701)
  x <- ALEA:::r_gum_internal(80, c(xi = 10, alpha = 2))

  cmp <- alea_compare(
    x,
    distributions = c("gum", "gev", "pe3"),
    methods = "lmom"
  )

  expect_s3_class(cmp, "alea_compare")
  expect_equal(nrow(as.data.frame(cmp)), 3)
  expect_true(all(as.data.frame(cmp)$status == "ok"))
  expect_equal(length(cmp$fit_objects), 3)
  expect_s3_class(cmp$fit_objects[[1]], "alea_fit")
  expect_equal(nrow(cmp$errors), 0)
})

test_that("alea_fit preserves one-model behavior and delegates multi-model requests", {
  set.seed(1702)
  x <- ALEA:::r_gum_internal(80, c(xi = 10, alpha = 2))

  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  expect_s3_class(fit, "alea_fit")
  expect_equal(fit$distribution, "gum")
  expect_equal(fit$method, "lmom")

  cmp1 <- alea_fit(x, distribution = c("gum", "gev"), method = "lmom")
  expect_s3_class(cmp1, "alea_compare")
  expect_equal(nrow(as.data.frame(cmp1)), 2)

  cmp2 <- alea_fit(x, distribution = "gum", method = c("lmom", "mle"))
  expect_s3_class(cmp2, "alea_compare")
  expect_equal(nrow(as.data.frame(cmp2)), 2)

  cmp3 <- alea_fit(x, distribution = c("gum", "gev"), method = c("lmom", "mle"))
  expect_s3_class(cmp3, "alea_compare")
  expect_equal(nrow(as.data.frame(cmp3)), 4)
})

test_that("alea_compare supports summary, coefficients, and aligned parameter columns", {
  set.seed(1703)
  x <- ALEA:::r_gum_internal(80, c(xi = 10, alpha = 2))

  cmp <- alea_compare(
    x,
    distributions = c("gum", "ln2"),
    methods = "lmom"
  )

  smry <- summary(cmp)
  co <- coef(cmp)

  expect_s3_class(smry, "summary.alea_compare")
  expect_s3_class(co, "data.frame")
  expect_true(all(c("distribution", "method", "fit_index") %in% names(co)))
  expect_true(all(c("location", "scale", "shape") %in% names(co)))
  expect_true(anyNA(co$shape))
})

test_that("alea_compare records partial fit failures without stopping successful fits", {
  x <- c(rep(10, 20), 11, 12, 13, 14)

  cmp <- alea_compare(
    x,
    distributions = c("gum", "gpa"),
    methods = c("lmom", "mle")
  )

  expect_s3_class(cmp, "alea_compare")
  expect_true(nrow(as.data.frame(cmp)) >= 1)
  expect_true(length(cmp$fit_objects) >= 1)
  expect_true(all(c("distribution", "method", "step", "message", "class") %in% names(cmp$errors)))
})

test_that("alea_quantile works for alea_compare and aligns parameter columns", {
  set.seed(1704)
  x <- ALEA:::r_gum_internal(100, c(xi = 10, alpha = 2))

  cmp <- alea_compare(
    x,
    distributions = c("gum", "ln2"),
    methods = "lmom"
  )

  rl <- alea_quantile(cmp, return_period = c(10, 50))

  expect_s3_class(rl, "alea_quantile")
  expect_equal(nrow(rl), 4)
  expect_true(all(c("distribution", "method", "return_period", "quantile") %in% names(rl)))
  expect_true(all(c("location", "scale", "shape") %in% names(rl)))
  expect_true(anyNA(rl$shape))
})

test_that("alea_gof works for alea_compare", {
  set.seed(1705)
  x <- ALEA:::r_gum_internal(100, c(xi = 10, alpha = 2))

  cmp <- alea_compare(
    x,
    distributions = c("gum", "gev"),
    methods = "lmom"
  )

  gof <- alea_gof(cmp, statistics = c("ks", "aic"))

  expect_s3_class(gof, "alea_gof")
  expect_equal(nrow(gof), 4)
  expect_true(all(c("distribution", "method", "statistic", "estimate") %in% names(gof)))
  expect_setequal(unique(gof$statistic), c("ks", "aic"))
})

test_that("confint works for alea_compare and is reproducible", {
  set.seed(1706)
  x <- ALEA:::r_gum_internal(80, c(xi = 10, alpha = 2))

  cmp <- alea_compare(
    x,
    distributions = c("gum", "gev"),
    methods = "lmom"
  )

  ci1 <- confint(
    cmp,
    return_period = c(10, 50),
    n_boot = 10,
    seed = 123
  )

  ci2 <- confint(
    cmp,
    return_period = c(10, 50),
    n_boot = 10,
    seed = 123
  )

  expect_s3_class(ci1, "alea_quantile_ci")
  expect_equal(nrow(ci1), 4)
  expect_true(all(c("distribution", "method", "lower", "upper", "n_success", "n_failed") %in% names(ci1)))
  expect_equal(ci1$lower, ci2$lower)
  expect_equal(ci1$upper, ci2$upper)
})

test_that("plot.alea_compare returns ggplot objects", {
  skip_if_not_installed("ggplot2")

  set.seed(1707)
  x <- ALEA:::r_gum_internal(80, c(xi = 10, alpha = 2))

  cmp <- alea_compare(
    x,
    distributions = c("gum", "gev"),
    methods = "lmom"
  )

  p <- plot(cmp, type = "quantile", return_period = c(2, 5, 10, 25))
  expect_s3_class(p, "ggplot")
})
