test_that("beta shapes are correct", {
  values <- qbeta(c(0.025, 0.975), 5, 17)
  shapes <- calc_beta(x1 = values[1], p1 = 0.025, x2 = values[2], p2 = 0.975)
  expect_equal(shapes$shape1, 5, tolerance = 0.001)
  expect_equal(shapes$shape2, 17, tolerance = 0.001)

  values <- qbeta(c(0.055, 0.945), 15, 22)
  shapes <- calc_beta(x1 = values[1], p1 = 0.055, x2 = values[2], p2 = 0.945)
  expect_equal(shapes$shape1, 15, tolerance = 0.001)
  expect_equal(shapes$shape2, 22, tolerance = 0.001)
})

test_that("logit priors are correct", {
  parms <- logit_beta(5, 17, n = 1e5)
  parm_samp <- rnorm(1e5, mean = parms$location, sd = parms$scale)
  quants <- quantile(parm_samp, probs = c(0.025, 0.975))
  quants <- inv_logit(quants)
  probs <- as.double(gsub("%", "", names(quants))) / 100
  shapes <- calc_beta(x1 = quants[1], p1 = probs[1],
                      x2 = quants[2], p2 = probs[2])
  expect_equal(shapes$shape1, 5, tolerance = 1)
  expect_equal(shapes$shape2, 17, tolerance = 1)
})
