context("Exact prediction interval")

installed_packages <- rownames(installed.packages())
check_EnvStats <- function() {
  if (!is.element("EnvStats", installed_packages)) {
    skip("EnvStats package not found")
  }
}
check_statisticalIntervals <- function() {
  if (!is.element("statisticalIntervals", installed_packages)) {
    skip("statisticalIntervals package not found")
  }
}

test_that("Dunnett's K factor - two-sided", {
  check_EnvStats()
  expect_equal(predictionFactor(n=10, k=3),
               EnvStats:::Dunnetts.K(n=10, k=3, m=1))
  #
  expect_equal(predictionFactor(10, method="exact"),
               EnvStats::predIntNormK(10, method="exact"))
  #
  x <- rnorm(10)
  interval_EnvStats <- EnvStats::predIntNorm(x, k=2, method="exact")
  expect_equal(predictionInterval(x, k=2, method="exact"),
               unname(interval_EnvStats$interval$limits),
               check.attributes = FALSE,
               tolerance = 1e-5)
})

test_that("Prediction interval - comparison statisticalIntervals", {
  check_statisticalIntervals()
  x <- rnorm(10)
  # two-sided
  I1 <- statisticalIntervals::calculate_interval(x, type="prediction",
                                                 futureObs=3, conf.level=0.95)
  I2 <- predictionInterval(x, k=3, method="Bonferroni")
  expect_equal(I1, I2)
  # one-sided
  J1 <- statisticalIntervals::calculate_interval(x, type="prediction",
                                                 futureObs=3, side="upper",
                                                 conf.level=0.95)
  J2 <- predictionInterval(x, k=3, method="Bonferroni", type="upper")
  expect_equal(J1[2], J2)
  ## mean prediction
  # one-sided
  J3 <- statisticalIntervals::calculate_interval(x,
                                                 type="meanprediction",
                                                 futureObs=3,
                                                 conf.level=0.95,
                                                 side="upper")
  J4 <- predictionInterval(x, n.mean=3, type="upper")
  expect_equal(J3[2],J4)
  # two-sided
  I3 <- statisticalIntervals::calculate_interval(x, type="meanprediction", futureObs=3, conf.level=0.95)
  I4 <- predictionInterval(x, n.mean=3)
  expect_equal(I3,I4)
})

test_that("Prediction intervals - Coverage", {
  set.seed(666)
  coverage <- CoverageEPI(10000, conf=0.8, n=c(5,10), k=c(2,3))$coverage
  expect_equal(coverage, rep(0.8,4), tolerance=1e-2)
})

# test_that("Dunnett's K1", {
#   test_KF1 <- function(x, k, rho){
#     V <- matrix(rho, nrow=k, ncol=k)
#     diag(V) <- 1
#     mnormt::pmnorm(x, varcov=V)
#   }
#   expect_equal(Dunnetts_KF1(0, 2, 1/3), test_KF1(0, 2, 1/3))
#   # http://stats.stackexchange.com/questions/77692/expected-value-of-minimum-order-statistic-from-a-normal-sample
#   # http://www.jstor.org/stable/2238463
#   gupta <- c(0.5, 0.30409, 0.20613, 0.14974, 0.11413, 0.09012, 0.07311,
#              0.06061, 0.05113, 0.04375, 0.0379, 0.03318)
#   p <- round(purrr::map_dbl(1:12, ~ Dunnetts_KF1(0, .x, 1/3)), 5)
#   expect_identical(p, gupta)
# })

# test_that("Dunnett's K2", {
#   test_KF2 <- function(x, k, rho){
#     V <- matrix(rho, nrow=k, ncol=k)
#     diag(V) <- 1
#     mnormt::sadmvn(lower=rep(-x,k), upper=rep(x,k), mean=rep(0,k),
#                    varcov=V)
#   }
#   expect_equal(Dunnetts_KF2(0, 2, 1/3), test_KF2(0, 2, 1/3))
# })

# test_that("Dunnett's K factor", {
#   check_EnvStats()
#   expect_equal(Dunnetts_K(n=10, k=3),
#                EnvStats:::Dunnetts.K(n=10, k=3, m=1))
#   #
#   expect_equal(predictionFactor(10, method="exact"),
#                EnvStats::predIntNormK(10, method="exact"))
#   #
#   x <- rnorm(10)
#   interval_EnvStats <- EnvStats::predIntNorm(x, k=2, method="exact")
#   expect_equal(predictionInterval(x, k=2, method="exact"),
#             interval_EnvStats$interval$limits,
#             check.attributes = FALSE)
# })
#
# test_that("Prediction interval - comparison statisticalIntervals", {
#   check_statisticalIntervals()
#   x <- rnorm(10)
#   I1 <- statisticalIntervals::calculate_interval(x, type="prediction", futureObs=3, conf.level=0.95)
#   I2 <- predictionInterval(x, k=3, method="Bonferroni")
#   expect_equal(I1, I2)
#   #
#   I3 <- statisticalIntervals::calculate_interval(x, type="meanprediction", futureObs=3, conf.level=0.95)
#   I4 <- predictionInterval(x, n.mean=3)
#   expect_equal(I3,I4)
# })
