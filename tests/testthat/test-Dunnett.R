context("Dunnett's distribution")

test_that("Dunnett's distribution", {
  pr <- 0.95; df <- 24; rho <- 0.5; k <- 3
  # should be 2.51 according to tables
  q <- qDunnett(p=pr, df=df, k=k, rho=rho)
  expect_equal(round(as.numeric(q), 2), 2.51, check.attributes=FALSE)
  #
  p <- pDunnett(q, df=df, k=k, rho=rho)
  expect_equal(as.numeric(p), pr)
  #
  set.seed(666)
  sims <- rDunnett(100000, df=df, k=k, rho=rho, tailed="two")
  expect_equal(mean(sims < q), pr, tolerance=1e-3)
  #
  expect_true(qDunnett(p=0, df=df, k=k, rho=rho) == 0)
  expect_true(pDunnett(q=0, df=df, k=k, rho=rho) == 0)
  expect_true(qDunnett(p=0, df=df, k=k, rho=rho, tailed="two") == -Inf)
  expect_true(pDunnett(q=Inf, df=df, k=k, rho=rho) == 1)
  expect_true(pDunnett(q=-Inf, df=df, k=k, rho=rho) == 0)
  expect_true(pDunnett(q=Inf, df=df, k=k, rho=rho, tailed="two") == 1)
  expect_true(pDunnett(q=-Inf, df=df, k=k, rho=rho, tailed="two") == 0)
})
