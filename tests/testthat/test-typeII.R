context("stats")

# to do: test missing values

test_that("Type II ANOVA", {
  dat <- npk
  dat$w <- 1
  dat$w[1:12] <- 2
  fit1 <- lm(yield ~ block + N + block:N, data=dat)
  fit2 <- lm(yield ~ block + N + block:N, weights=dat$w, data=dat)
  a1 <- typeIItable(fit1)
  a2 <- typeIItable(fit2)
  expect_false(identical(a1, a2))
  a2car <- car::Anova(fit2)
  expect_equal(a2$`Pr(>F)`, a2car$`Pr(>F)`)
})

test_that("Type II ANOVA - perfect fit", {
  dat <- npk
  dat <- within(dat, {
    y <- as.integer(block) + as.integer(N) + as.integer(block:N)
  })
  fit <- lm(y ~ block + N + block:N, data=dat)
  expect_warning(typeIItable(fit))
})
