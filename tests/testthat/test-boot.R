test_that("CI - Residual Bootstrap", {
  set.seed(seedval)
  b1 <- matrix(sample(fit2$residuals, size = niris*breps, replace = TRUE),
               nrow = niris, ncol = breps)
  y1 <- fit2$fitted.values + b1

  bhat1 <- matrix(NA, nrow = length(coef(fit2)), ncol = breps)
  for(i in 1:breps){
    bhat1[,i] <- coef(lm(y1[,i] ~ iris$Sepal.Width + iris$Species))
  }

  baseci <- c(t(apply(bhat1, 1, quantile, probs = c(0.025, 0.975))))


  set.seed(seedval)
  ci <- c(as.matrix(
    estimate_parameters(fit2,
                        assume.constant.variance = TRUE,
                        assume.normality = FALSE,
                        bootstrap.reps = breps)[,c(4,5)]
  ))

  expect_equal(ci, baseci)
})


test_that("CI - Wild Bootstrap", {
  set.seed(seedval)
  b1 <- matrix(rmammen(niris*breps), nrow = niris, ncol = breps)
  y1 <- fit2$fitted.values + b1*fit2$residuals

  bhat1 <- matrix(NA, nrow = length(coef(fit2)), ncol = breps)
  for(i in 1:breps){
    bhat1[,i] <- coef(lm(y1[,i] ~ iris$Sepal.Width + iris$Species))
  }

  baseci <- c(t(apply(bhat1, 1, quantile, probs = c(0.025, 0.975))))


  set.seed(seedval)
  ci <- c(as.matrix(
    estimate_parameters(fit2,
                        assume.constant.variance = FALSE,
                        assume.normality = FALSE,
                        bootstrap.reps = breps)[,c(4,5)]
  ))

  expect_equal(ci, baseci)
})
