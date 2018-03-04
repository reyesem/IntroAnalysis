test_that("CI - Normal, Constant Variance", {
  basevs0 <- c(confint(fit0))
  basevs2 <- c(confint(fit2))

  my0 <- c(as.matrix(
    estimate_parameters(fit0,
                        conf.level = 0.95,
                        assume.constant.variance = TRUE,
                        assume.normality = TRUE,
                        bootstrap.reps = breps)[, c(4,5)]
  ))

  my2 <- c(as.matrix(
    estimate_parameters(fit2,
                        conf.level = 0.95,
                        assume.constant.variance = TRUE,
                        assume.normality = TRUE,
                        bootstrap.reps = breps)[,c(4,5)]
  ))

  expect_equal(my0, basevs0)

  expect_equal(my2, basevs2)
})


test_that("CI - Normal, Nonconstant Variance", {
  carvs2 <- c(coef(fit2) + sqrt(diag(car::hccm(fit2, "hc0"))) %o%
    qt(c(0.025, 0.975), df = fit2$df.residual))


  my <- c(as.matrix(
    estimate_parameters(fit2,
                        conf.level = 0.95,
                        assume.constant.variance = FALSE,
                        assume.normality = TRUE,
                        bootstrap.reps = breps)[,c(4,5)]
  ))

  expect_equal(my, carvs2)
})


test_that("P - Normal, Constant Variance", {
  basevs1 <- anova(fit0, fit2)[2,6]
  basevs2 <- anova(fit1, fit2)[2,6]
  basevs3 <- summary(fit2)$coefficients[2,4]

  expect_equal(compare_models(fit2,
                              fit0,
                              assume.constant.variance = TRUE,
                              assume.normality = TRUE,
                              bootstrap.reps = breps)$p.value[1],
               basevs1)

  expect_equal(compare_models(fit2,
                              fit1,
                              assume.constant.variance = TRUE,
                              assume.normality = TRUE,
                              bootstrap.reps = breps)$p.value[1],
               basevs2)

  expect_equal(compare_models(fit2,
                              fit1,
                              assume.constant.variance = TRUE,
                              assume.normality = TRUE,
                              bootstrap.reps = breps)$p.value[1],
               basevs3)
})


test_that("P - Normal, Nonconstant Variance", {
  basevs1 <- car::Anova(fit1, white.adjust = "hc0")[1,3]

  Kmat <- matrix(c(0, 1, 0, 0), nrow = 1, ncol = 4)
  basevs2 <- car::linearHypothesis(fit2,
                                   Kmat,
                                   test = "F",
                                   white.adjust = "hc0")[2,4]

  expect_equal(compare_models(fit1,
                              fit0,
                              assume.constant.variance = FALSE,
                              assume.normality = TRUE,
                              bootstrap.reps = breps)$p.value[1],
               basevs1)

  expect_equal(compare_models(fit2,
                              fit1,
                              assume.constant.variance = FALSE,
                              assume.normality = TRUE,
                              bootstrap.reps = breps)$p.value[1],
               basevs2)
})