test_that("CI - Normal, Constant Variance", {
  basevs0 <- c(confint(fit0))
  basevs2 <- c(confint(fit2))

  my0 <- c(as.matrix(
    estimate_parameters(fit0,
                        confidence.level = 0.95,
                        assume.constant.variance = TRUE,
                        assume.normality = TRUE,
                        simulation.replications = breps)[, c(4,5)]
  ))

  my2 <- c(as.matrix(
    estimate_parameters(fit2,
                        confidence.level = 0.95,
                        assume.constant.variance = TRUE,
                        assume.normality = TRUE,
                        simulation.replications = breps)[,c(4,5)]
  ))

  expect_equal(my0, basevs0)

  expect_equal(my2, basevs2)
})


test_that("P - Normal, Constant Variance", {
  basevs1 <- anova(fit0, fit2)[2,6]
  basevs2 <- anova(fit1, fit2)[2,6]
  basevs3 <- summary(fit2)$coefficients[2,4]

  expect_equal(compare_models(fit2,
                              fit0,
                              assume.constant.variance = TRUE,
                              assume.normality = TRUE,
                              simulation.replications = breps)$p.value[1],
               basevs1)

  expect_equal(compare_models(fit2,
                              fit1,
                              assume.constant.variance = TRUE,
                              assume.normality = TRUE,
                              simulation.replications = breps)$p.value[1],
               basevs2)

  expect_equal(compare_models(fit2,
                              fit1,
                              assume.constant.variance = TRUE,
                              assume.normality = TRUE,
                              simulation.replications = breps)$p.value[1],
               basevs3)
})


test_that("CI - Logistic Regression", {
  basevs3 <- c(confint(fit3), use.names = FALSE)
  basevs4 <- c(confint(fit4), use.names = FALSE)

  my0 <- c(as.matrix(
    estimate_parameters(fit3,
                        confidence.level = 0.95,
                        method = "classical",
                        simulation.replications = breps)[, c(4,5)]
  ))

  my2 <- c(as.matrix(
    estimate_parameters(fit4,
                        confidence.level = 0.95,
                        method = "classical",
                        simulation.replications = breps)[,c(4,5)]
  ))

  expect_equal(my0, basevs3, tolerance = 0.0001, scale = 1)

  expect_equal(my2, basevs4, tolerance = 0.0001, scale = 1)
})


test_that("P - Logistic Regression", {
  basevs1 <- anova(fit3, fit4, test = "Chisq")[2, 5]

  expect_equal(compare_models(fit4,
                              fit3,
                              method = "classical",
                              simulation.replications = breps)$p.value[1],
               basevs1)
})

