# Set up Parameters
niris <- nrow(iris)
breps <- 29
seedval <- 123

ncars <- nrow(mtcars)

# Set up Models ----
fit0 <- lm(Sepal.Length ~ 1, data = iris)
fit1 <- lm(Sepal.Length ~ Species, data = iris)
fit2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

fit3 <- glm(am ~ 1, family = binomial, data = mtcars)
fit4 <- glm(am ~ hp, family = binomial, data = mtcars)