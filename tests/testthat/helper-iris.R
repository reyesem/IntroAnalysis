# Set up Parameters
niris <- nrow(iris)
breps <- 29
seedval <- 123

# Set up Models ----
fit0 <- lm(Sepal.Length ~ 1, data = iris)
fit1 <- lm(Sepal.Length ~ Species, data = iris)
fit2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
