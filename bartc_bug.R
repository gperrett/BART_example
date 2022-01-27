set.seed(7642)

n <- 1000L
beta.z <- c(.75, -0.5,  0.25)
beta.y <- c(.5,   1.0, -1.5)
sigma <- 2

x <- matrix(rnorm(3 * n), n, 3)
tau <- 4

p.score <- pnorm(x %*% beta.z)
z <- rbinom(n, 1, p.score)

mu.0 <- x %*% beta.y
mu.1 <- x %*% beta.y + tau
mean(mu.1 - mu.0)
y <- mu.0 * (1 - z) + mu.1 * z + rnorm(n, 0, sigma)

dat <- data.frame(y, z, x1 = x[, 1], x2 = x[, 2], x3 = x[, 3])
rm(y, z)

bartc(y, z, ., data = dat) # this should be 4

y <- dat$y
z <- dat$z

bartc(y, z, x) # this is correct with x as a matrix
