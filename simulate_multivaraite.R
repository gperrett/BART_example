library(bartCause)
set.seed(64)

N <- 1000

X <- rnorm(N, 35, 10)

miles <- floor(runif(N, 10, 80))
miles <- miles[X > 18 & X < 55]
X <- X[X > 18 & X < 55]


dat <- data.frame(age = X, scaled_age = scale(X), miles, sclaed_miles = scale(miles))
hist(scale(miles))

beta.z <- c(.1, .7)
p.score <- pnorm(cbind(dat$sclaed_miles, dat$scaled_age) %*% beta.z)

dat$miles[dat$miles>60] <- 'high'
dat$miles[dat$miles <= 60 & dat$miles >= 30] <- 'moderate'
dat$miles[dat$miles < 30] <- 'low'

dat$y1[dat$miles == 'high'] <- with(dat, 180 -12 +.5*scaled_age + I((scaled_age-.1)^2)*2)
dat$y1[dat$miles == 'moderate'] <- with(dat, 180 -10 +.5*scaled_age + I((scaled_age-.1)^2)*2.5)
dat$y1[dat$miles == 'low'] <- with(dat,180 -5 +.5*scaled_age + I((scaled_age-.1)^2)*3 )

dat$y1 <- dat$y1 + rnorm(nrow(dat))
dat$y0 <- with(dat, 176 +.5*scaled_age + I((scaled_age+.3)^2)*3.2 + rnorm(nrow(dat)))

dat$z <- rbinom(nrow(dat), 1, p.score)
dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)