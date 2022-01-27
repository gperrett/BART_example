library(bartCause)
set.seed(64)

N <- 500

X <- rnorm(N, 35, 10)

miles <- floor(runif(N, 10, 80))
miles <- miles[X > 18 & X < 55]
X <- X[X > 18 & X < 55]


dat <- data.frame(age = X, scaled_age = scale(X), miles, scaled_miles = scale(miles))

beta.z <- c(.1, .6)
p.score <- pnorm(cbind(dat$scaled_miles, dat$scaled_age) %*% beta.z)

dat$miles[dat$miles>60] <- 'high'
dat$miles[dat$miles <= 60 & dat$miles >= 30] <- 'moderate'
dat$miles[dat$miles < 30] <- 'low'

dat$y1[dat$miles == 'high'] <- with(dat[dat$miles == 'high',], 180 -15 +.5*scaled_age + I((scaled_age-.1)^2)*2.5)
dat$y1[dat$miles == 'moderate'] <- with(dat[dat$miles == 'moderate',], 180 -10 +.5*scaled_age + I((scaled_age-.1)^2)*2.5)
dat$y1[dat$miles == 'low'] <- with(dat[dat$miles == 'low',],180 -6 +.5*scaled_age + I((scaled_age-.1)^2)*2.7 )

dat$y1 <- dat$y1 + rnorm(nrow(dat))
dat$y0 <- with(dat, 176 +.5*scaled_age + I((scaled_age+.3)^2)*3.2 + rnorm(nrow(dat)))

dat$z <- rbinom(nrow(dat), 1, p.score)
dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)


# dat %>% count(miles)
# covs <- dat %>% dplyr::select(age, miles)
# model <- bartc(dat$y, dat$z, covs)
# icates <- bartCause::extract(model, 'icate')
# icate.sd <- apply(icates, 2, sd)
# icate.m <- apply(icates, 2, mean)
# icate.ub <- icate.m + 1.96*icate.sd
# icate.lb <- icate.m - 1.96*icate.sd
# dat <- dat %>% mutate(color = case_when(
#   miles == 'high' ~ 'green', 
#   miles == 'moderate' ~ 'orange',
#   miles == 'low' ~ 'pink'
# ))


# by age and mile
# par(mfrow = c(1,2))
# 
# hist(icate.m[dat$miles == 'high'], col = 'green', cex = .5, pch = 19, xlab = 'individual CATE', main = 'Heterogeneous Treatment Effects by Milage', xlim = c(-25,1), ylim = c(0, 60))
# hist(icate.m[dat$miles == 'moderate'], col = 'orange', cex = .5, pch = 19, xlab = 'individual CATE', xlim = c(-25, 1), add = T, ylim = c(0, 60))
# hist(icate.m[dat$miles == 'low'], col = 'pink', cex = .5, pch = 19, xlab = 'individual CATE',  xlim = c(-25, 1), add = T, ylim = c(0, 60), alpha = .6)
# legend('topleft', legend=c('Low Mileage', 'Moderate Mileage', 'High Mileage'), col = c('pink', 'orange', 'green'), pch = c(19, 19, 19))
# 
# plot(density(icate.m[dat$miles == 'high'], adjust = 2), 
#      col = 'green', cex = .5, pch = 19, xlab = 'individual CATE', 
#      main = 'Heterogeneous Treatment Effects by Mileage', 
#      xlim = c(-25,1), ylim = c(0, .15), 
#      lwd = 3)
# lines(density(icate.m[dat$miles == 'moderate'], adjust = 2.5), col = 'orange', cex = .5, pch = 19, xlab = 'individual CATE', xlim = c(-25, 1), add = T, lwd = 3)
# lines(density(icate.m[dat$miles == 'low'], adjust = 2), col = 'pink', cex = .5, pch = 19, xlab = 'individual CATE',  xlim = c(-25, 1), add = T, ylim = c(0, 60), lwd = 3)
# legend('topleft', legend=c('High Mileage', 'Moderate Mileage','Low Mileage'), col = c('green', 'orange', 'pink'), pch = c(19, 19, 19))
# 
# 
# plot(dat$age, icate.m, cex = .5, pch = 19, xlab = 'age', col = dat$color, ylab = 'individual CATE', main = 'Heterogeneous Treatment Effects by Age', ylim = c(min(icate.lb), max(icate.ub)))
# segments(dat$age, y0 = icate.lb, y1 = icate.ub, col = dat$color)
# legend('bottomleft', legend=c('High Mileage', 'Moderate Mileage','Low Mileage'), col = c('green', 'orange', 'pink'), pch = c(19, 19, 19))

# 
# # with ordered effect
# icate.o <- order(icate.m)
# plot(1:length(icate.o), icate.m[icate.o], cex = .5, pch = 19, xlab = 'order of individual effect size', col = dat$color, ylab = 'individual CATE', main = 'Heterogeneous Treatment Effects', ylim = c(min(icate.lb), max(icate.ub)))
# segments(1:length(icate.o), y0 = icate.lb[icate.o], y1 = icate.ub[icate.o], col = dat$color)
# legend('topleft', legend=c('High Mileage', 'Moderate Mileage','Low Mileage'), col = c('green', 'orange', 'pink'), pch = c(19, 19, 19))
# 
# plot(density(icate.m[dat$miles == 'high'], adjust = 2), 
#      col = 'green', cex = .5, pch = 19, xlab = 'individual CATE', 
#      main = 'Heterogeneous Treatment Effects by Milage', 
#      xlim = c(-25,1), ylim = c(0, .2), 
#      lwd = 3)
# lines(density(icate.m[dat$miles == 'moderate'], adjust = 2.5), col = 'orange', cex = .5, pch = 19, xlab = 'individual CATE', xlim = c(-25, 1), add = T, lwd = 3)
# lines(density(icate.m[dat$miles == 'low'], adjust = 2), col = 'pink', cex = .5, pch = 19, xlab = 'individual CATE',  xlim = c(-25, 1), add = T, ylim = c(0, 60), lwd = 3)
# legend('topleft', legend=c('High Mileage', 'Moderate Mileage','Low Mileage'), col = c('green', 'orange', 'pink'), pch = c(19, 19, 19))


# 
# 
# 
# 
# 
# 
# hist(icate.m, col = dat$color, cex = .5, pch = 19, xlab = 'age', ylab = 'individual CATE', main = 'Heterogeneous Treatment Effects', xlim = c(-25,1))
# 
# dat %>% 
#   ggplot(aes(icate.m)) + 
#   geom_histogram(fill = miles) + 
#   facet_wrap(~miles, ncol = 1)
