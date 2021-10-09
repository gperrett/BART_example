source('simulate_univariate.R')
library(rpart)
library(rpart.plot)
library(gbm)
library(bartCause)


# Figure 2 ----------------------------------------------------------------

## fit regresson lines
intercept <-  lm(y~age + z , dat)$coeff[[1]]
intercept.z <-  lm(y~age + z , dat)$coeff[[1]] + lm(y~age+ z , dat)$coeff[[3]]
slope <- lm(y~age + z , dat)$coeff[[2]]

## get estimates
ate <- with(dat, mean(y1 - y0)) 
att <- mean(dat[dat$z ==1,]$y1 - dat[dat$z ==1,]$y0)
atc <- mean(dat[dat$z ==0,]$y1 - dat[dat$z ==0,]$y0)
est <- lm(y~ z + age, dat)$coeff[[2]]
se <- sqrt(diag(vcov(lm(y~ z+ age, dat))))[[2]]
est + se*1.96;est - se*1.96
ate

## fit regresstion tree
tree.df <- dat[, c('y', 'z', 'age')]
names(tree.df) <- c('y', 'z', 'age')
tree <- rpart::rpart(y~ ., tree.df)
dat$tree.pred <- predict(tree)
tree.1 <- dat[dat$z ==1,]
tree.0 <- dat[dat$z ==0,]

## produce plot 
with(dat, plot(age, y, cex = .5, pch = 19, xlab = 'age', ylab = 'running time in minutes', main = 'Non-Linear Marathon Finishing Times', col = ifelse(z ==1, 'red', 'blue')))
abline(a = intercept, b = slope, col = 'blue', lwd = 2, lty = 3)
abline(a = intercept.z, b = slope, col = 'red', lwd = 2, lty = 3)
lines(dat$age[order(dat$age)], dat$true.1[order(dat$age)], lwd = 2, col = 'red')
lines(dat$age[order(dat$age)], dat$true.0[order(dat$age)], lwd = 2, col = 'blue')
lines(tree.1$age[order(tree.1$age)],tree.1$tree.pred[order(tree.1$age)], lwd = 2, col = 'red')
lines(tree.0$age[order(tree.0$age)],tree.0$tree.pred[order(tree.0$age)], lwd = 2, col = 'blue')


# Figure 3 ----------------------------------------------------------------
tree
## tree scatter fit
par(mfrow=c(1, 2))
plot(dat$age, dat$y, cex = .5, pch = 19, xlab = 'age', ylab = 'running time in minutes', main = 'Regression Tree Fit')
segments(min(dat$age), 177.3743, 25.22462, 177.3743, lwd = 3, col = 'red', lty = 1)
segments(25.22463, 173.9866, 45.81388, 173.9866, lwd = 3, col = 'red')
segments(45.81389, 178.5476, max(dat$age), 178.5476, lwd = 3, col = 'red')
segments(min(dat$age), 181.6307 , 23.67956, 181.6307 , lwd = 3, lty = 1, col = 'blue')
segments(23.67957, 176.7390, 41.65464, 176.7390, lwd = 3, col = 'blue')
segments(41.65465, 181.8829 , 45.81388, 181.8829 , lwd = 3, col = 'blue')
segments(45.81389, 187.5907, 51.05924, 187.5907, lwd = 3, col = 'blue')
segments(51.05924, 194.6522, max(dat$age), 194.6522, lwd = 3, col = 'blue')

## tree plot 
rpart.plot(tree, type = 2, extra = 0, box.palette=0, branch = 1)
par(mfrow=c(1, 1))


# Figure 4 ----------------------------------------------------------------

## fit gbm
gbm <- gbm(y~age + z, data = dat, interaction.depth = 3,n.trees = 100, cv.folds = 5, train.fraction = 1)
best.iter <- gbm.perf(gbm, method = "cv")
Yhat <- predict(gbm, newdata = dat, n.trees = 100, type = "link")
dat$gbm.preds <- Yhat
dat <- dat[order(dat$age), ]

## make gbm plot 
with(dat,plot(age, y, cex = .5, pch = 19, xlab = 'age', ylab = 'running time in minutes', main = 'Boosted Regression Trees',col = ifelse(z ==1, 'red', 'blue')))
lines(dat$age, dat$true.1, lwd = 2, col = 'red')
lines(dat$age, dat$true.0, lwd = 2, col = 'blue')
lines(dat$age[dat$z ==0], dat$gbm.preds[dat$z==0], lwd = 3, col = 'blue', lty = 2)
lines(dat$age[dat$z== 1], dat$gbm.preds[dat$z== 1], lwd = 3, col = 'red', lty = 2)


# Figure 5 ----------------------------------------------------------------



# Figure 6 ----------------------------------------------------------------

## fit bart 
bart <- bartc(dat$y, dat$z, dat$age)
dat$obs.m <- apply(bartCause::extract(bart, 'mu.obs'), 2, mean)
dat$obs.sd <- apply(bartCause::extract(bart, 'mu.obs'), 2,sd)
dat$obs.lcl <- dat$obs.m - 1.96*dat$obs.sd 
dat$obs.ucl <- dat$obs.m + 1.96*dat$obs.sd 

dat$cf.m <- apply(bartCause::extract(bart, 'mu.cf'), 2, mean)
dat$cf.sd <- apply(bartCause::extract(bart, 'mu.cf'), 2,sd)
dat$cf.lcl <- dat$cf.m - 1.96*dat$cf.sd 
dat$cf.ucl <- dat$cf.m + 1.96*dat$cf.sd 



dat <- dat[order(dat$age),]


with(dat,plot(age, y, cex = .5, pch = 19, xlab = 'age', ylab = 'running time in minutes', main = 'BART', col = ifelse(z ==1, 'red', 'blue')))
lines(dat$age[order(dat$age)], dat$true.1[order(dat$age)], lwd = 2, col = 'red')
lines(dat$age[order(dat$age)], dat$true.0[order(dat$age)], lwd = 2, col = 'blue')
lines(dat$age[dat$z == 1], dat$obs.m[dat$z==1], lwd = 3, col = 'red', lty = 2)
lines(dat$age[dat$z == 0], dat$obs.m[dat$z==0], lwd = 3, col = 'blue', lty = 2)
segments(dat$age[dat$z ==1], y0 = dat$obs.lcl[dat$z ==1], y1 = dat$obs.ucl[dat$z ==1], col = 'red', lwd = 2)
segments(dat$age[dat$z ==0], y0 = dat$obs.lcl[dat$z ==0], y1 = dat$obs.ucl[dat$z ==0], col = 'blue', lwd = 2)
segments(dat$age[dat$z ==1], y0 = dat$cf.lcl[dat$z ==1], y1 = dat$cf.ucl[dat$z ==1], col = 'blue', lwd = 2)
segments(dat$age[dat$z ==0], y0 = dat$cf.lcl[dat$z ==0], y1 = dat$cf.ucl[dat$z ==0], col = 'red', lwd = 2)


# Figure 7 ----------------------------------------------------------------
rm(list = ls())
source('simulate_overlap.R')
# #fit bart model with overlap adjustment
bart.overlap <- bartc(dat$y, dat$z, dat$age, commonSup.rule = 'sd')

## extract icates
icate.m <- apply(bartCause::extract(bart.overlap, 'icate'), 2, mean)
icate.sd <- apply(bartCause::extract(bart.overlap, 'icate'), 2,sd)
icate.lcl <- icate.m - 1.96*icate.sd
icate.ucl <- icate.m + 1.96*icate.sd

## fit plots 
par(mfrow = c(1, 2))

## icate and true tau 
with(dat, plot(rep(age, 2), c(icate.ucl, icate.lcl), cex = .5, pch = 19, type = 'n',xlab = 'age', ylab = 'treatment effect', main = 'BART Predicted Treatment Effect'))
segments(dat$age, y0 = icate.lcl, y1 = icate.ucl, col = ifelse(dat$z ==1, 'red', 'blue'))
lines(dat$age[order(dat$age)], dat$tau[order(dat$age)], lwd = 2)

## points removed by bart and true response surface
with(dat, plot(age, y, cex = .5, pch = 19, xlab = 'age', ylab = 'running time in minutes', main = 'Common Suppot Scatterplot', col = ifelse(z ==1, 'red', 'blue')))
lines(dat$age[order(dat$age)], dat$true.1[order(dat$age)], lwd = 2, col = 'red')
lines(dat$age[order(dat$age)], dat$true.0[order(dat$age)], lwd = 2, col = 'blue')
points(dat$age[bart.overlap$commonSup.sub == F],dat$y[bart.overlap$commonSup.sub == F])
legend('topleft', legend=c('Treatment', 'Control', 'Removed'), col = c('red', 'blue', 'black'), pch = c(19, 19, 21))


# Figure 8  ---------------------------------------------------------------


