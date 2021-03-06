source('simulate_univariate.R')
library(rpart)
library(rpart.plot)
library(gbm)
library(bartCause)
library(treatSens)
library(ggplot2)
library(gridExtra)


# Plot parameters
# https://personal.sron.nl/~pault/#sec:qualitative
cb_palette <- c('#004488', '#DDAA33', '#BB5566')
default_pars <- list(mar = c(2.15, 2.15, 2.1, 0.05), mgp = c(1.1, 0.2, 0), tcl = -0.2) 
default_dims <- c(5.5 * 1.5, 2.5 * 1.5)

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
pdf("plots/hill2fig.pdf", default_dims[1], default_dims[2])
do.call("par", default_pars)
with(dat, {
  plot(age, y, cex = .5, pch = ifelse(z == 1, 19, 22),
       xlab = 'age', ylab = 'running time in minutes',
       main = 'Non-Linear Marathon Finishing Times',
       col = cb_palette[as.integer(z == 1) + 1],
       bty = 'l')
})
abline(a = intercept, b = slope, col = cb_palette[1], lwd = 2, lty = 2)
abline(a = intercept.z, b = slope, col = cb_palette[2], lwd = 2, lty = 2)
lines(dat$age[order(dat$age)], dat$true.1[order(dat$age)], lwd = 1.5, col = cb_palette[2])
lines(dat$age[order(dat$age)], dat$true.0[order(dat$age)], lwd = 1.5, col = cb_palette[1])
lines(tree.1$age[order(tree.1$age)],tree.1$tree.pred[order(tree.1$age)], lwd = 2, col = cb_palette[2])
lines(tree.0$age[order(tree.0$age)],tree.0$tree.pred[order(tree.0$age)], lwd = 2, col = cb_palette[1])
dev.off()

# Figure 3 ----------------------------------------------------------------
tree
## tree scatter fit
pdf("plots/hill3fig.pdf", default_dims[1], default_dims[2])
do.call("par", append(default_pars, list(mfrow = c(1, 2))))
with(dat, {
  plot(age, y, cex = .5, pch = ifelse(z == 1, 19, 22),
       xlab = 'age', ylab = 'running time in minutes',
       main = 'Regression Tree Fit',
       col = cb_palette[as.integer(z == 1) + 1],
       bty = 'l')
})
segments(min(dat$age), 177.3743, 25.22462, 177.3743, lwd = 3, col = cb_palette[2], lty = 1)
segments(25.22463, 173.9866, 45.81388, 173.9866, lwd = 3, col = cb_palette[2])
segments(45.81389, 178.5476, max(dat$age), 178.5476, lwd = 3, col = cb_palette[2])
segments(min(dat$age), 181.6307 , 23.67956, 181.6307 , lwd = 3, lty = 1, col = cb_palette[1])
segments(23.67957, 176.7390, 41.65464, 176.7390, lwd = 3, col = cb_palette[1])
segments(41.65465, 181.8829 , 45.81388, 181.8829 , lwd = 3, col = cb_palette[1])
segments(45.81389, 187.5907, 51.05924, 187.5907, lwd = 3, col = cb_palette[1])
segments(51.05924, 194.6522, max(dat$age), 194.6522, lwd = 3, col = cb_palette[1])

## tree plot 
rpart.plot(tree, type = 2, extra = 0, box.palette=0, branch = 1)
dev.off()


# Figure 4 ----------------------------------------------------------------
## fit gbm
gbm <- gbm(y~age + z, data = dat, interaction.depth = 3,n.trees = 100, cv.folds = 5, train.fraction = 1)
best.iter <- gbm.perf(gbm, method = "cv", plot.it = FALSE)
Yhat <- predict(gbm, newdata = dat, n.trees = 100, type = "link")
dat$gbm.preds <- Yhat
dat <- dat[order(dat$age), ]

## make gbm plot 
pdf("plots/hill4fig.pdf", default_dims[1], default_dims[2])
do.call("par", default_pars)
with(dat, {
  plot(age, y, cex = .5, pch = ifelse(z == 1, 19, 22),
       xlab = 'age', ylab = 'running time in minutes',
       main = 'Boosted Regression Trees',
       col = cb_palette[as.integer(z == 1) + 1],
       bty = 'l')
})
lines(dat$age, dat$true.1, lwd = 1.5, col = cb_palette[2])
lines(dat$age, dat$true.0, lwd = 1.5, col = cb_palette[1])
lines(dat$age[dat$z ==0], dat$gbm.preds[dat$z==0], lwd = 3, col = cb_palette[1], lty = 2)
lines(dat$age[dat$z== 1], dat$gbm.preds[dat$z== 1], lwd = 3, col = cb_palette[2], lty = 2)
dev.off()


# Figure 5 ----------------------------------------------------------------

## select subset of data
dat <- dat[order(dat$age),]
observed <- round(dat[c(1,40,100), c('age', 'z', 'y')], 2)
elipise <- rep('...', 3)
observed <- rbind.data.frame(observed, elipise)
observed <- rbind.data.frame(observed, round(dat[c(400, 420, 460),c('age', 'z', 'y')], 2))
observed <- rbind(observed, elipise)
names(observed) <- c('age', 'hyperShoe', 'running time')
ggplot() + 
annotation_custom(tableGrob(observed, rows = NULL, theme = ttheme_default(base_size = 8))) 

## fit bart 
bart <- bartc(dat$y, dat$z, dat$age)

## extract posteriors 
mu.1 <- bartCause::extract(bart, 'mu.1')
mu.0 <- bartCause::extract(bart, 'mu.0')
icate <- bartCause::extract(bart, 'icate')

t1 <- observed
names(t1)  <- c('age', 'hyperShoe', 'mu (1) hat')

t1[,2] <- c(1,1,1,'...', 1, 1, 1, '...')
preds <- round(apply(mu.1[,c(1,40,100)], 2, mean), 2)
preds <- c(preds, '...')
preds <- c(preds, round(apply(mu.1[,c(400, 420, 460)], 2, mean), 2), '...')
t1[,3] <- preds
t1 

names(t1)[3] <- "$\\mu$"
ggplot() + 
  annotation_custom(
    tableGrob(t1,
              rows = NULL, theme = ttheme_default(base_size = 8))) 


t0 <- observed
names(t0)  <- c('age', 'hyperShoe', 'mu (0) hat')

t0[,2] <- c(0,0,0,'...', 0, 0, 0, '...')
preds <- round(apply(mu.0[,c(1,40,100)], 2, mean), 2)
preds <- c(preds, '...')
preds <- c(preds, round(apply(mu.0[,c(400, 420, 460)], 2, mean), 2), '...')
t0[,3] <- preds

names(t0)[3] <- 'prediction'

ggplot() + 
  annotation_custom(tableGrob(t0, rows = NULL, theme = ttheme_default(base_size = 8))) 


## averaged over icates
# hist(mu.1[, 1])
# hist(mu.0[, 1])
# hist(icate[, 1])
# 
# 
# par(mfrow = c(3,1))
# hist(apply(mu.1, 2, mean), 
#      xlab = TeX(r'(predicted $\textbf{\textit{mu}}$ | z = 1)'), 
#      main = TeX(r'($\widehat{\textbf{\textit{mu}}}(1)$)'),
#      col = cb_palette[2], 
#      xlim = c(170, 195))
# hist(apply(mu.0, 2, mean), 
#      xlab = TeX(r'(predicted $\textbf{\textit{mu}}$ | z = 0)'),
#      main = TeX(r'($\widehat{\textbf{\textit{mu}}}(0)$)'),
#      col = cb_palette[1], 
#      xlim = c(170, 195))
# 
# hist(apply(icate, 2, mean), 
#      xlab = 'individual conditional treatment effects (ICATE)', 
#      main = TeX(r'($\widehat{\textbf{\textit{mu}}}(1) - \widehat{\textbf{\textit{mu}}}(0)$)'),
#      col = 'purple')


## individual lelvel icate
library(latex2exp)
do.call("par", append(default_pars, list(mfrow = c(3, 1), oma = c(0, 0, 0, 0))))
hist(mu.1[,40], 
     xlab = TeX(r'(predicted $\mu_2$ | z = 1)'), 
     main = TeX(r'($\widehat{\mu_2}(1)$)'),
     col = cb_palette[2], 
     breaks = seq(175,182,l=15),
     xlim = c(175, 182))

hist(mu.0[,40], 
     xlab = TeX(r'(predicted $\mu_2$ | z = 0)'), 
     main = TeX(r'($\widehat{\mu_2}(0)$)'),
     col = cb_palette[1], 
     xlim = c(175, 182))


hist(icate[,40], 
     xlab = 'individual conditional treatment effects (ICATE)', 
     main = TeX(r'($\widehat{\mu_2}(1) - \widehat{\mu_2}(0)$)'),
     col = cb_palette[3])

# 
# ## make tables
# observed <- round(dat[c(1), c('age', 'z', 'y')], 2)
# observed <- rbind.data.frame(observed, observed, observed)
# elipise <- rep('...', 3)
# observed <- rbind.data.frame(observed, elipise, observed)
# names(observed) <- c('age', 'hyperShoe', 'running time')
# ggplot() + 
#   annotation_custom(tableGrob(observed, rows = NULL, theme = ttheme_default(base_size = 8))) 
# 
# t1 <- observed
# names(t1)  <- c('age', 'hyperShoe', 'mu (1) hat')
# 
# t1[,2] <- c(1,1,1,'...', 1, 1, 1)
# preds <- round(mu.1[1:3, 1], 2)
# preds <- c(preds, '...')
# preds <- c(preds, round(mu.1[4:6, 1], 2))
# t1[,3] <- preds
# 
# 
# ggplot() + 
#   annotation_custom(tableGrob(t1, rows = NULL, theme = ttheme_default(base_size = 8))) 
# 
# 
# 
# t0 <- observed
# names(t0)  <- c('age', 'hyperShoe', 'mu (0) hat')
# 
# t0[,2] <- c(0,0,0,'...', 0, 0, 0)
# preds <- round(mu.0[1:3, 1], 2)
# preds <- c(preds, '...')
# preds <- c(preds, round(mu.0[4:6, 1], 2))
# t0[,3] <- preds
# 
# 
# ggplot() + 
#   annotation_custom(tableGrob(t0, rows = NULL, theme = ttheme_default(base_size = 8))) 

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

pdf("plots/hill6fig.pdf", default_dims[1], default_dims[2])
do.call("par", default_pars)
with(dat, {
  plot(age, y, cex = .5, pch = ifelse(z == 1, 19, 22),
       xlab = 'age', ylab = 'running time in minutes',
       main = 'BART', col = cb_palette[as.integer(z == 1) + 1],
       bty = 'l')
})
lines(dat$age[order(dat$age)], dat$true.1[order(dat$age)], lwd = 1.5, col = cb_palette[2])
lines(dat$age[order(dat$age)], dat$true.0[order(dat$age)], lwd = 1.5, col = cb_palette[1])
lines(dat$age[dat$z == 1], dat$obs.m[dat$z==1], lwd = 3, col = cb_palette[2], lty = 2)
lines(dat$age[dat$z == 0], dat$obs.m[dat$z==0], lwd = 3, col = cb_palette[1], lty = 2)
segments(dat$age[dat$z ==1], y0 = dat$obs.lcl[dat$z ==1], y1 = dat$obs.ucl[dat$z ==1], col = cb_palette[2], lwd = .75)
segments(dat$age[dat$z ==0], y0 = dat$obs.lcl[dat$z ==0], y1 = dat$obs.ucl[dat$z ==0], col = cb_palette[1], lwd = .75)
segments(dat$age[dat$z ==1], y0 = dat$cf.lcl[dat$z ==1], y1 = dat$cf.ucl[dat$z ==1], col = cb_palette[1], lwd = 0.75)
segments(dat$age[dat$z ==0], y0 = dat$cf.lcl[dat$z ==0], y1 = dat$cf.ucl[dat$z ==0], col = cb_palette[2], lwd = 0.75)
dev.off()


# Figure 7 ----------------------------------------------------------------
rm(list = setdiff(ls(), c("default_pars", "default_dims", "cb_palette")))

source('simulate_overlap.R')
source('simulate_univariate2.R')
# #fit bart model with overlap adjustment
bart.overlap <- bartc(dat$y, dat$z, dat$age, commonSup.rule = 'sd')

## extract icates
icate.m <- apply(bartCause::extract(bart.overlap, 'icate'), 2, mean)
icate.sd <- apply(bartCause::extract(bart.overlap, 'icate'), 2,sd)
icate.lcl <- icate.m - 1.96*icate.sd
icate.ucl <- icate.m + 1.96*icate.sd

## fit plots 
pdf("plots/hill7fig.pdf", default_dims[1], default_dims[2])
do.call("par", append(default_pars, list(mfrow = c(1, 2))))
## icate and true tau 
with(dat, {
  plot(age, icate.m, cex = .5,
       xlab = 'age', ylab = 'treatment effect',
       main = 'BART Predicted Treatment Effect',
       ylim = c(min(icate.lcl), max(icate.ucl)),
       type = 'n', bty = 'l')
  segments(age, y0 = icate.lcl, y1 = icate.ucl,
           col = ifelse(z ==1, cb_palette[2], cb_palette[1]))
  lines(age[order(age)], tau[order(age)], lwd = 2)
})

## points removed by bart and true response surface
with(dat, {
    plot(age, y, cex = .5, pch = ifelse(z == 1, 19, 22),
         xlab = 'age', ylab = 'running time in minutes',
         main = 'Common Suppot Scatterplot',
         col = ifelse(z ==1, cb_palette[2], cb_palette[1]),
         bty = 'l')
  lines(age[order(age)], true.1[order(age)], lwd = 1.5, col = cb_palette[2])
  lines(age[order(age)], true.0[order(age)], lwd = 1.5, col = cb_palette[1])
  points(age[bart.overlap$commonSup.sub == FALSE], y[bart.overlap$commonSup.sub == FALSE],
         pch = 1)
  legend('topleft', legend=c('Treatment', 'Control', 'Removed'),
         col = c(cb_palette[2], cb_palette[1], 'black'),

         pch = c(19, 22, 1))
})
dev.off()


# Figure 8  ---------------------------------------------------------------
rm(list = setdiff(ls(), c("default_pars", "default_dims", "cb_palette")))
source('simulate_multivariate.R')

# pull covs
covs <- dat %>% dplyr::select(age, miles)
# fit model 
model <- bartc(dat$y, dat$z, covs)
icates <- bartCause::extract(model, 'icate')
icate.sd <- apply(icates, 2, sd)
icate.m <- apply(icates, 2, mean)
icate.ub <- icate.m + 1.96*icate.sd
icate.lb <- icate.m - 1.96*icate.sd

library(dplyr, warn.conflicts = FALSE)
dat <- dat %>%
  mutate(color =
           case_when(miles == 'low' ~ cb_palette[2],
                     miles == 'moderate' ~ cb_palette[1],
                     miles == 'high' ~ cb_palette[3]))


pdf("plots/hill8fig.pdf", default_dims[1], default_dims[2])
do.call("par", default_pars)
par(mfrow = c(1,2))

plot(density(icate.m[dat$miles == 'high'], adjust = 2), 
     col = cb_palette[3], cex = .5, pch = 19, xlab = 'individual CATE', 
     main = 'Heterogeneous Treatment Effects by Mileage', 
     xlim = c(-25,1), ylim = c(0, .15), 
     lwd = 3)
lines(density(icate.m[dat$miles == 'moderate'], adjust = 2), col = cb_palette[1], cex = .5, pch = 19, xlab = 'individual CATE', xlim = c(-25, 1), lwd = 3)
lines(density(icate.m[dat$miles == 'low'], adjust = 2), col = cb_palette[2], cex = .5, pch = 19, xlab = 'individual CATE',  xlim = c(-25, 1), ylim = c(0, 60), lwd = 3)
legend('topleft', legend=c('High Mileage', 'Moderate Mileage','Low Mileage'), col = c(cb_palette[3], cb_palette[1], cb_palette[2]), pch = c(19, 19, 19))


plot(dat$age, icate.m, cex = .5, pch = 19, xlab = 'age', col = dat$color, ylab = 'individual CATE', main = 'Heterogeneous Treatment Effects by Age', ylim = c(min(icate.lb), max(icate.ub)))
segments(dat$age, y0 = icate.lb, y1 = icate.ub, col = dat$color)
legend('bottomleft', legend=c('High Mileage', 'Moderate Mileage','Low Mileage'), col = c(cb_palette[3], cb_palette[1], cb_palette[2]), pch = c(19, 19, 19))
dev.off()


# Figure 9  ---------------------------------------------------------------
rm(list = setdiff(ls(), c("default_pars", "default_dims", "cb_palette")))
source('simulate_sensitivity.R')
library(treatSens)
out.bin <- treatSens(Y ~ Z + age, trt.family = binomial(link='probit'),data = dat, standardize = FALSE)
sensPlot(out.bin)

out.bin <- treatSens(Y ~ z + age, trt.family = binomial, data = dat, standardize = T)
sensPlot(out.bin)
