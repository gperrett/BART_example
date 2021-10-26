

library(bartCause)
set.seed(64)

N <- 500

# X <- rnorm(N, 35, 10)
# X <- X[X > 18 & X < 55]
X <- runif(N, 18, 55)
dat <- data.frame(age = X, scaled_age = scale(X))

#beta.z <-c(-1)
asn_z <- function(x){
  if(x <= 25){
    rbinom(1, 1, 1)
  }
  
  else if (x >47){
    rbinom(1,1,0)
  }
  
  else{rbinom(1, 1, .4)}
}

dat$z <- sapply(X, asn_z)


# 
# beta.z <-c(-.75)
# p.score <- pnorm(as.matrix(scale(X)) %*% beta.z)
# hist(p.score)
# summary(p.score)


#dat$z <- rbinom(nrow(dat),1, p.score)
#dat$z <- rbinom(nrow(dat),1, p.score)

dat$y1 <- with(dat, 
               180 -7 +.5*scaled_age + I((scaled_age)^2)*1.5  + rnorm(nrow(dat))
)

dat$true.1 <- with(dat, 
                   180 -7 +.5*scaled_age + I((scaled_age)^2)*1.5
)

dat$y0 <- with(dat, 
               176 +.5*scaled_age + I((scaled_age+.2)^2)*2.5 + rnorm(nrow(dat))
)

dat$true.0 <-  with(dat, 
                    176 +.5*scaled_age + I((scaled_age+.2)^2)*2.5
)

dat$tau <- dat$true.1 - dat$true.0

dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)



# linear regression fails
est <- lm(y~ z + age, dat)$coeff[[2]]
se <- sqrt(diag(vcov(lm(y~ z+ age, dat))))[[2]]
est + se*1.96;est - se*1.96
with(dat, mean(y1 - y0))







