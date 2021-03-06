library(bartCause)
set.seed(64)

N <- 500

X <- rnorm(N, 35, 10)
X <- X[X > 18 & X < 55]

dat <- data.frame(age = X, scaled_age = scale(X))

#beta.z <-c(-1)
asn_z <- function(x){
  if(x <= 30){
    rbinom(1, 1, .8)
  }
  
  else{rbinom(1, 1, .4)}
}

dat$z <- sapply(X, asn_z)



dat$y1 <- with(dat, 
               180 -7 +.5*scaled_age + I((scaled_age-.1)^2)*2  + rnorm(nrow(dat))
)

dat$true.1 <- with(dat, 
                   180 -7 +.5*scaled_age + I((scaled_age-.1)^2)*2 
)

dat$y0 <- with(dat, 
               176 +.5*scaled_age + I((scaled_age+.3)^2)*3.2 + rnorm(nrow(dat))
)

dat$true.0 <-  with(dat, 
                    176 +.5*scaled_age + I((scaled_age+.3)^2)*3.2
)

dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)



# linear regression fails
est <- lm(y~ z + age, dat)$coeff[[2]]
se <- sqrt(diag(vcov(lm(y~ z+ age, dat))))[[2]]
est + se*1.96;est - se*1.96
with(dat, mean(y1 - y0))
with(dat[dat$z ==1,], mean(y1 - y0))
with(dat[dat$z ==0,], mean(y1 - y0))


# bart suceeds 
bartc(dat$y, dat$z, dat$age)






