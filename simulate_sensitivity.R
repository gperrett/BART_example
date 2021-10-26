library(bartCause)
set.seed(64)

## low confounding
N <- 500

X = matrix(rnorm(2*N),N,2) 

betaz <- c(-.1, .1)

pscore <- pnorm(X %*% betaz)
#hist(pscore)

z <- rbinom(N, 1, pscore)
sum(z)

draw_u <- function(treat){
  if(treat ==1){
    rbinom(1, 1, .8)
  }
  
  else{
    rbinom(1,1,.2)
  }
}


U <- sapply(z, draw_u)
table(U, z)

tau <- 0

betay <- c(1, -2)

y1 <- 180 +  X%*%betay + tau - 4*U + rnorm(N, 0, 2)
y0 <- 180 +  X%*%betay - 4*U + rnorm(N, 0, 2)
y <- y0*(1-z) + y1*z

summary(bartc(y, z, X))
summary(bartc(y, z, X + U))


out.bin <- treatSens(y~ z + X, trt.family = binomial(link='probit'), standardize = F)
sensPlot(out.bin)


## high confounding
set.seed(64)
N = 500        #number of observations

zetay = -5           #coefficient on U in the outcome model
zetaz = .15            #coefficient on U in the treatment model
betaz = c(.5,-.25) #coefficients of X in the treatment model
betay = c(.5,-1)   #coefficients of X in the outcome model
tau = 0             #treatment effect

X = matrix(rnorm(2*N),N,2)  
U = rbinom(N,1,.5)                   #unmeasured confounder
ps = pnorm(X%*%betaz + zetaz*U) #propensity score
#hist(ps)
Z = rbinom(N,1,ps)                   #treatment variable
sum(Z)
epsilon = rnorm(N,0,2)               #error term
Y0 = X%*%betay + zetay*U + epsilon       #potential outcome(Z=0)
Y1 = X%*%betay + zetay*U + tau + epsilon #potential outcome(Z=1)
Y = Y0*(1-Z) + Y1*Z                  #realization of potential outcome

# sensitivity analysis
out.bin <- treatSens(Y~Z+X, trt.family = binomial(link="probit"), nsim = 20, 
                     #spy.range = c(0,4), spz.range = c(-2,2),grid.dim = c(5,3),
                     standardize = FALSE, verbose = TRUE)

sensPlot(out.bin) # draw contour plot

summary(bartc(Y, Z, X + U))
summary(bartc(Y, Z, X))

