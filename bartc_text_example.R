source("simulate_univariate.R")

bartc_fit <- bartCause::bartc(y, z, age,
                              data = dat, estimand = "att",
                              seed = 0)

summary(bartc_fit)

num_groups <- 5
dat <- within(dat, {
  country <- sample(num_groups, length(y), replace = TRUE)
  random_intercept <- rnorm(num_groups, 0, 0.75^2)[country]
  country <- as.factor(paste0("country_", country))
  
  y_mlm <- y + random_intercept
})

mlm_fit <- bartCause::bartc(y_mlm, z, age,
                            data = dat, estimand = "att",
                            group.by = country, group.effects = TRUE,
                            seed = 0)
summary(mlm_fit)


