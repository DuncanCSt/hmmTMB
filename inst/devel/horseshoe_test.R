# Horseshoe prior demonstration script
library(devtools)
devtools::install("../../../hmmTMB")

set.seed(123)

# Simulate covariate data -------------------------------------------------
n <- 250
x1 <- scale(rnorm(n))[, 1]          # informative
x2 <- scale(runif(n, -1, 1))[, 1]   # informative
x3 <- scale(rnorm(n))[, 1]          # noise
x4 <- scale(runif(n, -1, 1))[, 1]   # noise

# Hidden state process with two states
trans_mat <- matrix(c(0.95, 0.05,
                      0.05, 0.95),
                    nrow = 2, byrow = TRUE)
state <- numeric(n)
state[1] <- sample(1:2, 1)
for (t in 2:n) {
  state[t] <- sample(1:2, size = 1, prob = trans_mat[state[t - 1], ])
}

# Observation model: only x1 and x2 influence the mean
mu1 <- -0.5 + 1.3 * x1 - 1.5 * x2
mu2 <- 0.5 - 1.2 * x1 + 1.6 * x2
sigma <- c(0.4, 0.6)

obs_mean <- ifelse(state == 1, mu1, mu2)
obs_sd <- sigma[state]
y <- rnorm(n, mean = obs_mean, sd = obs_sd)

dat <- data.frame(
  ID = factor(1),
  y = y,
  x1 = as.numeric(x1),
  x2 = as.numeric(x2),
  x3 = as.numeric(x3),
  x4 = as.numeric(x4)
)

# Helper to build observation models -------------------------------------
create_observation <- function(data, horseshoe) {
  formulas <- list(y = list(mean = ~ x1 + x2 + x3 + x4, sd = ~ 1))
  par0 <- list(y = list(mean = c(mean(data$y) - 0.5, mean(data$y) + 0.5),
                        sd = c(sd(data$y), sd(data$y))))
  obs <- Observation$new(
    data = data,
    dists = list(y = "norm"),
    n_states = 2,
    par = par0,
    formulas = formulas,
    horseshoe = horseshoe
  )
  obs$update_par(par = obs$suggest_initial())
  obs
}

obs_hs <- create_observation(dat, horseshoe = TRUE)
obs_no_hs <- create_observation(dat, horseshoe = FALSE)

# Hidden process (no covariates) -----------------------------------------
create_hidden <- function(data) {
  MarkovChain$new(data = data, n_states = 2)
}

hid_hs <- create_hidden(dat)
hid_no_hs <- create_hidden(dat)

# Build HMM objects -------------------------------------------------------
model_hs <- HMM$new(obs = obs_hs, hid = hid_hs)
model_no_hs <- HMM$new(obs = obs_no_hs, hid = hid_no_hs)

# Fit models using Stan ---------------------------------------------------
message("Fitting horseshoe prior model ...")
model_hs$fit_stan(iter = 1000, warmup = 500, chains = 1, refresh = 0)

message("Fitting non-horseshoe model ...")
model_no_hs$fit_stan(iter = 1000, warmup = 500, chains = 1, refresh = 0)

# Summaries ---------------------------------------------------------------
credible_summary <- function(model, label) {
  draws <- model$iters(type = "raw")
  draws_df <- as.data.frame(draws)
  coeff_cols <- grep("^coeff_fe_obs", names(draws_df), value = TRUE)
  summary <- t(vapply(draws_df[coeff_cols], function(x) {
    c(mean = mean(x),
      `2.5%` = quantile(x, 0.025),
      `97.5%` = quantile(x, 0.975))
  }, numeric(3)))
  summary <- as.data.frame(summary)
  summary$parameter <- coeff_cols
  summary$model <- label
  summary[, c("model", "parameter", "mean", "2.5%", "97.5%")]
}

res_hs <- credible_summary(model_hs, "horseshoe")
res_no_hs <- credible_summary(model_no_hs, "no_horseshoe")

print(res_hs)
print(res_no_hs)
