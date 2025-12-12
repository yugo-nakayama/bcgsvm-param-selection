library(targets)

tar_option_set(
  packages = c("stats")
)

# source your functions
source("R/core.R")

list(
  tar_target(
    sim_data,
    make_toy_data(n1 = 20, n2 = 10, d = 256, seed = 1)
  ),
  tar_target(
    gamma_hat,
    optimize_gamma_snr(sim_data$X, sim_data$y)
  ),
  tar_target(
    loss_at_hat,
    loss_snr(gamma_hat, sim_data$X, sim_data$y)
  )
)

