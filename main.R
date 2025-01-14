# Main
library(stargazer)
library(tidyverse)

source("simulation_functions.R")
# Calls library(parallel) and runs source("estimation_functions.R")

# Partially flat functions-----------------------------------------------------

iter <- 50 #500
sample_sizes <- c(
  50,
  100
)#,
  #500,
  #1000,
  #5000,
  #10000)#,
  #50000)#,
  #100000,
  #500000)
model <- 10:18
k <- c(2, 3, 4)
j <- c(2, 3, 4)
rho <- c(0.3)
eta <- c(0.3)

params <- expand.grid(
  iter = iter,
  sample_size = sample_sizes,
  model = model,
  k = k,
  j = j,
  rho = rho,
  eta = eta
)
params <- params[params$k == params$j, ]
params$unconstrained <- NA
params$increasing <- NA

for (i in seq_len(nrow(params))) {
  sim_parameters <- set_sim_parameters(
    iterations = params$iter[i],
    sample_size = params$sample_size[i],
    model = params$model[i],
    k = params$k[i],
    j = params$j[i]
  )
  results <- sim(sim_parameters)
  params$unconstrained[i] <- results[1] * 1000
  params$increasing[i] <- results[2] * 1000
}

simulation_results <- params

# Rearranging data and stargazing----------------------------------------------
# Unconstrained----------------------------------------------------------------

df_wide <- simulation_results %>%
  select(sample_size, model, k, unconstrained) %>%
  pivot_wider(
    names_from = k,
    values_from = unconstrained
  ) %>%
  round(2) %>%
  group_by(model) %>%
  group_split()

stargazer(
  df_wide[[1]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model10_u.tex"
)

stargazer(
  df_wide[[2]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model11_u.tex"
)

stargazer(
  df_wide[[3]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model12_u.tex"
)

stargazer(
  df_wide[[4]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model13_u.tex"
)

stargazer(
  df_wide[[5]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model14_u.tex"
)

stargazer(
  df_wide[[6]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model15_u.tex"
)

stargazer(
  df_wide[[7]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model16_u.tex"
)

stargazer(
  df_wide[[8]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model17_u.tex"
)

stargazer(
  df_wide[[9]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model18_u.tex"
)

## Increasing-------------------------------------------------------------------

df_wide <- simulation_results %>%
  select(sample_size, model, k, increasing) %>%
  pivot_wider(names_from = k, values_from = increasing) %>%
  round(2) %>%
  group_by(model) %>%
  group_split()

stargazer(
  df_wide[[1]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model10_i.tex"
)

stargazer(
  df_wide[[2]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model11_i.tex"
)

stargazer(
  df_wide[[3]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model12_i.tex"
)

stargazer(
  df_wide[[4]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model13_i.tex"
)

stargazer(
  df_wide[[5]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model14_i.tex"
)

stargazer(
  df_wide[[6]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model15_i.tex"
)

stargazer(
  df_wide[[7]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model16_i.tex"
)

stargazer(
  df_wide[[8]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model17_i.tex"
)

stargazer(
  df_wide[[9]],
  type = "latex",
  title = "Simulation results",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "model18_i.tex"
)

# Ratios------------------------------------------------------------------------

ratios <- simulation_results %>%
  select(sample_size, model, k, unconstrained, increasing) %>%
  pivot_longer(cols = c(unconstrained, increasing)) %>%
  spread(key = name, value = value) %>%
  mutate(result = increasing / unconstrained) %>%
  drop_na() %>%
  select(sample_size, model, k, result) %>%
  pivot_wider(
    names_from = k,
    values_from = result
  ) %>%
  round(2) %>%
  group_by(model) %>%
  group_split()

stargazer(
  ratios[[1]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_10.tex"
)

stargazer(
  ratios[[2]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_11.tex"
)

stargazer(
  ratios[[3]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_12.tex"
)

stargazer(
  ratios[[4]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_13.tex"
)

stargazer(
  ratios[[5]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_14.tex"
)

stargazer(
  ratios[[6]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_15.tex"
)

stargazer(
  ratios[[7]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_16.tex"
)

stargazer(
  ratios[[8]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_17.tex"
)

stargazer(
  ratios[[9]],
  type = "latex",
  title = "Ratios",
  summary = FALSE,
  digits = 2,
  header = FALSE,
  out = "ratios_18.tex"
)