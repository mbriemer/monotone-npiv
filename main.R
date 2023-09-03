#library(fda)
library(dplyr)
library(purrr)
library(furrr)

source("simulation_functions.R")
source("estimation_functions.R")

# Main



ises_u <- c()
ises_c <- c()

sample_sizes <- c(500,
                  1000,
                  5000)
                  #10000,
                  #50000,
                  #100000,
                  #500000)

models <- c(2)

k <- c(2, 3)
j <- c(2, 3)

rho <- c(0.3)
eta <- c(0.3)

iterations <- 50

results <- sim(sample_size = 100,
               model = 2,
               k = 4,
               j = 4,
               sigma = 0.5,
               rho = 0.3,
               eta = 0.3)

params <- expand.grid(sample_size = sample_sizes,
                      model = models,
                      k = k,
                      j = j,
                      sigma = 0.5,
                      rho = rho,
                      eta = eta)

params <- params[params$j >= params$k, ]
params <- params[rep(seq_len(nrow(params)), each = 50), ]

#params <- params %>% filter(k <= j) %>% slice(rep(1:n(), each = 50) 


#%>% slice(rep(1:n), each = 50)

params_iter <- expand.grid(iterations,
                           params)

#list_wrapper <- function(sample_size, model, k, j, sigma, rho, eta) {
#  sim_parameters <- list(sample_size, model, k, j, sigma, rho, eta)
#  return(sim(sim_parameters))
#}

future::plan(future::multisession)

results <- future_pmap(params,
                       sim,
                       .progress = TRUE,
                       .options = furrr_options(seed = TRUE))

results <- pmap(params, sim, .progress = TRUE)

sim_parameters <- set_sim_parameters(sample_size = 100,
                                     model = 2,
                                     k = 4,
                                     j = 4)

ises <- sim(sim_parameters = sim_parameters)

ises_u <- c(ises_u, ises[1])
ises_c <- c(ises_c, ises[2])

mise_u <- median(ises_u)
mise_c <- median(ises_c)

#p_basis <- create.exponential.basis(rangeval = range(x),
#                                    nbasis = sim_parameters$k)
#q_basis <- create.exponential.basis(rangeval = range(w),
#                                    nbasis = sim_parameters$j)

#p <- eval.basis(x, p_basis)
#q <- eval.basis(w, q_basis)

# Simulation results

#y_hat_u <- p %*% beta_u
#y_hat_c <- p %*% beta_c

#plot(x, g(x, model_no = 2), col = "black")
#points(x, y_hat_u, col = "red")
#points(x, y_hat_c, col = "blue")

# Calculate MSE
# TODO Should be possible to find an analytic expression for the MISE?