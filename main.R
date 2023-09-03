#library(fda)
library(parallel)

source("simulation_functions.R")
source("estimation_functions.R")

# Main

iter <- 500
sample_sizes <- c(50,
                  100,
                  500,
                  1000,
                  5000,
                  10000,
                  50000)#,
                  #100000,
                  #500000)
model <- c(1, 2)
k <- c(2, 3, 4, 5)
j <- c(2, 3, 4, 5)

params <- expand.grid(iter = iter,
                      sample_size = sample_sizes,
                      model = model,
                      k = k,
                      j = j)

params <- params[params$k == params$j, ]

results <- data.frame(row.names = c("unconstrained", "increasing"))

#results <- foreach(i = 1:nrow(params)) %do% {
#  sim_parameters <- set_sim_parameters(iterations = params$iter[i],
#                                       sample_size = params$sample_size[i],
#                                       model = params$model[i],
#                                       k = params$k[i],
#                                       j = params$j[i])
#  mises <- sim(sim_parameters = sim_parameters)
#  params$MISE[i] <- mean(mises)
#}

# Main simulation function


for (i in 1:nrow(params)) {
  sim_parameters <- set_sim_parameters(iterations = params$iter[i],
                                       sample_size = params$sample_size[i],
                                       model = params$model[i],
                                       k = params$k[i],
                                       j = params$j[i])
  results <- rbind(results, sim(sim_parameters = sim_parameters))
}

results <- results * 1000

sim_parameters <- set_sim_parameters(iterations = 500,
                                     sample_size = 100,
                                     model = 2,
                                     k = 4,
                                     j = 4)

mises <- sim(sim_parameters = sim_parameters)

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