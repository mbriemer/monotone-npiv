#library(fda)
library(parallel)

source("simulation_functions.R")
source("estimation_functions.R")

# Main

# Replicating the simulation from the paper------------------------------------

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

results_paper <- data.frame(matrix(data = numeric(), ncol = 2))

for (i in 1:nrow(params)) {
  sim_parameters <- set_sim_parameters(iterations = params$iter[i],
                                       sample_size = params$sample_size[i],
                                       model = params$model[i],
                                       k = params$k[i],
                                       j = params$j[i])
  results_paper <- rbind(results_paper, sim(sim_parameters))
}

colnames(results_paper) <- c("unconstrained", "increasing")
results_paper <- results_paper * 1000

write.csv(results_paper, file = "results_paper.csv", row.names = FALSE)

# Replicating the simulation from the web supplement---------------------------

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
rho <- c(0.3, 0.5)
eta <- c(0.3, 0.7)

params <- expand.grid(iter = iter,
                      sample_size = sample_sizes,
                      model = model,
                      k = k,
                      j = j,
                      rho = rho,
                      eta = eta)
params <- params[params$k == params$j, ]
params <- params[params$rho != params$eta, ]

results_supplement <- data.frame(matrix(data = numeric(), ncol = 2))

for (i in 1:nrow(params)) {
  sim_parameters <- set_sim_parameters(iterations = params$iter[i],
                                       sample_size = params$sample_size[i],
                                       model = params$model[i],
                                       k = params$k[i],
                                       j = params$j[i])
  results_supplement <- rbind(results_supplement, sim(sim_parameters))
}
colnames(results_supplement) <- c("unconstrained", "increasing")
results_supplement <- results_supplement * 1000

write.csv(results_supplement, file = "results_supplement.csv", row.names = FALSE)

# Weak instrument, high/low degree of endogeneity------------------------------

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
rho <- c(0.3, 0.1)
eta <- c(0.3, 0.9)

params <- expand.grid(iter = iter,
                      sample_size = sample_sizes,
                      model = model,
                      k = k,
                      j = j,
                      rho = rho,
                      eta = eta)
params <- params[params$k <= params$j, ]
params <- params[params$rho != params$eta, ]

results_iv <- data.frame(matrix(data = numeric(), ncol = 2))

for (i in 1:nrow(params)) {
  sim_parameters <- set_sim_parameters(iterations = params$iter[i],
                                       sample_size = params$sample_size[i],
                                       model = params$model[i],
                                       k = params$k[i],
                                       j = params$j[i],
                                       rho = params$rho[i],
                                       eta = params$eta[i])
  results_iv <- rbind(results_iv, sim(sim_parameters))
}
colnames(results_iv) <- c("unconstrained", "increasing")
results_iv <- results_iv * 1000

write.csv(results_iv, file = "results_iv.csv", row.names = FALSE)

# Harder to estimate functional forms------------------------------------------

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
model <- c(3, 4, 5, 6, 7)
k <- c(2)#, 3, 4, 5)
j <- c(3)#, 4, 5)
#rho <- c(0.3)
#eta <- c(0.3)

params <- expand.grid(iter = iter,
                      sample_size = sample_sizes,
                      model = model,
                      k = k,
                      j = j)#,
                      #rho = rho,
                      #eta = eta)
params <- params[params$k <= params$j, ]

results_hard <- data.frame(matrix(data = numeric(), ncol = 2))

for (i in 1:nrow(params)) {
  sim_parameters <- set_sim_parameters(iterations = params$iter[i],
                                       sample_size = params$sample_size[i],
                                       model = params$model[i],
                                       k = params$k[i],
                                       j = params$j[i])
  results_hard <- rbind(results_hard, sim(sim_parameters))
}
colnames(results_hard) <- c("unconstrained", "increasing")
results_hard <- results_hard * 1000

write.csv(results_hard, file = "results_hard.csv", row.names = FALSE)

# Partially flat functions-----------------------------------------------------

iter <- 500
sample_sizes <- c(50,
                  100,
                  500,
                  1000,
                  5000,
                  10000)#,
                  #50000)#,
                  #100000,
                  #500000)
model <- 10:18
k <- c(2, 3, 4, 5)
j <- c(2, 3, 4, 5)
#rho <- c(0.3)
#eta <- c(0.3)

params <- expand.grid(iter = iter,
                      sample_size = sample_sizes,
                      model = model,
                      k = k,
                      j = j)#,
                      #rho = rho,
                      #eta = eta)
params <- params[params$k == params$j, ]

results_hard <- data.frame(matrix(data = numeric(), ncol = 2))

for (i in 1:nrow(params)) {
  sim_parameters <- set_sim_parameters(iterations = params$iter[i],
                                       sample_size = params$sample_size[i],
                                       model = params$model[i],
                                       k = params$k[i],
                                       j = params$j[i])
  results_hard <- rbind(results_hard, sim(sim_parameters))
}
colnames(results_hard) <- c("unconstrained", "increasing")
results_hard <- results_hard * 1000

write.csv(results_hard, file = "results_hard.csv", row.names = FALSE)

# Different bases?-------------------------------------------------------------



# Plot tables------------------------------------------------------------------

library(stargazer)

stargazer(results_paper,
          type = "latex",
          title = "Replication of the simulation from the paper",
          label = "tab:paper",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          column.labels = c("Unconstrained", "Increasing"),
          out = "paper.tex")

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