#library(fda)

source("simulation_functions.R")
source("estimation_functions.R")

# Main

ises_u <- c()
ises_c <- c()

#sim_setting <- matrix(data = )

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