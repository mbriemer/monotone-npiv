### DO NOT RUN
# This version was written hurriedly to be used for submitting the project on time.
# It is not intended to run flawlessly without further modifications.

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

results_flat <- data.frame(matrix(data = numeric(), ncol = 2))

for (i in 1:nrow(params)) {
  sim_parameters <- set_sim_parameters(iterations = params$iter[i],
                                       sample_size = params$sample_size[i],
                                       model = params$model[i],
                                       k = params$k[i],
                                       j = params$j[i])
  results_flat <- rbind(results_flat, sim(sim_parameters))
}
colnames(results_flat) <- c("unconstrained", "increasing")
results_flat <- results_flat * 1000

write.csv(results_flat, file = "results_flat.csv", row.names = FALSE)

params_flat <- params

test <- data.frame(params_flat, row.names = 1:nrow(params_flat))

results_flat_10 <- results_flat[which(params_flat$model == 10, ), ]

library(tidyverse)

params_flat_test <- params_flat
params_flat_test$join_key <- row_number(params_flat)
values_test <- results_flat
values_test$join_key <- row_number(values_test)

results_flat <- read.csv("simulation_results/results_flat.csv")

test2 <- params_flat_test
test2$unconstrained <- results_flat$unconstrained
test2$increasing <- results_flat$increasing

# Rearranging data and stargazing----------------------------------------------
library(stargazer)
# unconstrained----------------------------------------------------------------

df_wide <- test2 %>%
  select(sample_size, model, k, unconstrained) %>%
  pivot_wider(names_from = k,
              values_from = results) %>%
  select(-"5") %>%
  round(2) %>%
  group_by(model) %>%
  group_split()

stargazer(df_wide[[1]],
          type = "latex",
          title = "Simulation results",
          #label = "tab:paper",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          #column.labels = c("Unconstrained", "Increasing"),
          out = "model10_u.tex")

stargazer(df_wide[[2]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model11_u.tex")

stargazer(df_wide[[3]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model12_u.tex")

stargazer(df_wide[[4]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model13_u.tex")

stargazer(df_wide[[5]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model14_u.tex")

stargazer(df_wide[[6]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model15_u.tex")

stargazer(df_wide[[7]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model16_u.tex")

stargazer(df_wide[[8]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model17_u.tex")

stargazer(df_wide[[9]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model18_u.tex")

# increasing-------------------------------------------------------------------

df_wide <- test2 %>%
  select(sample_size, model, k, increasing) %>%
  pivot_wider(names_from = k,
              values_from = increasing) %>%
  select(-"5") %>%
  round(2) %>%
  group_by(model) %>%
  group_split()

stargazer(df_wide[[1]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model10_i.tex")

stargazer(df_wide[[2]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model11_i.tex")

stargazer(df_wide[[3]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model12_i.tex")

stargazer(df_wide[[4]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model13_i.tex")

stargazer(df_wide[[5]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model14_i.tex")

stargazer(df_wide[[6]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model15_i.tex")

stargazer(df_wide[[7]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model16_i.tex")

stargazer(df_wide[[8]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model17_i.tex")

stargazer(df_wide[[9]],
          type = "latex",
          title = "Simulation results",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "model18_i.tex")

# Ratio------------------------------------------------------------------------

ratios <- test2 %>%
  select(sample_size, model, k, unconstrained, increasing) %>%
  pivot_longer(cols = c(unconstrained, increasing)) %>%
  spread(key = name, value = value) %>%
  mutate(result = increasing / unconstrained) %>%
  drop_na() %>%
  select(sample_size, model, k, result) %>%
  pivot_wider(names_from = k,
              values_from = result) %>%
  round(2) %>%
  group_by(model) %>%
  group_split()

stargazer(ratios[[1]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_10.tex")

stargazer(ratios[[2]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_11.tex")

stargazer(ratios[[3]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_12.tex")

stargazer(ratios[[4]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_13.tex")

stargazer(ratios[[5]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_14.tex")

stargazer(ratios[[6]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_15.tex")

stargazer(ratios[[7]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_16.tex")

stargazer(ratios[[8]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_17.tex")

stargazer(ratios[[9]],
          type = "latex",
          title = "Ratios",
          summary = FALSE,
          digits = 2,
          header = FALSE,
          out = "ratios_18.tex")






for(g in 1:length(df_wide)) {

  filepath <- cat("test_", df_wide[[g]]$model, ".tex", sep = "")
  stargazer(df_wide[[g]],
            type = "latex",
            title = "Simulation results",
            #label = "tab:paper",
            summary = FALSE,
            digits = 2,
            header = FALSE,
            #column.labels = c("Unconstrained", "Increasing"),
            out = )
}

df_wide_i <- test2 %>%
  select(sample_size, model, k, increasing) %>%
  pivot_wider(names_from = k,
              values_from = increasing)


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