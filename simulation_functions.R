# Simulation constants and functions

# Simulation parameters

set_sim_parameters <- function(iterations, sample_size, model, k, j, sigma = 0.5, rho = 0.3, eta = 0.3) {
  sim_parameters <- list(iterations = iterations,
                         sample_size = sample_size,
                         model = model,
                         k = k,
                         j = j, #TODO enforce j >= k?
                         sigma = sigma,
                         rho = rho,
                         eta = eta)
  return(sim_parameters)
}

# Noise terms

make_noise_terms <- function(n, sigma, eta) {
  zeta <- rnorm(n, 0, 1)
  lunate_epsilon <- rnorm(n, 0, 1)
  ny <- rnorm(n, 0, 1)
  epsilon <- sigma * (eta * lunate_epsilon + sqrt(1 - eta^2) * ny)

  noise_terms <- list(zeta = zeta,
                      lunate_epsilon = lunate_epsilon,
                      ny = ny,
                      epsilon = epsilon)
  return(noise_terms)
}

# Data generating functions

sample_w <- function(zeta) {
  w <- pnorm(zeta, 0, 1)
  return(w)
}

sample_x <- function(rho, zeta, lunate_epsilon) {
  x <- pnorm(rho * zeta + sqrt(1 - rho^2) * lunate_epsilon, 0, 1)
  return(x)
}

g <- function(x, model_no) {
  if (model_no == 1) {
    return(x^2 + 0.2 * x)
  }
  if (model_no == 2) {
    return(2 * pmax(0, x - 1 / 2)^2 + 0.5 * x)
  }
}

sample_y <- function(x, epsilon, model_no) {
  y <- g(x, model_no) + epsilon
  return(y)
}

sim <- function(sim_parameters) {

  cat("Running simulation with parameters:", as.character(sim_parameters), "\n")

  n <- sim_parameters$sample_size
  k <- sim_parameters$k

  ises_u <- numeric(length = sim_parameters$iterations)
  ises_c <- numeric(length = sim_parameters$iterations)

  cl <- makeCluster(detectCores() - 1)
  clusterEvalQ(cl, source("simulation_functions.R"))
  clusterEvalQ(cl, source("estimation_functions.R"))

  results <- parLapply(cl = cl,
                       X = 1:sim_parameters$iterations,
                       fun = function(x) {
                         noise_terms <- make_noise_terms(n = n,
                                                         sigma = sim_parameters$sigma,
                                                         eta = sim_parameters$eta)
                         w <- sample_w(zeta = noise_terms$zeta)
                         x <- sample_x(rho = sim_parameters$rho,
                                       zeta = noise_terms$zeta,
                                       lunate_epsilon = noise_terms$lunate_epsilon)
                         y <- sample_y(x = x, epsilon = noise_terms$epsilon, model_no = 2)

                         p <- evaluate_basis(x, sim_parameters$k)
                         q <- evaluate_basis(w, sim_parameters$j)

                         beta_u <- estimate(y = y,
                                            p = p,
                                            q = q,
                                            shape = "unconstrained")

                         beta_c <- estimate(y = y,
                                            p = p,
                                            q = q)

                         ise_u <- integrate(function(x) (g(x, model_no = 2) - evaluate_basis(x, k) %*% beta_u)^2, 0, 1)$value
                         ise_c <- integrate(function(x) (g(x, model_no = 2) - evaluate_basis(x, k) %*% beta_c)^2, 0, 1)$value

                         return(c(ise_u, ise_c))
  })

  stopCluster(cl)

  matrix_results <- do.call(rbind, results)

  mise_u <- median(matrix_results[, 1])
  mise_c <- median(matrix_results[, 2])
  
  return(c(mise_u, mise_c))
}