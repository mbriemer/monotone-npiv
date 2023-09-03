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

g <- function(x, model) {
  if (model == 1) {
    return(x^2 + 0.2 * x)
  }
  if (model == 2) {
    return(2 * pmax(0, x - 1 / 2)^2 + 0.5 * x)
  }
  if (model == 3) {
    return(exp(x))
  }
  if (model == 4) {
    return(1 / (1 + exp(- 10 * (x - 1 / 3))) + 1 / (1 + exp(- 10 * (x - 2 / 3))))
  }
  if (model == 5) {
    return(pmax(0, x - 1 / 2)^3)
  }
  if (model == 6) {
    return(0.5 * sin(10 * x))
  }
  if (model == 7) {
    return(-x^2 + 1)
  }
}

sample_y <- function(x, epsilon, model) {
  y <- g(x, model) + epsilon
  return(y)
}

# Main simulation function

sim <- function(sim_parameters) {

  cat("Running simulation with parameters:", as.character(sim_parameters), "\n")

  cl <- makeCluster(detectCores() - 1)
  clusterEvalQ(cl, source("simulation_functions.R"))
  clusterEvalQ(cl, source("estimation_functions.R"))

  ises <- parLapply(cl = cl,
                    X = 1:sim_parameters$iterations,
                    fun = function(x) {
                      noise_terms <- make_noise_terms(n = sim_parameters$sample_size,
                                                      sigma = sim_parameters$sigma,
                                                      eta = sim_parameters$eta)
                      w <- sample_w(zeta = noise_terms$zeta)
                      x <- sample_x(rho = sim_parameters$rho,
                                    zeta = noise_terms$zeta,
                                    lunate_epsilon = noise_terms$lunate_epsilon)
                      y <- sample_y(x = x, epsilon = noise_terms$epsilon, model = 2)

                      p <- evaluate_basis(x, sim_parameters$k)
                      q <- evaluate_basis(w, sim_parameters$j)

                      beta_u <- estimate(y = y,
                                         p = p,
                                         q = q,
                                         shape = "unconstrained")

                      beta_c <- estimate(y = y,
                                         p = p,
                                         q = q)

                      ise_u <- integrate(function(x) (g(x, model = 2) - evaluate_basis(x, sim_parameters$k) %*% beta_u)^2, 0, 1)$value #sic!
                      ise_c <- integrate(function(x) (g(x, model = 2) - evaluate_basis(x, sim_parameters$j) %*% beta_c)^2, 0, 1)$value #sic!

                      return(c(ise_u, ise_c))
                    }
                    )

  stopCluster(cl)

  matrix_results <- do.call(rbind, ises)

  mise_u <- median(matrix_results[, 1])
  mise_c <- median(matrix_results[, 2])
  
  return(c(mise_u, mise_c))
}
