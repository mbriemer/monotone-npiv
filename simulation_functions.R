# Simulation constants and functions

# Simulation parameters

set_sim_parameters <- function(sample_size, model, k, j, sigma = 0.3, rho = 0.5, eta = 0.5) {
  sim_parameters <- list(sample_size = sample_size,
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

# Main simulation function

sim <- function(sim_parameters){
  n <- sim_parameters$sample_size
  k <- sim_parameters$k

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

  ise_u <- integrate(function(x) (g(x, model_no = 2) - evaluate_basis(x, sim_parameters$k) %*% beta_u)^2, 0, 1)$value
  ise_c <- integrate(function(x) (g(x, model_no = 2) - evaluate_basis(x, sim_parameters$k) %*% beta_c)^2, 0, 1)$value
  
  return(c(ise_u, ise_c))
}
