# Simulation constants and functions

# Simulation parameters

set_sim_parameters <- function(k, j, sigma = 0.3, rho = 0.5, eta = 0.5) {
  sim_parameters <- list(k= k,
                         j = j, #TODO enforce j >= k?
                         sigma = sigma,
                         rho = rho,
                         eta = eta)
  return(sim_parameters)
}

# noise terms

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
  if (model_no == 1){
    return(x^2 + 0.2 * x)
  }
  if (model_no == 2){
    return(2 * max(0, x - 1 / 2)^2 + 0.5 * x)
  }
}

sample_y <- function(x, epsilon, model_no) {
  y <- g(x, model_no) + epsilon
  return(y)
}

# estimation functions

evaluate_basis <- function(x, basis_dimension) {
  if (basis_dimension == 1){
    return(1)
  }
  if (basis_dimension == 2){
    return(cbind(1, x))
  }
  if (basis_dimension == 3){
    return(cbind(1, x, x^2))
  }
  if (basis_dimension == 4){
    return(cbind(1, x, x^2, max(0, x - 1 / 2)^2))
  }
  if (basis_dimension == 5){
    return(cbind(1, x, x^2, max(0, x - 1 / 3)^2, max(0, x - 2 / 3)^2))
  }
}

estimate <- function(b, y, x, w, p, q){

  minimand <- t(y - p %*% b) %*% q %*% solve(tcrossprod(q)) %*% t(q) %*% (y - p %*% b)
  return(minimand)
}

# Main

sample_size <- 10

sim_parameters <- set_sim_parameters(k = 3, j = 3)
noise_terms <- make_noise_terms(n = sample_size,
                                sigma = sim_parameters$sigma,
                                eta = sim_parameters$eta)

w <- sample_w(zeta = noise_terms$zeta)
x <- sample_x(rho = sim_parameters$rho,
              zeta = noise_terms$zeta,
              lunate_epsilon = noise_terms$lunate_epsilon)
y <- sample_y(x = x, epsilon = noise_terms$epsilon, model_no = 1)

p <- evaluate_basis(x, sim_parameters$k)
q <- evaluate_basis(w, sim_parameters$j)

minimize <- optim(
  par = rep(1, sim_parameters$k),
  fn = estimate,
  y = y,
  x = x,
  w = w,
  p = p,
  q = q,
)