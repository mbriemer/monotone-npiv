library(fda)

source("my_basisfd.R")
source("my_getbasismatrix.R")
source("my_getbasispenalty.R")
source("create.regression_splines.basis.R")
# TODO Do I need "functions analogous to fourier and fourierpen for evaluating basis functions for basis penalty matrices"?

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

make_p <- function(basis, x){ # TODO simplify
  p <- eval.basis(x, basis)
  return(p)
}

make_q <- function(basis, w) {
  q <- eval.basis(w, basis)
  return(q)
}

estimate <- function(b, y, x, w, p_k, q_k){

  p <- make_p(p_k, x) # TODO move?
  q <- make_q(q_k, w)

  minimand <- t(y - p %*% b) %*% q %*% solve(tcrossprod(q)) %*% t(q) %*% (y - p %*% b)
  return(minimand)
}

# Main

sample_size <- 100

sim_parameters <- set_sim_parameters(k = 4, j = 4)
noise_terms <- make_noise_terms(n = sample_size,
                                sigma = sim_parameters$sigma,
                                eta = sim_parameters$eta)

w <- sample_w(zeta = noise_terms$zeta)
x <- sample_x(rho = sim_parameters$rho,
              zeta = noise_terms$zeta,
              lunate_epsilon = noise_terms$lunate_epsilon)
y <- sample_y(x = x, epsilon = noise_terms$epsilon, model_no = 1)

basis <- create.bspline.basis(nbasis = sim_parameters$k)
plot(basis)

p_k <- create.exponential.basis(nbasis = sim_parameters$k)
q_k <- create.exponential.basis(nbasis = sim_parameters$j)

p <- make_p(p_k, x)
q <- make_q(q_k, w)

minimize <- optim(
  par = rep(0.5, sim_parameters$k),
  fn = estimate,
  y = y,
  x = x,
  w = w,
  p_k = p_k,
  q_k = q_k,
)