library(fda)

# Simulation constants and functions

# Simulation parameters

set_sim_parameters <- function(K, J, sigma = 0.3, rho = 0.5, eta = 0.5) {
  sim_parameters <- list(K = K,
                         J = J,
                         sigma = sigma,
                         rho = rho,
                         eta = eta)
  return(sim_parameters)
}

# Noise terms

make_noise_terms <- function(N, sigma, eta) {
  zeta <- rnorm(N, 0, 1)
  lunate_epsilon <- rnorm(N, 0, 1)
  ny <- rnorm(N, 0, 1)
  epsilon <- sigma * (eta * lunate_epsilon + sqrt(1 - eta^2) * ny)

  noise_terms <- list(zeta = zeta,
                      lunate_epsilon = lunate_epsilon,
                      ny = ny,
                      epsilon = epsilon)
  return(noise_terms)
}

# Data generating functions

sample_W <- function(zeta) {
  W <- pnorm(zeta, 0, 1)
  return(W)
}

sample_X <- function(rho, zeta, lunate_epsilon) {
  X <- pnorm(rho * zeta + sqrt(1 - rho^2) * lunate_epsilon, 0, 1)
  return(X)
}

g <- function(X, model_no) {
  if (model_no == 1){
    return(X^2 + 0.2 * X)
  }
  if (model_no == 2){
    return(2 * max(0, X - 1 / 2)^2 + 0.5 * X)
  }
}

sample_Y <- function(X, epsilon, model_no) {
  Y <- g(X, model_no) + epsilon
  return(Y)
}

# estimation functions

make_P <- function(basis, X){
  P <- eval.basis(X, basis)
  return(P)
}

make_Q <- function(basis, W) {
  Q <- eval.basis(W, basis)
  return(Q)
}

estimate <- function(b, Y, X, W, p_k, q_k){

  P <- make_P(p_k, X)
  Q <- make_Q(q_k, W)

  #print("P")
  #print(P)
  #print("b")
  #print(b)

  minimand <- t(Y - P %*% b) %*% Q %*% solve(tcrossprod(Q)) %*% t(Q) %*% (Y - P %*% b)
  return(minimand)
}

# Main

sample_size <- 100

sim_parameters <- set_sim_parameters(K = 4, J = 4)
noise_terms <- make_noise_terms(N = sample_size,
                                sigma = sim_parameters$sigma,
                                eta = sim_parameters$eta)

W <- sample_W(zeta = noise_terms$zeta)
X <- sample_X(rho = sim_parameters$rho,
              zeta = noise_terms$zeta,
              lunate_epsilon = noise_terms$lunate_epsilon)
Y <- sample_Y(X = X, epsilon = noise_terms$epsilon, model_no = 1)

basis <- create.bspline.basis(nbasis = sim_parameters$K)
plot(basis)

p_k <- create.exponential.basis(nbasis = sim_parameters$K)
q_k <- create.exponential.basis(nbasis = sim_parameters$J)

P <- make_P(p_k, X)
Q <- make_Q(q_k, W)

minimize <- optim(
  par = rep(0.5, sim_parameters$K),
  fn = estimate,
  Y = Y,
  X = X,
  W = W,
  p_k = p_k,
  q_k = q_k,
)