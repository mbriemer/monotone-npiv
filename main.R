# Simulation constants and functions

# Simulation parameters

set_sim_parameters <- function(k, j, sigma = 0.3, rho = 0.5, eta = 0.5) {
  sim_parameters <- list(k = k,
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

# Estimation functions

evaluate_basis <- function(x, basis_dimension) {
  if (basis_dimension == 1) {
    return(1)
  }
  if (basis_dimension == 2) {
    return(cbind(1, x))
  }
  if (basis_dimension == 3) {
    return(cbind(1, x, x^2))
  }
  if (basis_dimension == 4) {
    return(cbind(1, x, x^2, pmax(0, x - 1 / 2)^2))
  }
  if (basis_dimension == 5) {
    return(cbind(1, x, x^2, pmax(0, x - 1 / 3)^2, pmax(0, x - 2 / 3)^2))
  }
}

calculate_minimand <- function(b, y, x, w, p, q) {
  minimand <- t(y - p %*% b) %*% q %*% solve(crossprod(q)) %*% t(q) %*% (y - p %*% b)
  return(minimand)
}

estimate <- function(y, x, w, p, q, constraint = "none") {
  k <- ncol(p)
  lower <- rep(-Inf, k)
  upper <- rep(Inf, k)

  if (constraint == "increasing") {
    lower <- c(-Inf, rep(0, k - 1)) #TODO maybe more complex for j,k >= 4
  }

  if (constraint == "decreasing") {
    upper <- c(Inf, rep(0, k - 1)) #TODO maybe more complex for j,k >= 4
  }

  beta <- optim(par = rep(1, k),
                fn = calculate_minimand,
                y = y,
                x = x,
                w = w,
                p = p,
                q = q,
                method = "L-BFGS-B",
                lower = lower,
                upper = upper
)
  return(beta$par)
}

# Main

sample_size <- 100

sim_parameters <- set_sim_parameters(k = 3, j = 3)
noise_terms <- make_noise_terms(n = sample_size,
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
                   x = x,
                   w = w,
                   p = p,
                   q = q,
                   constraint = "none")

beta_c <- estimate(y = y,
                   x = x,
                   w = w,
                   p = p,
                   q = q,
                   constraint = "increasing")

# Simulation results

y_hat_u <- p %*% beta_u
y_hat_c <- p %*% beta_c

plot(x, g(x, model_no = 2), col = "black")
points(x, y_hat_u, col = "red")
points(x, y_hat_c, col = "blue")

# Calculate MSE
# TODO Should be possible to find an analytic expression for the MISE?

mse_u <- mean((y - y_hat_u)^2)
mse_c <- mean((y - y_hat_c)^2)