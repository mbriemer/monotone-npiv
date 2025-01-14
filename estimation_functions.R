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

calculate_minimand <- function(b, y, p, q) {
  minimand <- t(y - p %*% b) %*% q %*%
    solve(crossprod(q)) %*% t(q) %*% (y - p %*% b)
  return(minimand)
}

estimate <- function(y, p, q, shape = "increasing") {
  if (shape == "unconstrained") {
    beta <- solve(t(p) %*% q %*% solve(crossprod(q)) %*% t(q) %*% p) %*%
      t(p) %*% q %*% solve(crossprod(q)) %*% t(q) %*% y
    return(beta)
  }

  k <- ncol(p)

  wrapper <- function(b) {
    calculate_minimand(b, y, p, q)
  }

  if (k == 1) {
    beta <- optim(
      par = rep(1, k),
      fn = calculate_minimand,
      y = y,
      p = p,
      q = q
    )
  }
  if (k == 2) {
    beta <- optim(
      par = rep(1, k),
      fn = calculate_minimand,
      y = y,
      p = p,
      q = q,
      method = "L-BFGS-B",
      lower = c(- Inf, 0)
    )

  } else {
    if (k == 3) {
      constraints <- matrix(
        data = c(0, 1, 0,
                 0, 1, 2),
        nrow = 2,
        ncol = 3,
        byrow = TRUE
      )
    }
    if (k == 4) {
      constraints <- matrix(
        data = c(0, 1, 0, 0,
                 0, 1, 1, 0,
                 0, 1, 2, 3),
        nrow = 3,
        ncol = 4,
        byrow = TRUE
      )
    }
    if (k == 5) {
      constraints <- matrix(
        data = c(0, 1, 0, 0,
                 0, 1, 2 / 3, 0,
                 0, 1, 4 / 3, 2 / 3),
        nrow = 3,
        ncol = 4,
        byrow = TRUE
      )
    }

    beta <- constrOptim(
      theta = rep(1, k),
      f = wrapper,
      grad = NULL,
      ui = constraints,
      ci = rep(0, nrow(constraints))
    )
  }
  return(beta$par)
}