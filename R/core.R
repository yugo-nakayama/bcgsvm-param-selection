# ---- Data generator (toy) ----
make_toy_data <- function(n1, n2, d, seed = 1) {
  set.seed(seed)
  X1 <- matrix(rnorm(n1 * d, mean = 0, sd = 1), n1, d)
  X2 <- matrix(rnorm(n2 * d, mean = 0.3, sd = 1.2), n2, d)
  X <- rbind(X1, X2)
  y <- factor(c(rep("class1", n1), rep("class2", n2)))
  list(X = X, y = y)
}

# ---- Squared Euclidean distances ----
sq_dist_mat <- function(X) {
  G <- tcrossprod(X)
  s <- rowSums(X^2)
  D <- outer(s, s, "+") - 2 * G
  D[D < 0] <- 0
  D
}

gaussian_kernel_from_dist <- function(D, gamma) {
  exp(-D / gamma)
}

# ---- U-stat style estimators (paper-style) ----
estimate_eta_delta <- function(K, y) {
  y <- as.factor(y)
  lev <- levels(y)
  stopifnot(length(lev) == 2)

  i1 <- which(y == lev[1]); n1 <- length(i1)
  i2 <- which(y == lev[2]); n2 <- length(i2)

  K11 <- K[i1, i1, drop = FALSE]
  K22 <- K[i2, i2, drop = FALSE]
  K12 <- K[i1, i2, drop = FALSE]

  diag1 <- sum(diag(K11)); all1 <- sum(K11)
  diag2 <- sum(diag(K22)); all2 <- sum(K22)

  eta1_hat <- diag1 / (n1 - 1) - all1 / (n1 * (n1 - 1))
  eta2_hat <- diag2 / (n2 - 1) - all2 / (n2 * (n2 - 1))

  Delta_star_hat <- all1 / (n1^2) + all2 / (n2^2) - 2 * sum(K12) / (n1 * n2)
  Delta_hat <- Delta_star_hat - eta1_hat / n1 - eta2_hat / n2

  list(n1 = n1, n2 = n2, eta1 = eta1_hat, eta2 = eta2_hat,
       Delta_star = Delta_star_hat, Delta = Delta_hat)
}

# ---- Proposed loss: SNR-type ----
loss_snr <- function(gamma, X, y, eps = 1e-12, penalty = 1e6) {
  if (!is.finite(gamma) || gamma <= 0) return(Inf)

  D <- sq_dist_mat(X)
  K <- gaussian_kernel_from_dist(D, gamma)
  est <- estimate_eta_delta(K, y)

  if (!is.finite(est$Delta) || est$Delta <= eps) return(penalty)

  (est$eta1 / est$n1 + est$eta2 / est$n2) / (est$Delta^2)
}

# ---- 1D optimization on log-scale ----
optimize_gamma_snr <- function(X, y, gamma_min = NULL, gamma_max = NULL) {
  D <- sq_dist_mat(X)
  dvals <- D[upper.tri(D)]

  if (is.null(gamma_min)) gamma_min <- max(as.numeric(stats::quantile(dvals, 0.1)) / 100, .Machine$double.eps)
  if (is.null(gamma_max)) gamma_max <- as.numeric(stats::quantile(dvals, 0.9)) * 100

  obj <- function(logg) loss_snr(exp(logg), X, y)
  res <- stats::optimize(obj, interval = log(c(gamma_min, gamma_max)))
  exp(res$minimum)
}
