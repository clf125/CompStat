# Find the number of (fixed, non-adaptive) tangent points that minimizes
# TOTAL time to draw N samples -- not just the one that maximizes acceptance
# rate. More points = fewer rejections, but each proposal costs more, so
# there should be a sweet spot.

source("Assignments/Assignment 2/Target_setup.R")

## mode and curvature-based sd of log_f, computed directly (not copy-pasted)
y_hat <- optimize(log_f, interval = c(0, 10), maximum = TRUE)$maximum

second_derivative <- function(f, y, h = 0.0001) (f(y + h) - 2 * f(y) + f(y - h)) / h^2
sd_hat <- sqrt(-1 / second_derivative(log_f, y_hat))

## ---- Vectorized derivative evaluation (log_f_vec_faster comes from target_setup.R) ----

log_f_prime_vec <- function(y) {
  xy <- outer(dat$x, y)
  sum(dat$x * dat$z) - colSums(dat$x * exp(xy))
}

## ---- Build a fixed (non-adaptive) m-point envelope, same math as ars_sampling.R ----

build_envelope <- function(y_points) {
  y_points <- sort(y_points)
  m <- length(y_points)
  a <- log_f_prime_vec(y_points)
  b <- log_f_vec_faster(y_points) - a * y_points
  z <- (b[-1] - b[-m]) / (a[-m] - a[-1])
  list(a = a, b = b, z = c(0, z, Inf))
}

eval_V <- function(envelope, y) {
  # clamp: findInterval gives 0 for y < envelope$z[1] (=0), and indexing a
  # vector with 0 silently drops that element instead of giving NA, which
  # misaligns the recycling in a[i]*y for any later, in-range y too.
  i <- pmin(pmax(findInterval(y, envelope$z), 1), length(envelope$a))
  envelope$a[i] * y + envelope$b[i]
}

## ---- Vectorized proposal generation: draw n proposals at once -------------
## Same inverse-CDF math as sample_from_envelope, but for a whole vector of
## target areas at once instead of just one.

sample_from_envelope_vec <- function(envelope, n) {
  a <- envelope$a; b <- envelope$b; z <- envelope$z
  m <- length(a)
  R <- numeric(m)
  for (i in seq_len(m)) {
    lo <- z[i]; hi <- z[i + 1]
    if (is.infinite(hi)) {
      R[i] <- -exp(a[i] * lo + b[i]) / a[i]
    } else {
      R[i] <- (exp(a[i] * hi + b[i]) - exp(a[i] * lo + b[i])) / a[i]
    }
  }
  Q <- cumsum(R)
  d <- Q[m]

  target_area <- runif(n) * d
  i <- pmin(findInterval(target_area, Q) + 1, m)
  lo <- z[i]
  Q_full <- c(0, Q)      # Q_full[i] = area before piece i; avoids Q[i-1] breaking when i==1
  area_into_piece <- target_area - Q_full[i]
  rhs <- area_into_piece * a[i] + exp(a[i] * lo + b[i])
  (log(rhs) - b[i]) / a[i]
}

## ---- rng_vec comes from target_setup.R (shared with the Gaussian envelope) ----

## ---- One-shot fixed-envelope sampler: given M proposals, return the -------
## accepted subset (a random-length vector, <= M) -- same shape as
## log_rejection_sample_random from the Gaussian envelope.

sample_fixed_random <- function(N, envelope) {
  y <- sample_from_envelope_vec(envelope, N)
  accept <- log(runif(N)) <= log_f_vec_faster(y) - eval_V(envelope, y)
  y[accept]
}

sample_fixed_vec <- rng_vec(sample_fixed_random)

## ---- Measure total time for a given number of tangent points --------------

time_for_m <- function(m, N, min_iterations = 5) {
  y_points <- seq(0.02, y_hat + 3 * sd_hat, length.out = m)
  envelope <- build_envelope(y_points)
  b <- bench::mark(
    result <- sample_fixed_vec(N, envelope = envelope),
    min_iterations = min_iterations,
    check = FALSE
  )
  # acceptance rate, measured directly on a large batch (rng_vec itself
  # doesn't expose the number of attempts it took)
  y_check <- sample_from_envelope_vec(envelope, 20000)
  acc_rate <- mean(log(runif(20000)) <= log_f_vec_faster(y_check) - eval_V(envelope, y_check))
  data.frame(m = m, time = as.numeric(b$median), acceptance_rate = acc_rate)
}

## ---- Sweep over candidate m values and find the best -----------------------
## At N = 1000, total time is flat (and noise-dominated) for m roughly in
## [5, 30] -- picking argmin(time) directly is unstable across re-runs of the
## exact same sweep. It does rise again for very large m (the O(m) cost of
## building/sampling the envelope starts to show), so there is a genuine
## trade-off, just not a sharp one. Acceptance rate is the stable signal
## within the flat region, so: pick the smallest m that already reaches
## acc_threshold acceptance -- more points beyond that barely helps either
## metric, while very large m eventually costs more time for no real benefit.

find_best_m <- function(candidate_ms, N, min_iterations = 5, acc_threshold = 0.99) {
  results <- do.call(rbind, lapply(candidate_ms, time_for_m, N = N, min_iterations = min_iterations))
  above <- results[results$acceptance_rate >= acc_threshold, ]
  best <- if (nrow(above) > 0) above[which.min(above$m), ] else results[which.max(results$acceptance_rate), ]
  list(results = results, best = best)
}

out <- find_best_m(candidate_ms = c(2, 3, 5, 8, 12, 20, 30, 50, 60, 70), N = 1000, min_iterations = 30)
out$results   # table of time + acceptance rate for each m
out$best      # the single row with the lowest time

## ---- Correctness check: draw N samples for a given m, plot vs target ------
## Same idea as the Gaussian correctness check in Gaussian_envelope.R, but
## the majorizing function here is exp(V(y)) directly (alpha' = 1 exactly,
## by tangency), so the scaled envelope on the target's normalized scale
## is just exp(V(y)) / Z.

unnorm_f_vec <- function(y) exp(log_f_vec_faster(y))
Z <- integrate(unnorm_f_vec, lower = 0, upper = Inf)$value
target_density <- function(y) unnorm_f_vec(y) / Z

plot_log_affine_correctness <- function(m, N = 1000, seed = 1, ylim = c(0, 10)) {
  y_points <- seq(0.02, y_hat + 3 * sd_hat, length.out = m)
  envelope <- build_envelope(y_points)

  set.seed(seed)
  samples <- sample_fixed_vec(N, envelope = envelope)

  scaled_envelope <- function(y) exp(eval_V(envelope, y)) / Z
  x_max <- y_hat + 3 * sd_hat

  hist(samples, breaks = 30, freq = FALSE, xlim = c(0, x_max), ylim = ylim,
       main = paste0("m = ", m, ": samples vs target vs envelope"), xlab = "y")
  # explicit from/to: curve()'s default range is the plot's auto-expanded axis
  # (~4% past xlim on each side), which strays into y < 0 -- outside the
  # target's support and outside the envelope's domain
  curve(target_density, from = 0, to = x_max, add = TRUE, col = "red", lwd = 2)
  curve(scaled_envelope, from = 0, to = x_max, add = TRUE, col = "darkgreen", lwd = 2)
  legend("topright", legend = c("target density", "log-affine envelope"),
         col = c("red", "darkgreen"), lwd = 2, bty = "n", cex = 0.8)
}
