# Compare the two required Option A methods: the Gaussian envelope
# vs. the piecewise log-affine / adaptive rejection sampling envelope.

source("Assignment/Assignment 2/Gaussian_envelope.R")
source("Assignment/Assignment 2/Log-affine_envelope.R")

## ---- Build a good fixed log-affine envelope (m=30, near-optimal from the sweep) ----

y_points <- seq(0.02, y_hat + 3 * sd_hat, length.out = 30)
ars_envelope <- build_envelope(y_points)

## ---- Compare -----------------------------------------------------------------

bench::mark(
  gaussian = log_rejection_sample_vec(1000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster),
  log_affine = sample_fixed_vec(1000, envelope = ars_envelope),
  check = FALSE,
  min_iterations = 20
)

## ---- Benchmark across a range of N -----------------------------------------

methods_bench <- bench::press(
  N = c(100, 200, 500, 1000),
  bench::mark(
    gaussian   = log_rejection_sample_vec(N, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster),
    log_affine = sample_fixed_vec(N, envelope = ars_envelope),
    check = FALSE,
    min_iterations = 20
  )
)

plot(methods_bench)

## ---- Custom line plot: median time vs N, by method -------------------------

dplyr::mutate(methods_bench, method = as.character(expression)) |>
  ggplot2::ggplot(ggplot2::aes(N, median, color = method)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(y = "time (s)")

