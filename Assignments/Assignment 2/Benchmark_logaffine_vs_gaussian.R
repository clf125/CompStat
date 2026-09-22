# Benchmark the piecewise log-affine envelope (m = 5, and the best m found
# by the sweep in Log-affine_envelope.R) against the two fastest Gaussian
# envelope implementations: the vectorized R sampler and the specialized C++
# sampler.

source("Assignments/Assignment 2/Log-affine_envelope.R")
source("Assignments/Assignment 2/Gaussian_envelope.R")
Rcpp::sourceCpp("Assignments/Assignment 2/CPP_Gaussian_rejection_specialized.cpp")

## ---- Fixed log-affine envelopes: m = 5 and m = best (from the sweep) ------

best_m <- out$best$m
envelope_m5    <- build_envelope(seq(0.02, y_hat + 3 * sd_hat, length.out = 5))
envelope_mbest <- build_envelope(seq(0.02, y_hat + 3 * sd_hat, length.out = best_m))

## ---- Benchmark across a range of N -----------------------------------------
## bench_label_mbest is built from best_m so the label always matches whatever
## m the sweep actually picked, instead of hardcoding "20" into the name.
## bench::mark's ... doesn't support tidy-eval injection (no `!!name :=`), so
## the placeholder name is swapped in afterwards via the "description"
## attribute on the expression column -- that's what plot()/as.character()
## actually read as the label.

bench_label_mbest <- paste0("log_affine_m", best_m, "_best")

logaffine_bench_N <- bench::press(
  N = c(100, 500, 1000, 2000),
  bench::mark(
    log_affine_m5                  = sample_fixed_vec(N, envelope = envelope_m5),
    log_affine_mbest_placeholder   = sample_fixed_vec(N, envelope = envelope_mbest),
    cpp_specialized_gaussian       = gaussian_rejection_cpp(N, dat$z, dat$x, mu, sigma, log_alpha_prime),
    r_vectorized_faster_gaussian   = log_rejection_sample_vec(N, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster),
    check = FALSE,
    min_iterations = 20
  )
)

desc <- attr(logaffine_bench_N$expression, "description")
desc[desc == "log_affine_mbest_placeholder"] <- bench_label_mbest
attr(logaffine_bench_N$expression, "description") <- desc

plot(logaffine_bench_N)

## ---- Custom line plot: median time vs N, by method -------------------------

dplyr::mutate(logaffine_bench_N, method = as.character(expression)) |>
  ggplot2::ggplot(ggplot2::aes(N, median, color = method)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(y = "time (s)")
