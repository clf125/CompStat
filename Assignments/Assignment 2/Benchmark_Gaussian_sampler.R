source("Assignment/Assignment 2/Gaussian_envelope.R")
Rcpp::sourceCpp("Assignment/Assignment 2/CPP_Gaussian_rejection_general.cpp")
Rcpp::sourceCpp("Assignment/Assignment 2/CPP_Gaussian_rejection_specialized.cpp")

## ---- Single benchmark, N = 1000 -------------------------------------------

bench::mark(
  r_sapply_slower           = log_rejection_sample_vec(1000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian),
  r_vectorized_faster       = log_rejection_sample_vec(1000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster),
  cpp_general               = rejection_sample_cpp(1000, gaussian_rproposal, log_accept_gaussian),
  cpp_specialized_gaussian  = gaussian_rejection_cpp(1000, dat$z, dat$x, mu, sigma, log_alpha_prime),
  check = FALSE,
  relative = TRUE
)

## ---- Benchmark across a range of N -----------------------------------------

samplers_bench <- bench::press(
  N = c(100, 200, 500, 1000),
  bench::mark(
    r_sapply_slower           = log_rejection_sample_vec(N, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian),
    r_vectorized_faster       = log_rejection_sample_vec(N, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster),
    cpp_general               = rejection_sample_cpp(N, gaussian_rproposal, log_accept_gaussian),
    cpp_specialized_gaussian  = gaussian_rejection_cpp(N, dat$z, dat$x, mu, sigma, log_alpha_prime),
    check = FALSE,
    min_iterations = 20
  )
)

plot(samplers_bench)

## ---- Custom line plot: median time vs N, by method -------------------------

dplyr::mutate(samplers_bench, method = as.character(expression)) |>
  ggplot2::ggplot(ggplot2::aes(N, median, color = method)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(y = "time (s)")
