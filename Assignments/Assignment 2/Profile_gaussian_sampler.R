#Profiling the Gaussian sampler, before and after vectorizing log_f
source("Assignments/Assignment 2/Gaussian_envelope.R", keep.source = TRUE)

#Profile the original sampler: reveals the sapply bottleneck in log_f_vec (~98% of runtime)
profvis::profvis({
  log_rejection_sample_vec(N = 200000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian)
})

#Profile the vectorized sampler: confirms that bottleneck is gone after switching to log_f_vec_faster
profvis::profvis({
  log_rejection_sample_vec(N = 200000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster)
})
