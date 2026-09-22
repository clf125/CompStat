source("Assignments/Assignment 2/Target_setup.R")

#Analytic mean and sd, from completing the square
A <- sum(dat$x * (dat$z - 1))
B <- sum(dat$x^2)
mu <- A / B
sigma <- sqrt(1 / B)
log_alpha_prime <-100-A^2/(2*B)-log(sigma*sqrt(2*pi))

#Target log-density, vectorized 
log_f_vec <- function(y) sapply(y, function(yi) sum(dat$z * dat$x * yi - exp(dat$x * yi)))

#Log acceptance ratio, using the log_f_vec
log_accept_gaussian <- function(y) {
log_f_vec(y)+log_alpha_prime-dnorm(y,mean=mu, sd=sigma, log=TRUE)
}

#Log acceptance ratio, using the log_f_vec_faster
log_accept_gaussian_faster <- function(y) {
  log_f_vec_faster(y) + log_alpha_prime - dnorm(y, mean = mu, sd = sigma, log = TRUE)
}

#Draw proposals from the Gaussian envelope
gaussian_rproposal <- function(n) {rnorm(n,mean=mu, sd=sigma)}

#Rejection-sampler vectorized
log_rejection_sample_random <- function(N, rproposal, log_accept) {
y <- rproposal(N)
u <- runif(N)
accept <- log(u) <= log_accept(y)
y[accept]
}

#Wrap into an exact-N sampler via rng_vec from chapter 6
log_rejection_sample_vec <- rng_vec(log_rejection_sample_random)


## ---- Correctness check: N = 1000 samples vs target and envelope -----------

set.seed(1)
samples <- log_rejection_sample_vec(N = 1000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian)

# normalized target density, via numerical integration of the unnormalized target
unnorm_f_vec <- function(y) exp(log_f_vec(y))
Z <- integrate(unnorm_f_vec, lower = 0, upper = Inf)$value
target_density <- function(y) unnorm_f_vec(y) / Z

# scaled envelope, on the same normalized scale (M = exp(-log_alpha_prime), since
# log_alpha_prime = -logM, i.e. alpha' = 1/M)
scaled_envelope <- function(y) exp(-log_alpha_prime) * dnorm(y, mean = mu, sd = sigma) / Z

peak_height <- max(scaled_envelope(seq(0, 1, length.out = 500)))

hist(samples, breaks = 30, freq = FALSE, ylim = c(0, peak_height * 1.05),
     main = "N = 1000: samples vs target vs envelope", xlab = "y")
curve(target_density, add = TRUE, col = "red", lwd = 2)
curve(scaled_envelope, add = TRUE, col = "darkgreen", lwd = 2)
legend("topright", legend = c("target density", "scaled envelope"),
       col = c("red", "darkgreen"), lwd = 2, bty = "n")

## ---- Acceptance rate --------------------------------------------------

# empirical: draw a large batch directly and check the raw accept/reject fraction
set.seed(1)
y_check <- gaussian_rproposal(200000)
u_check <- runif(200000)
accept_check <- log(u_check) <= log_accept_gaussian(y_check)
empirical_rate <- mean(accept_check)

# theoretical: acceptance rate = Z * alpha', where alpha' = exp(log_alpha_prime)
theoretical_rate <- Z * exp(log_alpha_prime)

cat("empirical acceptance rate: ", empirical_rate, "\n")
cat("theoretical acceptance rate:", theoretical_rate, "\n")
