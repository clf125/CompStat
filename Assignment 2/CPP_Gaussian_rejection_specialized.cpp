#include <Rcpp.h>
using namespace Rcpp;

// Specialized rejection sampler: the target's log-likelihood sum and the
// Gaussian envelope are hardcoded directly in C++ (like von_mises_cpp
// hardcodes kappa*(cos(x)-1)), with no callbacks into R at all.

// [[Rcpp::export]]
NumericVector
gaussian_rejection_cpp(int N, NumericVector z, NumericVector x,
                        double mu, double sigma, double log_alpha_prime)
{
  int n = z.size();
  NumericVector out(N);
  double y, log_fy, log_gy;
  bool reject;

  for (int i = 0; i < N; ++i) {
    do {
      y = R::rnorm(mu, sigma);

      log_fy = 0.0;
      for (int k = 0; k < n; ++k) {
        log_fy += z[k] * x[k] * y - exp(x[k] * y);
      }
      log_gy = R::dnorm(y, mu, sigma, true);

      reject = log(R::runif(0, 1)) > (log_fy + log_alpha_prime - log_gy);
    } while (reject);
    out[i] = y;
  }

  return out;
}
