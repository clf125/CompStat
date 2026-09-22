#include <Rcpp.h>
using namespace Rcpp;

// General rejection sampler, inspired by von_mises_cpp but taking the
// proposal generator and log-acceptance-ratio as R functions, so it works
// for any target/envelope pair instead of one hardcoded distribution.

// [[Rcpp::export]]
NumericVector
rejection_sample_cpp(int N, Function rproposal, Function log_accept)
{
  NumericVector out(N);
  double y;
  bool reject;

  for (int i = 0; i < N; ++i) {
    do {
      y = as<double>(rproposal(1));
      reject = log(R::runif(0, 1)) > as<double>(log_accept(y));
    } while (reject);
    out[i] = y;
  }

  return out;
}
