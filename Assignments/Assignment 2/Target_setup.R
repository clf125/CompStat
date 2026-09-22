# Shared setup used by both Option A methods (Gaussian envelope and
# piecewise log-affine)

#Loading data
dat <- read.csv("Assignments/Assignment 2/poisson.csv")


#The target log-density
log_f <- function(y) sum(dat$z * dat$x * y - exp(dat$x * y))


#The target log-density in vectorized form
log_f_vec_faster <- function(y) {
  xy <- outer(dat$x, y)
  colSums(dat$z * xy - exp(xy))
}


#The general adaptive-batching wrapper: turns a one-shot sampler 
#into one that returns exactly N accepted samples
#Function is from chapter 6
rng_vec <- function(rng, fact = 1.2, M_min = 100) {
  force(rng); force(fact); force(M_min)  # lock in current values (avoids lazy-eval bugs in the closure)

  function(N, ..., cb) {
    j <- 0       # which batch we're on
    l <- 0       # accepted samples so far
    x <- list()  # x[[j]] = accepted observations from batch j

    while (l < N) {
      j <- j + 1
      M <- floor(max(fact * (N - l), M_min))  # proposals to draw this round: fact x (still needed), floored at M_min
      x[[j]] <- rng(M, ...)                   # draw M proposals; ... forwards extra args straight to rng
      l <- l + length(x[[j]])                 # update running total accepted

      if (!missing(cb)) cb()             # optional callback, e.g. for progress reporting
      if (j == 1) fact <- fact * N / l   # after batch 1, recalibrate fact to the observed acceptance rate (= M/l)
    }

    unlist(x)[seq_len(N)]  # flatten the batches into one vector, keep exactly N (drop any overshoot)
  }
}

