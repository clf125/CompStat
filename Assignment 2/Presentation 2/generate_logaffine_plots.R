source("Assignment/Assignment 2/Benchmark_logaffine_vs_gaussian.R")

png("Assignment/Assignment 2/Presentation 2/bench_plot3.png", width = 1100, height = 700)
print(plot(logaffine_bench_N))
dev.off()

p4 <- dplyr::mutate(logaffine_bench_N, method = as.character(expression)) |>
  ggplot2::ggplot(ggplot2::aes(N, median, color = method)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(y = "time (s)")
ggplot2::ggsave("Assignment/Assignment 2/Presentation 2/bench_plot4.png", p4, width = 9, height = 6)

## ---- Cache the sweep result: timings are noisy and the m = 5..70 times ----
## are all close together, so re-running the sweep gives a different "best m"
## each time. Save it once so the slide text/plots all agree with each other.

saveRDS(out, "Assignment/Assignment 2/Presentation 2/best_m.rds")

## ---- Sweep results: time vs m and acceptance rate vs m, side by side ------
## Time alone is flat/noisy in the region that matters (see Log-affine_envelope.R);
## acceptance rate is the stable signal that actually drives the choice of m.

p5a <- ggplot2::ggplot(out$results, ggplot2::aes(m, time)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line() +
  ggplot2::geom_point(data = out$best, ggplot2::aes(m, time), color = "red", size = 4) +
  ggplot2::labs(x = "number of intervals (m)", y = "median time for N = 1000 (s)",
                title = "Time vs. m (flat/noisy)")

p5b <- ggplot2::ggplot(out$results, ggplot2::aes(m, acceptance_rate)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = 0.99, linetype = "dashed", color = "gray40") +
  ggplot2::geom_point(data = out$best, ggplot2::aes(m, acceptance_rate), color = "red", size = 4) +
  ggplot2::labs(x = "number of intervals (m)", y = "acceptance rate",
                title = "Acceptance rate vs. m (stable signal)")

png("Assignment/Assignment 2/Presentation 2/sweep_plot.png", width = 1600, height = 600, res = 130)
gridExtra::grid.arrange(p5a, p5b, ncol = 2)
dev.off()
