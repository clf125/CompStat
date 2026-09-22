source("Assignments/Assignment 2/Benchmark_Gaussian_sampler.R")

png("Assignments/Assignment 2/Presentation 2/bench_plot1.png", width = 900, height = 600)
print(plot(samplers_bench))
dev.off()

p2 <- dplyr::mutate(samplers_bench, method = as.character(expression)) |>
  ggplot2::ggplot(ggplot2::aes(N, median, color = method)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(y = "time (s)")
ggplot2::ggsave("Assignments/Assignment 2/Presentation 2/bench_plot2.png", p2, width = 9, height = 6)
