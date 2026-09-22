# Per-line profiling bar charts for the "Profiling" slides: self-time (%) per
# source line, before and after vectorizing log_f. Replaces the hand-typed
# percentage tables with a static plot built from Rprof's own numbers.
#
# options(keep.source = TRUE) has to be set globally (not just passed to the
# outer source() call below) because Gaussian_envelope.R itself sources
# Target_setup.R without keep.source = TRUE -- without the global option,
# line info for anything defined in Target_setup.R (incl. log_f_vec_faster)
# is lost, and self-time collapses onto the outer call site instead.
options(keep.source = TRUE)

src_dir <- "Assignments/Assignment 2"

grDevices::pdf(NULL)
source(file.path(src_dir, "Gaussian_envelope.R"), keep.source = TRUE)
grDevices::dev.off()

profile_self_pct <- function(expr) {
  tmp <- tempfile(fileext = ".out")
  Rprof(tmp, line.profiling = TRUE, interval = 0.005)
  force(expr)
  Rprof(NULL)
  by_line <- summaryRprof(tmp, lines = "show")$by.line
  unlink(tmp)

  by_line <- by_line[rownames(by_line) != "<no location>", , drop = FALSE]
  file <- sub("#[0-9]+$", "", rownames(by_line))
  line <- as.integer(sub("^.*#", "", rownames(by_line)))
  path <- file.path(src_dir, file)

  src_text <- mapply(function(f, l) {
    trimws(tryCatch(readLines(f)[l], error = function(e) NA_character_))
  }, path, line)

  data.frame(
    label = sprintf("%s:%d  %s", file, line,
                     ifelse(nchar(src_text) > 42, paste0(substr(src_text, 1, 39), "..."), src_text)),
    self_pct = by_line$self.pct,
    row.names = NULL
  )
}

plot_profile <- function(df, title, n = 6) {
  df <- head(df[order(-df$self_pct), ], n)
  df$label <- factor(df$label, levels = rev(df$label))

  ggplot2::ggplot(df, ggplot2::aes(x = label, y = self_pct)) +
    ggplot2::geom_col(fill = "#2a78d6", width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", self_pct)),
                        hjust = -0.15, color = "#0b0b0b", size = 3.6) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(limits = c(0, max(df$self_pct) * 1.18), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "% of total self time", title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#e1e0d9"),
      axis.text = ggplot2::element_text(color = "#0b0b0b", size = 9),
      axis.title.x = ggplot2::element_text(color = "#898781", size = 10),
      plot.title = ggplot2::element_text(color = "#0b0b0b", face = "bold", size = 13),
      plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5)
    )
}

set.seed(1)
before_df <- profile_self_pct(
  log_rejection_sample_vec(N = 200000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian)
)

set.seed(1)
after_df <- profile_self_pct(
  log_rejection_sample_vec(N = 200000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster)
)

p_before <- plot_profile(before_df, "Before: sapply-based log_f_vec")
p_after  <- plot_profile(after_df,  "After: vectorized log_f_vec_faster")

ggplot2::ggsave(file.path(src_dir, "Presentation 2/profiling_before.png"), p_before, width = 8, height = 4, dpi = 150)
ggplot2::ggsave(file.path(src_dir, "Presentation 2/profiling_after.png"), p_after, width = 8, height = 4, dpi = 150)
