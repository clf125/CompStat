# Real profvis screenshots for the "Profiling" slides (before/after
# vectorizing log_f), matching the look used in the lecture slides:
# render the actual profvis widget to a self-contained HTML file, screenshot
# it headlessly via Chrome (webshot2/chromote), then crop to the informative
# regions and stack them.
#
# profvis captures its expression via substitute() at the call site, so it
# must be called directly with a literal code block -- passing it through a
# wrapper function breaks the srcref it needs for line highlighting.
#
# profvis DOES render a separate code panel for Target_setup.R (the file
# Gaussian_envelope.R sources log_f_vec_faster and rng_vec from), with
# colSums correctly highlighted for the vectorized run -- an earlier version
# of this script missed it because the screenshot delay (2s) was too short
# for the widget to finish expanding all panels before capture, not because
# of any real profvis limitation. Using vheight = 6000 and delay = 3 fixes
# it; the panel sits far enough down that a shorter viewport cuts it off.

options(keep.source = TRUE)

src_dir <- "Assignments/Assignment 2"
out_dir <- file.path(src_dir, "Presentation 2")

grDevices::pdf(NULL)
source(file.path(src_dir, "Gaussian_envelope.R"), keep.source = TRUE)
grDevices::dev.off()

save_shoot_crop <- function(p, html_path, png_path, windows, page_height = 6000, width = 1400) {
  htmlwidgets::saveWidget(p, html_path, selfcontained = FALSE)
  webshot2::webshot(html_path, png_path, vwidth = width, vheight = page_height, delay = 3)
  unlink(html_path)
  unlink(sub("\\.html$", "_files", html_path), recursive = TRUE)

  img <- magick::image_read(png_path)
  crops <- lapply(windows, function(w) magick::image_crop(img, magick::geometry_area(width, w[2], 0, w[1])))
  magick::image_write(magick::image_append(do.call(c, crops), stack = TRUE), png_path)
}

# each window is c(y_offset, height), found by inspecting a full uncropped
# vheight = 6000 capture (see conversation/history for how these were found)
before_windows <- list(
  c(0, 460),     # <expr> panel + Gaussian_envelope.R through the highlighted sapply line
  c(5700, 300)   # flame graph
)
after_windows <- list(
  c(0, 85),      # <expr> panel only
  c(1320, 290),  # Target_setup.R panel, header through the highlighted colSums line
  c(5700, 300)   # flame graph
)

set.seed(1)
p_before <- profvis::profvis({
  log_rejection_sample_vec(N = 200000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian)
}, interval = 0.005)
save_shoot_crop(p_before, file.path(out_dir, "_profvis_before.html"), file.path(out_dir, "profiling_before.png"),
                 before_windows)

set.seed(1)
p_after <- profvis::profvis({
  log_rejection_sample_vec(N = 200000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster)
}, interval = 0.005)
save_shoot_crop(p_after, file.path(out_dir, "_profvis_after.html"), file.path(out_dir, "profiling_after.png"),
                 after_windows)
