# Real profvis screenshots for the "Profiling" slides (before/after
# vectorizing log_f), matching the look used in the lecture slides:
# render the actual profvis widget to a self-contained HTML file, screenshot
# it headlessly via Chrome (webshot2/chromote), then crop to the informative
# regions.
#
# profvis captures its expression via substitute() at the call site, so it
# must be called directly with a literal code block -- passing it through a
# wrapper function breaks the srcref it needs for line highlighting.
#
# profvis also never renders a separate code panel for Target_setup.R (the
# file Gaussian_envelope.R sources log_f_vec_faster from) even though the
# flame graph correctly attributes time to it -- confirmed by capturing the
# full page (vheight = 2600) and finding it ends right after
# Gaussian_envelope.R's own last line, with no second file panel at all. So
# "after" is cropped to just the profiled expression + flame graph, while
# "before" (whose hot line lives directly in Gaussian_envelope.R) keeps the
# highlighted source line too.

options(keep.source = TRUE)

src_dir <- "Assignments/Assignment 2"
out_dir <- file.path(src_dir, "Presentation 2")

grDevices::pdf(NULL)
source(file.path(src_dir, "Gaussian_envelope.R"), keep.source = TRUE)
grDevices::dev.off()

save_shoot_crop <- function(p, html_path, png_path, top_height, flame_top = 2200, flame_height = 400,
                             page_height = 2600, width = 1400) {
  htmlwidgets::saveWidget(p, html_path, selfcontained = FALSE)
  webshot2::webshot(html_path, png_path, vwidth = width, vheight = page_height, delay = 2)
  unlink(html_path)
  unlink(sub("\\.html$", "_files", html_path), recursive = TRUE)

  img <- magick::image_read(png_path)
  top_crop <- magick::image_crop(img, magick::geometry_area(width, top_height, 0, 0))
  flame_crop <- magick::image_crop(img, magick::geometry_area(width, flame_height, 0, flame_top))
  magick::image_write(magick::image_append(c(top_crop, flame_crop), stack = TRUE), png_path)
}

set.seed(1)
p_before <- profvis::profvis({
  log_rejection_sample_vec(N = 200000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian)
}, interval = 0.005)
save_shoot_crop(p_before, file.path(out_dir, "_profvis_before.html"), file.path(out_dir, "profiling_before.png"),
                 top_height = 460)

set.seed(1)
p_after <- profvis::profvis({
  log_rejection_sample_vec(N = 200000, rproposal = gaussian_rproposal, log_accept = log_accept_gaussian_faster)
}, interval = 0.005)
save_shoot_crop(p_after, file.path(out_dir, "_profvis_after.html"), file.path(out_dir, "profiling_after.png"),
                 top_height = 85)
