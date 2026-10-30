#' Save a figure as PNG and, optionally, PDF
#'
#' Runs \code{draw} on a PNG device and, when \code{pdf} is \code{TRUE}, also
#' on a PDF device. The PDF is on by default; turn it off for every figure with
#' \code{options(save_figure.pdf = FALSE)} (set in \code{run_all.R}). A single-page figure gives
#' \code{<name>.png}; a multi-page figure gives one PNG per page,
#' \code{<name>_p01.png}, \code{<name>_p02.png}, ... Old PNGs for the same
#' figure are removed first so page counts never mix between runs.
#'
#' @param pdf_path Character. Output path ending in \code{.pdf}; the PNGs are
#'   written next to it with the same name.
#' @param draw Function with no arguments that draws all pages (e.g.
#'   \code{function() print(p)}, a loop of \code{print()} calls, or base
#'   graphics).
#' @param width,height Numeric. Page size in inches.
#' @param dpi Numeric. PNG resolution (default 200).
#' @param pdf_device Function. PDF device to use (default \code{grDevices::pdf};
#'   pass \code{grDevices::cairo_pdf} when the figure needs Unicode glyphs).
#' @param pdf Logical. Write the PDF (default \code{getOption("save_figure.pdf", TRUE)}).
#'
#' @return Invisibly, the paths of the PNG files written.
save_figure <- function(pdf_path, draw, width, height, dpi = 200,
                        pdf_device = grDevices::pdf,
                        pdf = getOption("save_figure.pdf", TRUE)) {
  if (pdf) {
    pdf_device(pdf_path, width = width, height = height)
    tryCatch(draw(), finally = grDevices::dev.off())
  }

  stem <- sub("\\.pdf$", "", pdf_path)
  name <- basename(stem)
  is_page_png <- function(files) {
    startsWith(basename(files), name) &
      grepl("^(_p[0-9]+)?\\.png$", substring(basename(files), nchar(name) + 1))
  }
  existing <- list.files(dirname(stem), pattern = "\\.png$", full.names = TRUE)
  unlink(existing[is_page_png(existing)])

  # render pages to a scratch folder, then name them by page count
  scratch <- tempfile("png_pages_")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  png_type <- if (capabilities("cairo")) "cairo" else NULL
  grDevices::png(file.path(scratch, "page_%03d.png"), width = width, height = height,
                 units = "in", res = dpi, type = png_type)
  tryCatch(draw(), finally = grDevices::dev.off())

  pages <- sort(list.files(scratch, pattern = "\\.png$", full.names = TRUE))
  dest <- if (length(pages) == 1) {
    paste0(stem, ".png")
  } else {
    sprintf("%s_p%02d.png", stem, seq_along(pages))
  }
  file.copy(pages, dest, overwrite = TRUE)
  invisible(dest)
}
