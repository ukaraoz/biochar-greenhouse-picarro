#' Read a Picarro .dat file into a data frame
#'
#' Reads a single whitespace-delimited Picarro instrument output file,
#' handling invalid UTF-8 characters and optionally dropping incomplete rows.
#' An optional POSIXct timestamp column is derived from the EPOCH_TIME column.
#'
#' @param file Character. Path to a single .dat file.
#' @param tz Character. Time zone for the timestamp column (default: "UTC").
#' @param add_timestamp Logical. If TRUE, adds a \code{timestamp} column derived
#'   from \code{EPOCH_TIME} and moves it to the first column position (default: TRUE).
#' @param drop_incomplete Logical. If TRUE, silently drops rows whose field count
#'   differs from the header; if FALSE, stops with an error listing those rows
#'   (default: TRUE).
#'
#' @return A data frame with all columns from the .dat file, plus a leading
#'   \code{timestamp} column when \code{add_timestamp = TRUE}. The attribute
#'   \code{dropped_lines} records the (1-indexed) line numbers that were removed.
read_picarro_dat <- function(file, tz = "UTC", add_timestamp = TRUE,
                             drop_incomplete = TRUE) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("`file` must be a single file path.", call. = FALSE)
  }

  if (!file.exists(file)) {
    stop("File does not exist: ", file, call. = FALSE)
  }

  lines <- readLines(file, warn = FALSE)
  # Picarro output can contain invalid UTF-8 bytes; strip them before parsing
  lines <- iconv(lines, to = "ASCII", sub = "")
  # drop blank / whitespace-only lines that would confuse field counting
  lines <- lines[nzchar(trimws(lines))]

  if (length(lines) == 0L) {
    stop("File is empty: ", file, call. = FALSE)
  }

  # count fields per line to detect truncated rows (e.g. from instrument crash)
  con <- textConnection(lines)
  on.exit(close(con), add = TRUE)
  field_counts <- utils::count.fields(
    con,
    sep = "",
    quote = "",
    blank.lines.skip = FALSE,
    comment.char = ""
  )

  # header defines the expected field count; data rows start at index 2
  expected_fields <- field_counts[1L]
  incomplete_lines <- which(field_counts[-1L] != expected_fields) + 1L

  if (length(incomplete_lines) > 0L) {
    if (!drop_incomplete) {
      stop(
        "Found incomplete data rows at file line(s): ",
        paste(incomplete_lines, collapse = ", "),
        call. = FALSE
      )
    }
    lines <- lines[-incomplete_lines]
  }

  # sep = "" treats any run of whitespace as a delimiter (Picarro's format)
  data <- utils::read.table(
    text = lines,
    header = TRUE,
    sep = "",
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    comment.char = "",
    na.strings = c("NA", "NaN", "nan", "")
  )

  if (add_timestamp) {
    if (!"EPOCH_TIME" %in% names(data)) {
      stop("Expected an `EPOCH_TIME` column in: ", file, call. = FALSE)
    }

    # EPOCH_TIME is Unix time in seconds; coerce via numeric to avoid integer overflow
    data$timestamp <- as.POSIXct(
      as.numeric(data$EPOCH_TIME),
      origin = "1970-01-01",
      tz = tz
    )

    # move timestamp to the first column for readability
    data <- data[c("timestamp", setdiff(names(data), "timestamp"))]
  }

  # store dropped line numbers so callers can audit what was removed
  attr(data, "dropped_lines") <- incomplete_lines
  data
}
