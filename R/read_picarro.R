as_hms_text <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%H:%M:%S"))
  }

  if (inherits(x, "difftime")) {
    seconds <- as.numeric(x, units = "secs")
    return(format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S"))
  }

  if (is.numeric(x)) {
    seconds <- x * 24 * 60 * 60
    return(format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S"))
  }

  as.character(x)
}

read_picarro_dat <- function(file, tz = "UTC", add_timestamp = TRUE,
                             drop_incomplete = TRUE) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("`file` must be a single file path.", call. = FALSE)
  }

  if (!file.exists(file)) {
    stop("File does not exist: ", file, call. = FALSE)
  }

  lines <- readLines(file, warn = FALSE)
  # handles invalid UTF-8 strings
  lines <- iconv(lines, to = "ASCII", sub = "")
  lines <- lines[nzchar(trimws(lines))]

  if (length(lines) == 0L) {
    stop("File is empty: ", file, call. = FALSE)
  }

  con <- textConnection(lines)
  on.exit(close(con), add = TRUE)
  field_counts <- utils::count.fields(
    con,
    sep = "",
    quote = "",
    blank.lines.skip = FALSE,
    comment.char = ""
  )

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

    data$timestamp <- as.POSIXct(
      as.numeric(data$EPOCH_TIME),
      origin = "1970-01-01",
      tz = tz
    )

    data <- data[c("timestamp", setdiff(names(data), "timestamp"))]
  }

  attr(data, "dropped_lines") <- incomplete_lines
  data
}
