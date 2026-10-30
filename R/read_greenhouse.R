#' Read the greenhouse environment log
#'
#' Downloads the logger CSV from Google Drive (a plain CSV, not a Sheet, so
#' googledrive rather than googlesheets4; interactive OAuth on first run,
#' cached afterwards) and parses \code{DateTime} into \code{time}.
#'
#' @param url Character. Drive link or file ID of the CSV
#'   (\code{config$data_sources$greenhouse_log}).
#' @param tz Character. Time zone of the logger clock.
#' @return Data frame with the CSV columns plus \code{time} (POSIXct).
read_greenhouse_log <- function(url, tz = "America/Los_Angeles") {
  googledrive::drive_auth()
  file <- tempfile(fileext = ".csv")
  googledrive::drive_download(googledrive::as_id(url), path = file, overwrite = TRUE)
  log <- read.csv(file, check.names = FALSE)
  log$time <- as.POSIXct(log$DateTime, format = "%a %b %d %H:%M:%S %Y", tz = tz)
  log
}

#' Timestamps of the greenhouse log, by which sensors were recorded
#'
#' Rows with blank relative humidity and PAR are temperature-only readings.
#'
#' @param log Output of \code{read_greenhouse_log()}.
#' @return Data frame of \code{time} and \code{Logged} (factor: "Temperature,
#'   RH and PAR" or "Temperature only").
greenhouse_log_times <- function(log) {
  temperature_only <- is.na(log$`Indoor Relative Humidity (%)`) & is.na(log$`Indoor PAR Light (PAR)`)
  out <- data.frame(time = log$time,
                    Logged = factor(ifelse(temperature_only, "Temperature only", "Temperature, RH and PAR"),
                                    levels = c("Temperature, RH and PAR", "Temperature only")))
  out[!is.na(out$time), ]
}
