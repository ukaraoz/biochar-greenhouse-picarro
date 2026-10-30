#' Read the pipeline configuration
#'
#' Reads \code{config.yml} (source data locations) and checks that every data
#' source the pipeline uses is set.
#'
#' @param path Character. Path to the YAML config file.
#' @return A named list; data source links are in \code{$data_sources}.
read_config <- function(path) {
  config <- yaml::read_yaml(path)
  required <- c("metadata_sheet", "afterwatering_sheet", "endpoint_sheet", "plant_measurements_sheet", "greenhouse_log")
  missing <- setdiff(required, names(Filter(\(x) is.character(x) && nzchar(x), config$data_sources)))
  if (length(missing) > 0) {
    stop("config file ", path, " is missing data_sources: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  config
}
