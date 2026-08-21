#' Convert a time value to an HH:MM:SS character string
#'
#' Accepts POSIXt, difftime, or numeric (fractional day) values and returns
#' a character string in HH:MM:SS format. Falls back to \code{as.character()}
#' for any other type.
#'
#' @param x A POSIXt, difftime, numeric (fractional day), or other object.
#' @return A character string formatted as HH:MM:SS.
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

#' Read and harmonise chamber measurement metadata from Google Sheets
#'
#' Reads the LightDark and After-Watering measurement sheets from the project
#' Google Sheet, converts raw time columns to POSIXct datetimes
#' (America/Los_Angeles), pivots the LightDark sheet from wide to long so that
#' Light and CO2_Fixation measurements are stacked as a \code{type} column, assigns
#' a per-PotID \code{Order_Index} to each measurement occasion, and returns both
#' tables in a named list ready for \code{bind_rows()}.
#'
#' @param metadata_url Character. URL of the Google Sheet containing the
#'   \code{LightDark_Measurment} sheet.
#' @param afterwatering_url Character. URL of the Google Sheet containing the
#'   \code{After_WateringMeasurment} sheet.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{lightdark_metadata}{Long-format data frame with columns
#'       \code{PotID}, \code{Date}, \code{Chamber_Start_DateTime},
#'       \code{Chamber_End_DateTime}, \code{Order_Index}, and \code{type}
#'       (\code{"Light"} or \code{"CO2_Fixation"}).}
#'     \item{afterwatering_metadata}{Data frame with the same key columns plus
#'       \code{type = "After_Watering"}.}
#'   }
read_metadata <- function(metadata_url, afterwatering_url) {
  # --- LightDark sheet --------------------------------------------------
  # The sheet stores Light and CO2_Fixation windows as paired wide columns;
  # we convert the raw hh:mm:ss time strings to POSIXct before pivoting.
  # .keep = "unused" drops the original raw time columns after mutate.
  lightdark_metadata = read_sheet(metadata_url, sheet = "LightDark_Measurment") %>%
    # googlesheets4 sometimes appends "...N" sentinel columns — remove them
    select(-contains("...")) %>%
    mutate(
      Date = as.Date(Date),
      Chamber_Start_DateTime_Light = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_Start_Time_Light_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_End_DateTime_Light = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_End_Time_Light_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_Start_DateTime_CO2_Fixation = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_Start_Time_CO2_Fixation_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_End_DateTime_CO2_Fixation = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_End_Time_CO2_Fixation_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      .keep = "unused"
    ) %>%
    group_by(PotID) %>%
    # Order_Index ranks measurement occasions chronologically within each pot
    mutate(
      Light_Order_Index = row_number(Chamber_Start_DateTime_Light),
      CO2_Fixation_Order_Index = row_number(Chamber_Start_DateTime_CO2_Fixation)
    ) %>%
    ungroup()

  # Pivot wide → long so Light and CO2_Fixation rows are stacked under a
  # single `type` column. The regex pattern splits column names like
  # "Chamber_Start_DateTime_Light" into value-key ("Start") and type ("Light"),
  # producing unified Chamber_Start_DateTime / Chamber_End_DateTime columns.
  lightdark_metadata_long = lightdark_metadata %>%
    rename(
      Order_Index_Light = Light_Order_Index,
      Order_Index_CO2_Fixation = CO2_Fixation_Order_Index
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        Chamber_Start_DateTime_Light,
        Chamber_End_DateTime_Light,
        Chamber_Start_DateTime_CO2_Fixation,
        Chamber_End_DateTime_CO2_Fixation,
        Order_Index_Light,
        Order_Index_CO2_Fixation
      ),
      names_to = c(".value", "type"),
      names_pattern = "^(?:Chamber_)?(Start|End|Order_Index)(?:_DateTime)?_(Light|CO2_Fixation)$"
    ) %>%
    rename(
      Chamber_Start_DateTime = Start,
      Chamber_End_DateTime = End
    ) %>%
    relocate(type, .after = last_col())

  # --- After-Watering sheet ---------------------------------------------
  # Already in long format (one measurement type per row), so only datetime
  # conversion and Order_Index assignment are needed.
  afterwatering_metadata = read_sheet(afterwatering_url, sheet = "After_WateringMeasurment") %>%
    select(-contains("...")) %>%
    mutate(
      Date = as.Date(Date),
      Chamber_Start_DateTime = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_Start_Time_After_Watering_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_End_DateTime = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_End_Time_After_Watering_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      type = "After_Watering",
      .keep = "unused"
    ) %>%
    group_by(PotID) %>%
    mutate(Order_Index = row_number(Chamber_Start_DateTime)) %>%
    ungroup()

  list(
    lightdark_metadata = lightdark_metadata_long,
    afterwatering_metadata = afterwatering_metadata
  )
}
