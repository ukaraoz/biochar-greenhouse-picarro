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
#' Light and Dark measurements are stacked as a \code{Condition} column, assigns
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
#'       \code{Chamber_End_DateTime}, \code{Order_Index}, and \code{Condition}
#'       (\code{"Light"} or \code{"Dark"}).}
#'     \item{afterwatering_metadata}{Data frame with the same key columns plus
#'       \code{Condition = "After_Watering"}.}
#'   }
read_metadata <- function(metadata_url, afterwatering_url) {
  # --- LightDark sheet --------------------------------------------------
  # The sheet stores Light and Dark windows as paired wide columns.
  # We convert the raw hh:mm:ss time strings to POSIXct before pivoting.
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
      Chamber_Start_DateTime_Dark = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_Start_Time_Dark_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_End_DateTime_Dark = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_End_Time_Dark_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      .keep = "unused"
    ) %>%
    group_by(PotID) %>%
    # Order_Index ranks measurement occasions chronologically within each pot
    mutate(
      Light_Order_Index = row_number(Chamber_Start_DateTime_Light),
      Dark_Order_Index = row_number(Chamber_Start_DateTime_Dark)
    ) %>%
    ungroup()

  # Pivot wide → long so Light and Dark rows are stacked under a
  # single `Condition` column. The regex pattern splits column names like
  # "Chamber_Start_DateTime_Light" into value-key ("Start") and condition ("Light"),
  # producing unified Chamber_Start_DateTime / Chamber_End_DateTime columns.
  lightdark_metadata_long = lightdark_metadata %>%
    rename(
      Order_Index_Light = Light_Order_Index,
      Order_Index_Dark = Dark_Order_Index
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        Chamber_Start_DateTime_Light,
        Chamber_End_DateTime_Light,
        Chamber_Start_DateTime_Dark,
        Chamber_End_DateTime_Dark,
        Order_Index_Light,
        Order_Index_Dark
      ),
      names_to = c(".value", "Condition"),
      names_pattern = "^(?:Chamber_)?(Start|End|Order_Index)(?:_DateTime)?_(Light|Dark)$"
    ) %>%
    rename(
      Chamber_Start_DateTime = Start,
      Chamber_End_DateTime = End
    ) %>%
    relocate(Condition, .after = last_col())

  # --- After-Watering sheet ---------------------------------------------
  # Already in long format (one measurement condition per row), so only datetime
  # conversion and Order_Index assignment are needed. Each row is also one
  # watering of the pot, with the water added in Water_g_60WHC.
  # Only columns A:N are read for now. The tab has a second, hidden block in
  # columns Q:U that repeats Water_g_60WHC, Soil_Temperature_C_7cm, surface
  # temperature and EC (typed-in copies, identical to H:K as of 2026-10-30, with
  # the "_13cm" suffix on the other column) and adds Soil_Moisture_percent.
  # Reading A:N keeps one copy of each column under its own name.
  afterwatering_metadata = read_sheet(afterwatering_url, sheet = "After_WateringMeasurment", range = "A:N") %>%
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
      Condition = "After_Watering",
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

#' Total water added to each pot over the experiment
#'
#' Sums the water added at each watering (\code{Water_g_60WHC}, g added to
#' bring the pot to 60% of water-holding capacity; one After-Watering sheet row
#' per watering) to the end-point total per pot.
#'
#' @param afterwatering_metadata The \code{afterwatering_metadata} element of
#'   \code{read_metadata()}.
#'
#' @return Data frame with one row per pot: \code{PotID}, \code{Soil},
#'   \code{Treatment}, \code{Replicate}, \code{Block}, \code{N_Waterings},
#'   \code{First_Watering}, \code{Last_Watering} (dates) and
#'   \code{Water_Total_g}.
total_water_added <- function(afterwatering_metadata) {
  n_blank <- sum(is.na(afterwatering_metadata$Water_g_60WHC))
  if (n_blank > 0) {
    warning(n_blank, " blank Water_g_60WHC cell(s) in After_WateringMeasurment, counted as no water",
            call. = FALSE)
  }

  afterwatering_metadata %>%
    group_by(PotID, Soil, Treatment, Replicate, Block) %>%
    summarise(
      N_Waterings = sum(!is.na(Water_g_60WHC)),
      First_Watering = min(Date),
      Last_Watering = max(Date),
      Water_Total_g = sum(Water_g_60WHC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Block, PotID)
}

#' Water use efficiency of each pot
#'
#' Water use efficiency (mg shoot dry weight per g water added) = end-point
#' shoot dry weight (\code{Shoot dry weigh_mg} in the end-point sheet) divided
#' by the total water added (\code{total_water_added()}). The end-point sheet
#' has one row per pot and soil fraction (\code{Soil_Type} Bulk and
#' Rhizosphere) with the same shoot weight in both, plus T0 and biochar
#' reference rows without plants; only the pots in \code{water_per_pot} are
#' kept. The function stops if a pot has no shoot weight or two different ones.
#'
#' @param endpoint_url Character. URL of the Google Sheet containing the
#'   \code{pH_EC_EndPoint_July2026} sheet.
#' @param water_per_pot Output of \code{total_water_added()}.
#'
#' @return \code{water_per_pot} with \code{Shoot_Dry_Weight_mg} and
#'   \code{WUE_mg_per_g} added.
water_use_efficiency <- function(endpoint_url, water_per_pot) {
  shoots <- read_sheet(endpoint_url, sheet = "pH_EC_EndPoint_July2026") %>%
    transmute(PotID = trimws(`Sample ID`), Shoot_Dry_Weight_mg = `Shoot dry weigh_mg`) %>%
    filter(PotID %in% water_per_pot$PotID) %>%
    distinct()

  conflicting <- shoots$PotID[duplicated(shoots$PotID)]
  if (length(conflicting) > 0) {
    stop("pot(s) with different shoot dry weights in the end-point sheet: ",
         paste(unique(conflicting), collapse = ", "), call. = FALSE)
  }
  missing <- setdiff(water_per_pot$PotID, shoots$PotID[!is.na(shoots$Shoot_Dry_Weight_mg)])
  if (length(missing) > 0) {
    stop("pot(s) without a shoot dry weight in the end-point sheet: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  water_per_pot %>%
    left_join(shoots, by = "PotID") %>%
    mutate(WUE_mg_per_g = Shoot_Dry_Weight_mg / Water_Total_g)
}

#' Read the plant measurements over time
#'
#' Reads the \code{Raw_Plant_Measurements_Shoot_Chlorophyll} sheet (one row per
#' pot and date) and averages the raw readings per pot and date: shoot count
#' and shoot height over the 7 shoots (\code{_R1}-\code{_R7}) and chlorophyll
#' index over \code{Leaf_1}-\code{Leaf_5}. \code{Leaf_6} is ignored: on
#' 2026-04-09 it holds the shoot count average instead of a leaf reading, which
#' also makes the sheet's \code{Chlorophyll_index_Average} wrong for that date.
#'
#' @param plant_url Character. URL of the Google Sheet containing the
#'   \code{Raw_Plant_Measurements_Shoot_Chlorophyll} sheet.
#'
#' @return Data frame with one row per pot and date: \code{PotID},
#'   \code{Block}, \code{Date}, \code{Shoot_Count}, \code{Shoot_Height_cm} and
#'   \code{Chlorophyll_Index}.
read_plant_measurements <- function(plant_url) {
  sheet <- read_sheet(plant_url, sheet = "Raw_Plant_Measurements_Shoot_Chlorophyll")
  row_mean <- function(columns) rowMeans(as.matrix(sheet[columns]), na.rm = TRUE)
  sheet %>%
    transmute(
      PotID = trimws(PotID),
      Block,
      # dates are typed as yyyymmdd numbers
      Date = as.Date(as.character(Date), format = "%Y%m%d"),
      Shoot_Count = row_mean(paste0("Shoots_Count_R", 1:7)),
      Shoot_Height_cm = row_mean(paste0("Shoots_Height_cm_R", 1:7)),
      Chlorophyll_Index = row_mean(paste0("Leaf_", 1:5))
    )
}

#' Plant measurements averaged over dates
#'
#' @param plant_measurements Output of \code{read_plant_measurements()}.
#' @return Data frame with one row per pot: \code{PotID}, \code{Block},
#'   \code{N_Dates} and the mean over dates of \code{Shoot_Count},
#'   \code{Shoot_Height_cm} and \code{Chlorophyll_Index}.
plant_measurements_per_pot <- function(plant_measurements) {
  plant_measurements %>%
    group_by(PotID, Block) %>%
    summarise(N_Dates = n_distinct(Date),
              across(c(Shoot_Count, Shoot_Height_cm, Chlorophyll_Index), mean),
              .groups = "drop")
}
