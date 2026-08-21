read_metadata <- function(metadata_url, afterwatering_url) {
  lightdark_metadata = read_sheet(metadata_url, sheet = "LightDark_Measurment") %>%
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
    mutate(
      Light_Order_Index = row_number(Chamber_Start_DateTime_Light),
      CO2_Fixation_Order_Index = row_number(Chamber_Start_DateTime_CO2_Fixation)
    ) %>%
    ungroup()

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
