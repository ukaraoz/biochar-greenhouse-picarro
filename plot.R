# reading Piccarro data and aligning it with metadata
# gsheets URLs
library("googlesheets4", "dplyr", "ggplot2")
googlesheets4::gs4_auth()

base = "/Users/ukaraoz/Work/EBI/greenhouse"
data_dir = file.path(base, "Piccaro_Raw_data")
files = list.files(path = data_dir, full.names = T, recursive = T, pattern = "\\.dat")

lightdark_url="https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw/edit?gid=0#gid=0"
afterwatering_url="https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw/edit?gid=1522788687#gid=1522788687"
metadata_url = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw"


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

read_metadata <- function(metadata_url) {
  # read and organize metadata sheet
  ## 1. lightdark
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
  # separate out light and CO2_fixation columns
  # their time slots are non overlapping
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

  metadata = list(
    lightdark_metadata = lightdark_metadata_long,
    afterwatering_metadata = afterwatering_metadata
  )
  return(metadata)
}

metadata = read_metadata(metadata_url)
lightdark_metadata = metadata[["lightdark_metadata"]]
afterwatering_metadata = metadata[["afterwatering_metadata"]]
metadata_joined = bind_rows(lightdark_metadata, afterwatering_metadata)

picarro_data <- files %>%
    setNames(files) %>%
    lapply(read_picarro_dat) %>%
    bind_rows(.id = "source_file") %>%
    mutate(row_index = row_number()) %>%
    select(row_index, everything())

measurements = c("N2O", "CO2", "CH4", "H2O", "NH3")
treatment_levels = c("Conventional soil", "Organic soil", "Biochar", "Biochar+SMT", "Biochar+MPSS", "Biochar+MPSS+SMT")
select_cols = c("PotID", "Soil", "Treatment", "Replicate", "Block", "Date", 
                measurements, "timestamp", "Order_Index", "type")

picarro_data_joined = picarro_data %>%
  left_join(
    metadata_joined,
    by = join_by(
      timestamp >= Chamber_Start_DateTime,
      timestamp <= Chamber_End_DateTime
    )
  ) %>%
  filter(!is.na(Chamber_Start_DateTime)) %>%
  select(select_cols)

picarro_data_averaged = picarro_data_joined %>%
  group_by(PotID, Replicate, Block, Soil, Treatment, Order_Index, type) %>%
  summarise(
    across(measurements, \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    Order_Index = factor(Order_Index, levels = 1:15, ordered = T),
    type = factor(type, levels = c("Light", "CO2_Fixation", "After_Watering"), ordered = T),
    Order_Index_type = factor(interaction(Order_Index, type, sep = "."),
                              levels = as.vector(outer(1:15, c("Light", "CO2_Fixation", "After_Watering"), paste, sep = ".")),
                              ordered = TRUE)) %>%
  select(Soil, Treatment, type, all_of(measurements), PotID, Order_Index, Block) %>%
  tidyr::pivot_longer(cols = all_of(measurements), names_to = "measurement", values_to = "value")

plot = ggplot(picarro_data_averaged,
                aes(x = type,
                    y = value,
                    fill = factor(Treatment, levels = treatment_levels, ordered = T))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_point(aes(shape = Block, group = Treatment), position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +
  facet_wrap(~ measurement, scales = "free_y") +
  labs(x = "Type", y = "Value", fill = "Treatment", shape = "Block") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Conventional soil"  = "#FF0000",
                               "Organic soil"       = "#0000FF",
                               "Biochar"            = "#FFFF00",
                               "Biochar+SMT"        = "#FF8000",
                               "Biochar+MPSS"       = "#8000FF",
                               "Biochar+MPSS+SMT"   = "#00FF00"))
print(plot)
ggsave(file.path(base, "boxplot_Soil_Treatment.pdf"), p_temp, width = 22, height = 12)


