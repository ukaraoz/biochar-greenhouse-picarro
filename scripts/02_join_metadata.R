# Google Sheet URLs — the main sheet holds LightDark; afterwatering is a
# separate tab referenced by its gid parameter
metadata_url      = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw"
afterwatering_url = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw/edit?gid=1522788687#gid=1522788687"

# read_metadata() returns a named list; unpack and stack into a single table
# covering all three measurement conditions (Light, Dark, After_Watering)
metadata           = read_metadata(metadata_url, afterwatering_url)
lightdark_metadata = metadata[["lightdark_metadata"]]
afterwatering_metadata = metadata[["afterwatering_metadata"]]
metadata_joined    = bind_rows(lightdark_metadata, afterwatering_metadata)

# Local fixes for chamber-time typos in the metadata sheet, listed in
# data/metadata_corrections.csv with the sheet row, reason and status. A fix is
# applied only while the sheet still holds the recorded (wrong) value, so a
# correction made in the sheet itself takes precedence.
metadata_corrections = read.csv(file.path(base, "data/metadata_corrections.csv"), colClasses = "character")
for (i in seq_len(nrow(metadata_corrections))) {
  fix = metadata_corrections[i, ]
  time_col = paste0("Chamber_", fix$Field, "_DateTime")
  row = which(trimws(metadata_joined$PotID) == fix$PotID &
                metadata_joined$Date == as.Date(fix$Date) &
                metadata_joined$Condition == fix$Condition &
                format(metadata_joined[[time_col]], "%H:%M:%S") == fix$Sheet_value)
  if (length(row) == 1) {
    metadata_joined[[time_col]][row] = as.POSIXct(paste(fix$Date, fix$Corrected_value), tz = "America/Los_Angeles")
    message("Metadata correction applied (", fix$Status, "): ", fix$PotID, " ", fix$Date, " ",
            fix$Condition, " ", fix$Field, " ", fix$Sheet_value, " -> ", fix$Corrected_value)
  } else {
    message("Metadata correction skipped, sheet no longer holds ", fix$Sheet_value, ": ",
            fix$PotID, " ", fix$Date, " ", fix$Condition, " ", fix$Field)
  }
}

# Flag chamber windows with missing or implausible times, usually typos in the
# sheet (e.g. an end time before the start time). No readings can fall inside
# such a window, so without this check the measurement drops out silently.
# Chamber closures are ~10 min.
window_min_minutes = 8
window_max_minutes = 13
metadata_window_issues = metadata_joined %>%
  mutate(Duration_min = round(as.numeric(Chamber_End_DateTime - Chamber_Start_DateTime, units = "mins"), 1)) %>%
  filter(is.na(Duration_min) | Duration_min < window_min_minutes | Duration_min > window_max_minutes) %>%
  transmute(PotID, Date, Condition,
            Start = format(Chamber_Start_DateTime, "%H:%M:%S"),
            End = format(Chamber_End_DateTime, "%H:%M:%S"),
            Duration_min) %>%
  arrange(Date, PotID)

if (nrow(metadata_window_issues) > 0) {
  warning(nrow(metadata_window_issues), " chamber window(s) in the metadata sheet have missing times or a ",
          "duration outside ", window_min_minutes, "-", window_max_minutes, " min; fix them in the sheet:\n",
          paste(capture.output(print(as.data.frame(metadata_window_issues), row.names = FALSE)), collapse = "\n"),
          call. = FALSE)
}

# Non-equi join: match each Picarro timestamp to the chamber window it falls in.
# A left_join is used so unmatched rows are retained for inspection; the
# subsequent filter drops them, keeping only rows within a valid chamber window.
picarro_data_joined = picarro_data %>%
  left_join(
    metadata_joined,
    by = join_by(
      timestamp >= Chamber_Start_DateTime,
      timestamp <= Chamber_End_DateTime
    )
  ) %>%
  # rows with no matching chamber window have NA Chamber_Start_DateTime
  filter(!is.na(Chamber_Start_DateTime)) %>%
  select(all_of(select_cols))

# Report valid chamber windows that matched no Picarro readings (e.g. the
# analyser was not logging, as on 2026-04-29 between 14:24 and 17:17), so the
# missing measurements are listed rather than dropped silently.
windows_without_data = metadata_joined %>%
  filter(!is.na(Chamber_Start_DateTime), Chamber_End_DateTime > Chamber_Start_DateTime) %>%
  anti_join(distinct(picarro_data_joined, PotID, Order_Index, Condition),
            by = c("PotID", "Order_Index", "Condition")) %>%
  transmute(PotID, Date, Condition,
            Start = format(Chamber_Start_DateTime, "%H:%M:%S"),
            End = format(Chamber_End_DateTime, "%H:%M:%S")) %>%
  arrange(Date, Start)

if (nrow(windows_without_data) > 0) {
  warning(nrow(windows_without_data), " chamber window(s) have no Picarro readings and are missing from the data:\n",
          paste(capture.output(print(as.data.frame(windows_without_data), row.names = FALSE)), collapse = "\n"),
          call. = FALSE)
}

writexl::write_xlsx(picarro_data_joined, file.path(base, "output/tables/picarro_data_joined.xlsx"))
