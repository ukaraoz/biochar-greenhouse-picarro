# Google Sheet URLs — the main sheet holds LightDark; afterwatering is a
# separate tab referenced by its gid parameter
metadata_url      = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw"
afterwatering_url = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw/edit?gid=1522788687#gid=1522788687"

# read_metadata() returns a named list; unpack and stack into a single table
# covering all three measurement conditions (Light, CO2_Fixation, After_Watering)
metadata           = read_metadata(metadata_url, afterwatering_url)
lightdark_metadata = metadata[["lightdark_metadata"]]
afterwatering_metadata = metadata[["afterwatering_metadata"]]
metadata_joined    = bind_rows(lightdark_metadata, afterwatering_metadata)

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

writexl::write_xlsx(picarro_data_joined, file.path(base, "output/tables/picarro_data_joined.xlsx"))
