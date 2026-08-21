# Google Sheet URLs — the main sheet holds LightDark; afterwatering is a
# separate tab referenced by its gid parameter
metadata_url      = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw"
afterwatering_url = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw/edit?gid=1522788687#gid=1522788687"

# read_metadata() returns a named list; unpack and stack into a single table
# covering all three measurement types (Light, CO2_Fixation, After_Watering)
metadata           = read_metadata(metadata_url, afterwatering_url)
lightdark_metadata = metadata[["lightdark_metadata"]]
afterwatering_metadata = metadata[["afterwatering_metadata"]]
metadata_joined    = bind_rows(lightdark_metadata, afterwatering_metadata)

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
