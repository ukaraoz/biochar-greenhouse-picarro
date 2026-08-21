metadata_url      = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw"
afterwatering_url = "https://docs.google.com/spreadsheets/d/1mEHIjlCxUYlHyZAtM29sMYMGwfevLKi1IuZGaYqSyfw/edit?gid=1522788687#gid=1522788687"

metadata           = read_metadata(metadata_url, afterwatering_url)
lightdark_metadata = metadata[["lightdark_metadata"]]
afterwatering_metadata = metadata[["afterwatering_metadata"]]
metadata_joined    = bind_rows(lightdark_metadata, afterwatering_metadata)

picarro_data_joined = picarro_data %>%
  left_join(
    metadata_joined,
    by = join_by(
      timestamp >= Chamber_Start_DateTime,
      timestamp <= Chamber_End_DateTime
    )
  ) %>%
  filter(!is.na(Chamber_Start_DateTime)) %>%
  select(all_of(select_cols))

writexl::write_xlsx(picarro_data_joined, file.path(base, "output/tables/picarro_data_joined.xlsx"))
