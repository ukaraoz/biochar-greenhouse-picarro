library(googlesheets4)
library(dplyr)
googlesheets4::gs4_auth()

data_dir = file.path(base, "Piccaro_Raw_data")
files = list.files(path = data_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.dat")

measurements = c("N2O", "CO2", "CH4", "H2O", "NH3")
select_cols = c("PotID", "Soil", "Treatment", "Replicate", "Block", "Date",
                measurements, "timestamp", "Order_Index", "type")

picarro_data <- files %>%
  setNames(files) %>%
  lapply(read_picarro_dat) %>%
  bind_rows(.id = "source_file") %>%
  mutate(row_index = row_number()) %>%
  select(row_index, everything())
