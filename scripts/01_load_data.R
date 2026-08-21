library(googlesheets4)
library(dplyr)
# interactive OAuth flow; token is cached after the first run
googlesheets4::gs4_auth()

# `base` is set in run_all.R; scripts expect it to exist in the environment
data_dir = file.path(base, "picarro_raw_data")
# discover all .dat files recursively so new subdirectory drops are picked up automatically
files = list.files(path = data_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.dat")

# gas measurement columns shared across averaging, joining, and plotting steps
measurements = c("N2O", "CO2", "CH4", "H2O", "NH3")
# columns retained after the metadata join; defined here so downstream scripts
# can reference select_cols without repeating the list
select_cols = c("PotID", "Soil", "Treatment", "Replicate", "Block", "Date",
                measurements, "timestamp", "Order_Index", "type")

# read all .dat files, tag each row with its source file, and add a global
# row_index for traceability back to the raw instrument output
picarro_data <- files %>%
  setNames(files) %>%
  lapply(read_picarro_dat) %>%
  bind_rows(.id = "source_file") %>%
  mutate(row_index = row_number()) %>%
  select(row_index, everything())
