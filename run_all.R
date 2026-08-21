base = "/Users/ukaraoz/Work/biochar/greenhouse"

source(file.path(base, "R/read_picarro.R"))
source(file.path(base, "R/read_metadata.R"))

source(file.path(base, "scripts/01_load_data.R"))
source(file.path(base, "scripts/02_join_metadata.R"))
source(file.path(base, "scripts/03_average.R"))
source(file.path(base, "scripts/04_plot.R"))
