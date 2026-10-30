base = "/Users/ukaraoz/Work/biochar/greenhouse"

# Figures are always saved as PNG; set to FALSE to skip the PDF versions
options(save_figure.pdf = TRUE)

# Source data links (Google Sheet, greenhouse log) are set in config.yml
source(file.path(base, "R/config.R"))
config = read_config(file.path(base, "config.yml"))

source(file.path(base, "R/read_picarro.R"))
source(file.path(base, "R/read_metadata.R"))
source(file.path(base, "R/save_figure.R"))
source(file.path(base, "R/measurement_windows.R"))
source(file.path(base, "R/read_greenhouse.R"))

source(file.path(base, "scripts/01_load_data.R"))
source(file.path(base, "scripts/02_join_metadata.R"))
source(file.path(base, "scripts/03_plot_timeseries.R"))
source(file.path(base, "scripts/04_greenhouse_environment.R"))
source(file.path(base, "scripts/05_calculate_flux.R"))
source(file.path(base, "scripts/06_plot_flux_fits.R"))
source(file.path(base, "scripts/07_co2_partitioning.R"))
