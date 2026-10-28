base = "/Users/ukaraoz/Work/biochar/greenhouse"

source(file.path(base, "R/read_picarro.R"))
source(file.path(base, "R/read_metadata.R"))

source(file.path(base, "scripts/01_load_data.R"))
source(file.path(base, "scripts/02_join_metadata.R"))
source(file.path(base, "scripts/03_average.R"))
source(file.path(base, "scripts/04_plot.R"))
source(file.path(base, "scripts/05_mixed_models.R"))
source(file.path(base, "scripts/06_write_markdown.R"))
source(file.path(base, "scripts/07_write_overview_pdf.R"))
source(file.path(base, "scripts/08_plot_timeseries.R"))
source(file.path(base, "scripts/09_greenhouse_temperature.R"))
source(file.path(base, "scripts/10_calculate_flux.R"))
source(file.path(base, "scripts/11_plot_flux_fits.R"))
source(file.path(base, "scripts/12_co2_partitioning.R"))
