# biochar-greenhouse-picarro

Analysis of Picarro gas analyser measurements from a greenhouse experiment investigating the effect of biochar soil amendments on greenhouse gas emissions (N₂O, CO₂, CH₄, H₂O, NH₃).

## Experiment overview

Pots were prepared with six soil treatments:

| Treatment | Description |
|---|---|
| Conventional soil | Unamended mineral soil |
| Organic soil | Unamended organic soil |
| Biochar | Mineral soil + biochar |
| Biochar+SMT | Mineral soil + biochar + SMT inoculum |
| Biochar+MPSS | Mineral soil + biochar + MPSS inoculum |
| Biochar+MPSS+SMT | Mineral soil + biochar + MPSS + SMT inocula |

Gas flux measurements were taken under three conditions (`Condition`):

- **Light** — chamber closed under light
- **CO2_Fixation** — chamber closed for CO₂ fixation measurement
- **After_Watering** — chamber closed after a watering event

Each pot was measured repeatedly over time; `Order_Index` ranks measurement occasions chronologically within each pot.

## Resources

- [Biochar Greenhouse Experiment Master Document](https://docs.google.com/document/d/12pguhFinevDNARxmVrXZqBQZc2flD6XqkcNT2dJXbMQ/edit?tab=t.0) — experimental protocol: design, treatments, growth conditions and methods (*Brachypodium distachyon*, BioEpic Greenhouse, LBNL)
- [Greenhouse_Temperature.csv](https://drive.google.com/file/d/1eyDQzHW4PIhj3ZT2eYb9SAsSlT4buZIM/view?usp=drive_link) — greenhouse environment log: indoor temperature (°C), relative humidity (%) and PAR light, ~15-min intervals

## Repository structure

```
greenhouse/
├── R/
│   ├── read_picarro.R      # read_picarro_dat(): parses raw .dat files
│   └── read_metadata.R     # as_hms_text(), read_metadata(): reads chamber timing from Google Sheets
├── scripts/
│   ├── 01_load_data.R      # reads all .dat files → picarro_data
│   ├── 02_join_metadata.R  # joins chamber windows → picarro_data_joined
│   ├── 03_average.R        # averages readings per chamber window → picarro_data_averaged
│   ├── 04_plot.R           # boxplots of gas measurements by treatment and condition
│   ├── 05_mixed_models.R   # pairwise repeated-measures mixed models per condition × gas × comparison
│   ├── 06_write_markdown.R # writes the mixed-model results to a Markdown report
│   ├── 07_write_overview_pdf.R # writes the overview tables to a one-page PDF
│   ├── 08_plot_timeseries.R # raw readings over chamber windows, replicates overlaid
│   ├── 09_greenhouse_temperature.R # greenhouse temperature per chamber window (observed or imputed)
│   ├── 10_calculate_flux.R # CO2, CH4 and N2O fluxes per chamber window (goFlux)
│   └── 11_plot_flux_fits.R # flux-fit diagnostic figures, one per gas and treatment
├── output/
│   ├── figures/            # PDF plots
│   └── tables/             # Excel exports
├── picarro_raw_data/       # raw Picarro .dat files (not tracked in git)
└── run_all.R               # entry point — runs the full pipeline
```

## Running the pipeline

```r
source("run_all.R")
```

`run_all.R` sets `base` to the project root, sources the two function files in `R/`, then runs the eleven pipeline scripts in order. A Google account with access to the metadata Google Sheet and the greenhouse temperature CSV on Google Drive is required; the OAuth tokens are cached after the first interactive logins via `googlesheets4::gs4_auth()` and `googledrive::drive_auth()`.

## Data

### Inputs
- **Raw Picarro output** — whitespace-delimited `.dat` files in `picarro_raw_data/`. Each file contains sub-second gas readings with an `EPOCH_TIME` Unix timestamp column.
- **Measurement metadata** — Google Sheet (`Brodie_EBI_Greenhouse_PiccaroMeasurement_2026`) recording the start and end times of each chamber placement for each pot and measurement condition.

### Outputs

| File | Description |
|---|---|
| `output/tables/picarro_data_joined.xlsx` | Raw Picarro readings matched to chamber windows |
| `output/tables/picarro_data_averaged.xlsx` | One mean value per gas per pot per measurement occasion |
| `output/figures/boxplot_by_treatment.pdf` | Boxplots of all five gases by treatment and measurement condition |
| `output/figures/timeseries_by_replicate.pdf` | One page per gas × treatment: raw readings vs. time since chamber closure, replicates overlaid, x-axis split into Light / Dark / After watering ranges, one panel per measurement occasion |
| `output/tables/chamber_window_temperature.xlsx` | Greenhouse temperature (°C) at the midpoint of each chamber window. `Temp_source` is `observed` (interpolated from the logger) or `imputed` (median daily cycle; the logger starts 2026-04-30), with a p10–p90 range for imputed values |
| `output/figures/greenhouse_temperature.pdf` | Logged greenhouse temperature, the imputed daily cycle before logging began, and the chamber-window values |
| `output/tables/flux_results.xlsx` | CO2 (µmol m⁻² s⁻¹), CH4 and N2O (nmol m⁻² s⁻¹) fluxes per chamber window from dry mole fractions: `best.flux` and its `model` (linear or Hutchinson–Mosier, chosen by goFlux `best.flux()`), both model estimates, minimal detectable flux (`MDF`) and `quality.check` flags |
| `output/figures/flux_fits/flux_fits_<gas>_<treatment>.png` | Flux-fit diagnostics, 18 figures (3 gases × 6 treatments) at 300 dpi: dry mole fraction vs time since closure, one row per occasion and one column per condition, the four replicates overlaid with their fitted model and labelled flux (`*` below MDF), deadband shaded (~4–11 MB each) |
| `output/tables/mixed_model_results.xlsx` | Pairwise mixed models (condition × gas × comparison): treatment difference, 95% CI, % change, direction and p-values |
| `output/tables/mixed_model_results.md` | Markdown report of the mixed-model results: overview grid and full tables per condition |
| `output/tables/mixed_model_overview.pdf` | One-page PDF of the overview tables (% change and p per gas, condition and comparison) |
| `output/figures/mixed_model_diagnostics.pdf` | Residual diagnostics for each condition × gas × comparison model |

## Dependencies

```r
install.packages(c("googlesheets4", "dplyr", "tidyr", "ggplot2", "writexl", "nlme", "emmeans", "gridExtra", "googledrive"))
devtools::install_github("Qepanna/goFlux")  # flux calculation; not on CRAN
```
