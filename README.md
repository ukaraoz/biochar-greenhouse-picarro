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
- **Dark** — chamber closed in the dark (photosynthesis stops), so the CO₂ flux is ecosystem respiration; paired with Light to estimate CO₂ fixation (GPP = respiration − net exchange)
- **After_Watering** — chamber closed after a watering event

Each pot was measured repeatedly over time; `Order_Index` ranks measurement occasions chronologically within each pot.

## Resources

- [Biochar Greenhouse Experiment Master Document](https://docs.google.com/document/d/12pguhFinevDNARxmVrXZqBQZc2flD6XqkcNT2dJXbMQ/edit?tab=t.0) — experimental protocol: design, treatments, growth conditions and methods (*Brachypodium distachyon*, BioEpic Greenhouse, LBNL)
- [Greenhouse_Temperature.csv](https://drive.google.com/file/d/184jLi4XAqnDkcbxAWHGWeuM4ZFq0i-6E/view) — greenhouse environment log: indoor temperature (°C), relative humidity (%) and PAR light, ~15-min intervals

## Repository structure

```
greenhouse/
├── config.yml              # source data links (metadata and end-point Google Sheet, greenhouse log CSV)
├── R/
│   ├── config.R            # read_config(): reads and checks config.yml
│   ├── read_picarro.R      # read_picarro_dat(): parses raw .dat files
│   ├── read_metadata.R     # as_hms_text(), read_metadata(): reads chamber timing from Google Sheets
│   ├── read_greenhouse.R   # read_greenhouse_log(), greenhouse_log_times(): greenhouse logger CSV from Google Drive
│   ├── save_figure.R       # save_figure(): writes each figure as PNG (and PDF unless turned off)
│   └── measurement_windows.R # chamber-window utilities: check_window_durations(), find_windows_without_data(), measurement_days(), summarise_in_windows(), picarro_log_segments(), picarro_logging_gaps(), plot_window_timeline()
├── scripts/
│   ├── 01_load_data.R      # reads all .dat files → picarro_data
│   ├── 02_join_metadata.R  # joins chamber windows → picarro_data_joined; window checks, water per pot and the logging timeline figure
│   ├── 03_plot_timeseries.R # raw readings over chamber windows, replicates overlaid
│   ├── 04_greenhouse_environment.R # greenhouse temperature, relative humidity and PAR per chamber window, one figure each
│   ├── 05_calculate_flux.R # CO2, CH4 and N2O fluxes per chamber window (goFlux)
│   ├── 06_plot_flux_fits.R # flux-fit diagnostic figures, one per gas and treatment
│   └── 07_co2_partitioning.R # NEE, ER and GPP per pot and occasion from the light/dark CO2 fluxes
├── output/
│   ├── figures/            # figures (PNG, plus PDF unless turned off in run_all.R)
│   └── tables/             # Excel exports
├── picarro_raw_data/       # raw Picarro .dat files (not tracked in git)
└── run_all.R               # entry point — runs the full pipeline
```

## Running the pipeline

The links to the source data are set in `config.yml` under `data_sources`: `metadata_sheet` and `afterwatering_sheet` (chamber times, Google Sheet), `endpoint_sheet` (end-point pot measurements, same Sheet, tab pH_EC_EndPoint_July2026; not used by the pipeline yet) and `greenhouse_log` (greenhouse temperature, RH and PAR CSV on Google Drive). When a source file is replaced, update its link there; the scripts read `config$data_sources$<name>`.

```r
source("run_all.R")
```

`run_all.R` sets `base` to the project root, reads `config.yml` into `config`, sources the function files in `R/`, then runs the seven pipeline scripts in order. A Google account with access to the metadata Google Sheet and the greenhouse temperature CSV on Google Drive is required; the OAuth tokens are cached after the first interactive logins via `googlesheets4::gs4_auth()` and `googledrive::drive_auth()`.

## Data

### Inputs
- **Raw Picarro output** — whitespace-delimited `.dat` files in `picarro_raw_data/`. Each file contains sub-second gas readings with an `EPOCH_TIME` Unix timestamp column.
- **Measurement metadata** — Google Sheet (`Brodie_EBI_Greenhouse_PiccaroMeasurement_2026`) recording the start and end times of each chamber placement for each pot and measurement condition.

### Outputs

| File | Description |
|---|---|
| `output/tables/water_added_per_pot.xlsx` | End-point total water added to each pot (`Water_g_60WHC` summed over its 15 waterings, After_WateringMeasurment tab columns A:N) |
| `output/tables/picarro_data_joined.xlsx` | Raw Picarro readings matched to chamber windows, including dry mole fractions (not tracked in git, ~90 MB; regenerated by the pipeline) |
| `output/figures/timeseries_by_replicate.pdf` | One page per gas × treatment: raw readings vs. time since chamber closure, replicates overlaid, x-axis split into Light / Dark / After watering ranges, one panel per measurement occasion |
| `output/tables/chamber_window_greenhouse.xlsx` | Greenhouse temperature (`Temp_C`, °C), relative humidity (`RH_pct`, %) and PAR light (`PAR`) at the midpoint of each chamber window. `<Temp/RH/PAR>_source` is `observed` (interpolated from the logger) or `imputed` (median daily cycle, only for windows outside the log; the log covers 2026-03-01 to 2026-05-30, so none currently), with a p10–p90 range for imputed values. `<Temp/RH/PAR>_mean`, `_median` and `_sd` summarise each variable over the whole window, by `window_summary_method` in `04_greenhouse_environment.R`: `"readings"` (default; raw readings inside the window, or the `window_n_closest` nearest readings when none fall inside; SD blank when only one reading is used) or `"interpolate"` (log interpolated to 1-min steps, time-weighted). `_n_readings` counts the raw readings inside the window, `_basis` says how the values were obtained (interpolated / inside window / closest n), and `_max_offset_min` is how far outside the window the farthest reading used lies |
| `output/figures/greenhouse_temperature.pdf`, `greenhouse_relative_humidity.pdf`, `greenhouse_PAR.pdf` | Logged greenhouse variable over the measurement period (first to last measurement day in the sheet), measurement days shaded by day 1 (blocks A–B) / day 2 (blocks C–D) with occasion numbers, and the value at each chamber window coloured by condition (PDF + PNG) |
| `output/figures/picarro_log_timeline.pdf` | One row per measurement day: Picarro log segments (tan by sampling rate, ≈0.8 readings/s until 2026-04-13 midday, ≈1.2 after; white edges = hourly files), chamber windows coloured by treatment with the block letter inside, a grey condition bar underneath (Light, Dark, After watering), logging gaps outlined, and greenhouse logger times (red lines; solid = temperature, RH and PAR, dotted = temperature only) where they overlap Picarro logging or a chamber window (PDF + PNG) |
| `output/tables/flux_results.xlsx` | CO2 (µmol m⁻² s⁻¹), CH4 and N2O (nmol m⁻² s⁻¹) fluxes per chamber window from dry mole fractions: `best.flux` and its `model` (linear or Hutchinson–Mosier, chosen by goFlux `best.flux()`), both model estimates, minimal detectable flux (`MDF`) and `quality.check` flags. `Temp_C` is the greenhouse temperature used for the window (window mean by `window_summary_method`, default readings) and `Temp_basis` how it was obtained |
| `output/figures/flux_fits/flux_fits_<gas>_<treatment>.png` | Flux-fit diagnostics, 18 figures (3 gases × 6 treatments) at 300 dpi: dry mole fraction vs time since closure, one row per occasion and one column per condition, the four replicates overlaid with their fitted model and labelled flux (`*` below MDF), deadband shaded (~4–11 MB each) |
| `output/tables/CO2_partitioning.xlsx` | Per pot and occasion: NEE (light flux), ER (dark flux), GPP = ER − NEE and NEE after watering, in µmol CO₂ m⁻² s⁻¹, with review flags (missing window, negative ER or GPP, room air, unusual light–dark timing, below MDF); second sheet has treatment means per occasion; further sheets give each pot's total GPP, ER and NEE over the experiment (g C m⁻², daily rates assuming a 15 h photoperiod and 24 h respiration, integrated between measurement dates), treatment means and Tukey pairwise comparisons from a Treatment + Block model. Sheet `variance_partition`: for GPP, ER and NEE, variance explained by occasion alone, by greenhouse temperature + PAR + RH alone, and by occasion after correcting for the environment (± treatment and block), as R² gains in linear models; plus a mixed model with pot as random intercept (likelihood-ratio test and marginal R² gain for occasion) |
| `output/figures/CO2_partitioning_by_occasion.pdf` | One column per treatment: GPP, ER and NEE over occasions (top row), and underneath the greenhouse temperature, PAR and relative humidity during the same pots' Light and Dark windows; individual pots faint, treatment mean bold (PDF + PNG) |
| `output/figures/CO2_partitioning_cumulative.pdf` | Page 1: cumulative GPP, ER and NEE over time per pot; page 2: end-of-experiment totals by treatment (pots and mean ± SE) |

## Metadata corrections (to confirm with Slimane)

Seven chamber windows in the metadata sheet have impossible times (an end before the start, or a 0–1 min window), so their readings were silently dropped. `data/metadata_corrections.csv` lists a proposed fix for each (sheet tab and row, recorded value, corrected value, reasoning) and `02_join_metadata.R` applies them, only while the sheet still holds the recorded value. The corrected times are inferred from the ~10 min window length and the measurement sequence and are **unconfirmed**: check them with Slimane, then fix the sheet (the local correction then stops applying) or mark the row confirmed. `02_join_metadata.R` also warns about any remaining window with missing times or a duration outside 8–13 min.

## Known data gaps

On 2026-04-29 (occasion 8, block B) the Picarro stopped logging between 14:24:01 and 17:17:14. The ten windows recorded in the sheet for that afternoon (Conv_BC-SMT_2_B Dark and After watering; Conv_BC-MPSS_2_B and Conv_BC-MPSS-SMT_2_B Light, Dark and After watering; Conv_Ctrl_2_B and Conv_BC_2_B After watering) have no readings in `picarro_raw_data/`, and Conv_BC-SMT_2_B Light is cut off after ~2 min. These measurements cannot be recovered unless another log file from that afternoon turns up. `02_join_metadata.R` warns about every window that matches no Picarro readings.

## Dependencies

```r
install.packages(c("googlesheets4", "dplyr", "tidyr", "ggplot2", "writexl", "emmeans", "ggh4x", "googledrive", "yaml", "ggnewscale"))
devtools::install_github("Qepanna/goFlux")  # flux calculation; not on CRAN
```
