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

Gas flux measurements were taken under three conditions (`type`):

- **Light** — chamber closed under light
- **CO2_Fixation** — chamber closed for CO₂ fixation measurement
- **After_Watering** — chamber closed after a watering event

Each pot was measured repeatedly over time; `Order_Index` ranks measurement occasions chronologically within each pot.

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
│   └── 04_plot.R           # boxplots of gas measurements by treatment and type
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

`run_all.R` sets `base` to the project root, sources the two function files in `R/`, then runs the four pipeline scripts in order. A Google account with access to the metadata Google Sheet is required; the OAuth token is cached after the first interactive login via `googlesheets4::gs4_auth()`.

## Data

### Inputs
- **Raw Picarro output** — whitespace-delimited `.dat` files in `picarro_raw_data/`. Each file contains sub-second gas readings with an `EPOCH_TIME` Unix timestamp column.
- **Measurement metadata** — Google Sheet (`Brodie_EBI_Greenhouse_PiccaroMeasurement_2026`) recording the start and end times of each chamber placement for each pot and measurement type.

### Outputs

| File | Description |
|---|---|
| `output/tables/picarro_data_joined.xlsx` | Raw Picarro readings matched to chamber windows |
| `output/tables/picarro_data_averaged.xlsx` | One mean value per gas per pot per measurement occasion |
| `output/figures/boxplot_by_treatment.pdf` | Boxplots of all five gases by treatment and measurement type |

## Dependencies

```r
install.packages(c("googlesheets4", "dplyr", "tidyr", "ggplot2", "writexl"))
```
