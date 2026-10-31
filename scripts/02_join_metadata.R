# Google Sheet URLs from config.yml — the main sheet holds LightDark;
# afterwatering is a separate tab referenced by its gid parameter
metadata_url      = config$data_sources$metadata_sheet
afterwatering_url = config$data_sources$afterwatering_sheet

# read_metadata() returns a named list; unpack and stack into a single table
# covering all three measurement conditions (Light, Dark, After_Watering)
metadata           = read_metadata(metadata_url, afterwatering_url)
lightdark_metadata = metadata[["lightdark_metadata"]]
afterwatering_metadata = metadata[["afterwatering_metadata"]]
metadata_joined    = bind_rows(lightdark_metadata, afterwatering_metadata)

# figure labels map Treatment to treatment_names through the PotID prefix
# (treatment_prefixes in 01_load_data.R); stop if the sheet disagrees
prefix_mismatch = metadata_joined %>%
  distinct(PotID = trimws(PotID), Treatment) %>%
  filter(is.na(treatment_prefixes[Treatment]) | sub("_[0-9]+_[A-D]$", "", PotID) != treatment_prefixes[Treatment])
if (nrow(prefix_mismatch) > 0) {
  stop("PotID prefix does not match treatment_prefixes for: ",
       paste(prefix_mismatch$PotID, prefix_mismatch$Treatment, sep = " / ", collapse = ", "), call. = FALSE)
}

# End-point total water added to each pot, summed over its waterings
# (total_water_added() in R/read_metadata.R)
# and water use efficiency (end-point shoot dry weight / total water,
# water_use_efficiency() in R/read_metadata.R)
water_per_pot = total_water_added(afterwatering_metadata) %>%
  water_use_efficiency(config$data_sources$endpoint_sheet, .)
writexl::write_xlsx(water_per_pot, file.path(base, "output/tables/water_added_per_pot.xlsx"))

# Boxplot per treatment with the 4 pots overlaid (jittered), one panel per
# variable (data: one row per pot with Treatment, variable and value)
plot_treatment_boxplot = function(data) {
  ggplot2::ggplot(data %>% mutate(Treatment = factor(Treatment, levels = treatment_levels)),
                  ggplot2::aes(Treatment, value)) +
    # outliers are drawn by the overlaid points
    ggplot2::geom_boxplot(fill = "grey90", color = "grey35", width = 0.55, outlier.shape = NA) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1),
                        size = 1.8, alpha = 0.8) +
    ggplot2::facet_wrap(~ variable, scales = "free_y", strip.position = "left") +
    ggplot2::scale_x_discrete(labels = treatment_labels_wrapped) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.placement = "outside", strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = ggplot2::rel(1.1)))
}

# Water use efficiency by treatment
p = plot_treatment_boxplot(water_per_pot %>%
                             transmute(Treatment, value = WUE_mg_per_g,
                                       variable = "Water use efficiency (mg shoot dry weight / g water)"))
save_figure(file.path(base, "output/figures/water_use_efficiency.pdf"), function() print(p),
            width = 9, height = 5)

# Plant measurements over time (6 dates), averaged per pot over the dates
# (read_plant_measurements() and plant_measurements_per_pot() in R/read_metadata.R)
plant_measurements = read_plant_measurements(config$data_sources$plant_measurements_sheet)
plant_per_pot = plant_measurements_per_pot(plant_measurements) %>%
  inner_join(distinct(water_per_pot, PotID, Treatment), by = "PotID") %>%
  relocate(Treatment, .after = PotID)
if (nrow(plant_per_pot) != nrow(water_per_pot)) {
  warning("plant measurements found for ", nrow(plant_per_pot), " of ", nrow(water_per_pot), " pots", call. = FALSE)
}
writexl::write_xlsx(plant_per_pot, file.path(base, "output/tables/plant_measurements_per_pot.xlsx"))

plant_variables = c(Shoot_Count = "Shoot count", Shoot_Height_cm = "Shoot height (cm)",
                    Chlorophyll_Index = "Chlorophyll index (leaves 1-5)")
p = plot_treatment_boxplot(plant_per_pot %>%
                             tidyr::pivot_longer(all_of(names(plant_variables)), names_to = "variable") %>%
                             mutate(variable = factor(plant_variables[variable], levels = plant_variables))) +
  # three panels side by side: full treatment names, angled
  ggplot2::scale_x_discrete(labels = treatment_labels) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
save_figure(file.path(base, "output/figures/plant_measurements.pdf"), function() print(p),
            width = 14, height = 6)

# Root imaging (WinRHIZO, 4 replicate measurements per pot).
# read_root_imaging() maps each measurement to the standard PotID through
# Treatment + Block and cross-checks the sheet's PotID column (see
# R/read_metadata.R). Per-pot table = mean of the 4 measurements.
root_measurements = read_root_imaging(config$data_sources$root_imaging_sheet,
                                      distinct(water_per_pot, PotID, Treatment, Block))
root_per_pot = root_imaging_per_pot(root_measurements)
writexl::write_xlsx(list(per_pot = root_per_pot, per_measurement = root_measurements),
                    file.path(base, "output/tables/root_imaging.xlsx"))

# Boxplot per treatment of the replicate measurements of each pot, overlaid
# with every measurement as a point: shape = replicate measurement (1-4),
# color = block (primary colors; yellow darkened to stay visible on white,
# green as the fourth). One panel per variable.
# (data: one row per measurement with Treatment, Block, Replicate, variable, value)
plot_replicate_boxplot = function(data, ncol = 3) {
  data %>%
    mutate(Treatment = factor(Treatment, levels = treatment_levels), Replicate = factor(Replicate)) %>%
    ggplot2::ggplot(ggplot2::aes(Treatment, value)) +
    # outliers are drawn by the overlaid points
    ggplot2::geom_boxplot(fill = "grey92", color = "grey45", width = 0.6, outlier.shape = NA) +
    ggplot2::geom_point(ggplot2::aes(color = Block, shape = Replicate),
                        position = ggplot2::position_jitter(width = 0.22, height = 0, seed = 1),
                        size = 2, stroke = 0.8) +
    ggplot2::facet_wrap(~ variable, ncol = ncol, scales = "free_y", strip.position = "left") +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 4), name = "Replicate measurement") +
    ggplot2::scale_color_manual(values = c(A = "#E41A1C", B = "#1F4FE0", C = "#E6B800", D = "#1A9E3A")) +
    ggplot2::scale_x_discrete(labels = treatment_labels) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
                   strip.placement = "outside", strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = ggplot2::rel(1.05)))
}

# Each root imaging variable by treatment
root_variable_labels = c("Length_cm" = "Root length (cm)", "ProjArea_cm2" = "Projected area (cm²)",
                         "SurfArea_cm2" = "Surface area (cm²)", "AvgDiam_mm" = "Average diameter (mm)",
                         "LenPerVol_cm/m3" = "Length per volume (cm/m³)", "RootVolume_cm3" = "Root volume (cm³)",
                         "Tips" = "Tips", "Forks" = "Forks", "Crossings" = "Crossings")
p = plot_replicate_boxplot(root_measurements %>%
                             tidyr::pivot_longer(all_of(root_imaging_variables), names_to = "variable") %>%
                             mutate(variable = factor(root_variable_labels[variable], levels = root_variable_labels)))
save_figure(file.path(base, "output/figures/root_imaging.pdf"), function() print(p),
            width = 14, height = 13, pdf_device = grDevices::cairo_pdf)

# Root mycorrhizal colonization, one row per pot. The sheet has no pot ID;
# read_mycorrhizal_colonization() maps its Treatment + Block to the standard
# PotID and cross-checks the shoot dry weight against the end-point sheet
# (see R/read_metadata.R).
mycorrhizal_colonization = read_mycorrhizal_colonization(config$data_sources$mycorrhizal_colonization_sheet,
                                                         water_per_pot)
# The 4 replicate reads per pot from the RawData tab (50 points each); their
# mean must equal the PlotingData percentage (read_mycorrhizal_replicates())
mycorrhizal_replicates = read_mycorrhizal_replicates(config$data_sources$mycorrhizal_colonization_sheet,
                                                     water_per_pot, mycorrhizal_colonization)
writexl::write_xlsx(list(per_pot = mycorrhizal_colonization, per_replicate = mycorrhizal_replicates),
                    file.path(base, "output/tables/mycorrhizal_colonization.xlsx"))

# Mycorrhizal colonization by treatment, same style as the root imaging figure
p = plot_replicate_boxplot(mycorrhizal_replicates %>%
                             transmute(Treatment, Block, Replicate, value = Colonization_pct,
                                       variable = "Root mycorrhizal colonization (%)"), ncol = 1)
save_figure(file.path(base, "output/figures/mycorrhizal_colonization.pdf"), function() print(p),
            width = 8, height = 6.5)

# Local fixes for chamber-time typos in the metadata sheet, listed in
# data/metadata_corrections.csv with the sheet row, reason and status. A fix is
# applied only while the sheet still holds the recorded (wrong) value, so a
# correction made in the sheet itself takes precedence.
metadata_corrections = read.csv(file.path(base, "data/metadata_corrections.csv"), colClasses = "character")
for (i in seq_len(nrow(metadata_corrections))) {
  fix = metadata_corrections[i, ]
  time_col = paste0("Chamber_", fix$Field, "_DateTime")
  row = which(trimws(metadata_joined$PotID) == fix$PotID &
                metadata_joined$Date == as.Date(fix$Date) &
                metadata_joined$Condition == fix$Condition &
                format(metadata_joined[[time_col]], "%H:%M:%S") == fix$Sheet_value)
  if (length(row) == 1) {
    metadata_joined[[time_col]][row] = as.POSIXct(paste(fix$Date, fix$Corrected_value), tz = "America/Los_Angeles")
    message("Metadata correction applied (", fix$Status, "): ", fix$PotID, " ", fix$Date, " ",
            fix$Condition, " ", fix$Field, " ", fix$Sheet_value, " -> ", fix$Corrected_value)
  } else {
    message("Metadata correction skipped, sheet no longer holds ", fix$Sheet_value, ": ",
            fix$PotID, " ", fix$Date, " ", fix$Condition, " ", fix$Field)
  }
}

# Flag chamber windows with missing or implausible times, usually typos in the
# sheet (check_window_durations() in R/measurement_windows.R).
window_min_minutes = 8
window_max_minutes = 13
metadata_window_issues = check_window_durations(metadata_joined, window_min_minutes, window_max_minutes)

if (nrow(metadata_window_issues) > 0) {
  warning(nrow(metadata_window_issues), " chamber window(s) in the metadata sheet have missing times or a ",
          "duration outside ", window_min_minutes, "-", window_max_minutes, " min; fix them in the sheet:\n",
          paste(capture.output(print(as.data.frame(metadata_window_issues), row.names = FALSE)), collapse = "\n"),
          call. = FALSE)
}

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

# Report valid chamber windows that matched no Picarro readings (e.g. the
# analyser was not logging, as on 2026-04-29 between 14:24 and 17:17), so the
# missing measurements are listed rather than dropped silently.
windows_without_data = find_windows_without_data(metadata_joined, picarro_data_joined)

if (nrow(windows_without_data) > 0) {
  warning(nrow(windows_without_data), " chamber window(s) have no Picarro readings and are missing from the data:\n",
          paste(capture.output(print(as.data.frame(windows_without_data), row.names = FALSE)), collapse = "\n"),
          call. = FALSE)
}

writexl::write_xlsx(picarro_data_joined, file.path(base, "output/tables/picarro_data_joined.xlsx"))

# Timeline of Picarro logging, chamber windows and greenhouse log times per
# measurement day (plot_window_timeline() in R/measurement_windows.R,
# read_greenhouse_log() in R/read_greenhouse.R)
p = plot_window_timeline(metadata_joined,
                         picarro_log_segments(picarro_data, days = unique(metadata_joined$Date)),
                         logger_times = greenhouse_log_times(read_greenhouse_log(config$data_sources$greenhouse_log)),
                         treatment_labels = treatment_labels)
save_figure(file.path(base, "output/figures/picarro_log_timeline.pdf"), function() print(p),
            width = 15, height = 15, pdf_device = grDevices::cairo_pdf)
