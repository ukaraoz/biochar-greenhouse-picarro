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

# End-point total water added to each pot, summed over its waterings
# (total_water_added() in R/read_metadata.R)
# and water use efficiency (end-point shoot dry weight / total water,
# water_use_efficiency() in R/read_metadata.R)
water_per_pot = total_water_added(afterwatering_metadata) %>%
  water_use_efficiency(config$data_sources$endpoint_sheet, .)
writexl::write_xlsx(water_per_pot, file.path(base, "output/tables/water_added_per_pot.xlsx"))

# Water use efficiency by treatment: one point per pot (shape = block),
# treatment mean ± SD in black
p = ggplot2::ggplot(water_per_pot %>% mutate(Treatment = factor(Treatment, levels = treatment_levels)),
                    ggplot2::aes(Treatment, WUE_mg_per_g)) +
  ggplot2::geom_point(ggplot2::aes(color = Treatment, shape = Block), size = 2.5,
                      position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1)) +
  ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1),
                        geom = "pointrange", color = "black", size = 0.4,
                        position = ggplot2::position_nudge(x = 0.3)) +
  ggplot2::scale_color_manual(values = timeline_treatment_colors, guide = "none") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(title = "Water use efficiency",
                subtitle = "End-point shoot dry weight / total water added over 15 waterings; black: treatment mean ± SD",
                x = NULL, y = "Water use efficiency (mg shoot dry weight / g water)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
                 panel.grid.minor = ggplot2::element_blank())
save_figure(file.path(base, "output/figures/water_use_efficiency.pdf"), function() print(p),
            width = 7, height = 5)

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
                         logger_times = greenhouse_log_times(read_greenhouse_log(config$data_sources$greenhouse_log)))
save_figure(file.path(base, "output/figures/picarro_log_timeline.pdf"), function() print(p),
            width = 15, height = 15, pdf_device = grDevices::cairo_pdf)
