library(ggplot2)

# Greenhouse environment (indoor temperature, relative humidity and PAR light)
# for each chamber window, plus one figure per variable.
# The logger CSV (on Google Drive) covers 2026-03-01 to 2026-05-30, so every
# chamber window falls inside the log. Any window outside it would be imputed
# from the median daily cycle: the median value of each 15-min time-of-day slot
# (an earlier export started only on 2026-04-30 and needed this).
# (link in config.yml, read_greenhouse_log() in R/read_greenhouse.R)
greenhouse_tz  = "America/Los_Angeles"

# How each variable is summarised over a chamber window (summarise_in_windows()):
#   "readings":    raw readings inside the window, or the window_n_closest
#                  nearest readings when none fall inside (default)
#   "interpolate": log interpolated to 1-min steps across the window (time-weighted)
window_summary_method = "readings"
window_n_closest      = 2

# Logger variables: CSV column, output column for the window value, prefix for
# the <prefix>_source / _p10 / _p90 columns, and figure title, axis label and file
greenhouse_variables = list(
  list(column = "Indoor Temp (°C)", value = "Temp_C", prefix = "Temp",
       title = "Greenhouse indoor temperature", ylab = "Temperature (°C)",
       file = "greenhouse_temperature"),
  list(column = "Indoor Relative Humidity (%)", value = "RH_pct", prefix = "RH",
       title = "Greenhouse indoor relative humidity", ylab = "Relative humidity (%)",
       file = "greenhouse_relative_humidity"),
  list(column = "Indoor PAR Light (PAR)", value = "PAR", prefix = "PAR",
       title = "Greenhouse indoor PAR light", ylab = "PAR light",
       file = "greenhouse_PAR")
)

greenhouse_raw = read_greenhouse_log(config$data_sources$greenhouse_log, tz = greenhouse_tz)

# one time series per variable; some rows log only some sensors (blank values), drop them
greenhouse_logs = lapply(greenhouse_variables, \(v) {
  greenhouse_raw %>%
    transmute(time, value = .data[[v$column]]) %>%
    filter(!is.na(time), !is.na(value)) %>%
    arrange(time)
})

# 15-min time-of-day slot (0-95) in greenhouse local time
time_slot = function(t) {
  lt = as.POSIXlt(t, tz = greenhouse_tz)
  lt$hour * 4 + lt$min %/% 15
}

# median daily cycle; p10/p90 express day-to-day spread as imputation uncertainty
daily_profile = function(log) {
  log %>%
    mutate(slot = time_slot(time)) %>%
    group_by(slot) %>%
    summarise(median = median(value), p10 = quantile(value, 0.1), p90 = quantile(value, 0.9),
              .groups = "drop")
}
greenhouse_profiles = lapply(greenhouse_logs, daily_profile)

# Value at each chamber window's midpoint: linear interpolation of the log when
# the midpoint falls inside the logged period, otherwise the profile value.
# Columns are named <value>, <prefix>_source, <prefix>_p10, <prefix>_p90.
window_values = function(v, log, profile) {
  logged_range = range(log$time)
  metadata_joined %>%
    mutate(
      midpoint = Chamber_Start_DateTime + (Chamber_End_DateTime - Chamber_Start_DateTime) / 2,
      slot = time_slot(midpoint),
      observed = approx(log$time, log$value, xout = midpoint)$y
    ) %>%
    left_join(profile, by = "slot") %>%
    mutate(
      # compare as numbers: window and logger times may carry different tzone attributes
      source = if_else(between(as.numeric(midpoint), as.numeric(logged_range[1]), as.numeric(logged_range[2])),
                       "observed", "imputed"),
      value = if_else(source == "observed", observed, median),
      # the p10-p90 range only applies to imputed values
      p10 = if_else(source == "imputed", p10, NA_real_),
      p90 = if_else(source == "imputed", p90, NA_real_)
    ) %>%
    select(PotID, Order_Index, Condition, value, source, p10, p90) %>%
    rename(!!v$value := value, !!paste0(v$prefix, "_source") := source,
           !!paste0(v$prefix, "_p10") := p10, !!paste0(v$prefix, "_p90") := p90)
}

window_keys = c("PotID", "Order_Index", "Condition")
chamber_greenhouse = Reduce(
  \(acc, i) left_join(acc, window_values(greenhouse_variables[[i]], greenhouse_logs[[i]], greenhouse_profiles[[i]]),
                      by = window_keys),
  seq_along(greenhouse_variables),
  metadata_joined %>% select(PotID, Date, Order_Index, Condition, Chamber_Start_DateTime, Chamber_End_DateTime)
)

# mean, median and SD of each variable over each chamber window, by
# window_summary_method (summarise_in_windows() in R/measurement_windows.R)
for (i in seq_along(greenhouse_variables)) {
  chamber_greenhouse = summarise_in_windows(greenhouse_logs[[i]], chamber_greenhouse,
                                            prefix = greenhouse_variables[[i]]$prefix,
                                            method = window_summary_method,
                                            n_closest = window_n_closest)
}

# temperature for the flux calculation (05_calculate_flux.R)
# (the flux uses Temp_mean, by window_summary_method; Temp_C is the midpoint value)
chamber_temperature = chamber_greenhouse %>%
  select(PotID, Date, Order_Index, Condition, Chamber_Start_DateTime, Chamber_End_DateTime,
         Temp_C, Temp_source, Temp_p10, Temp_p90, Temp_mean, Temp_basis, Temp_max_offset_min)

# writexl drops time zones and writes UTC; format as local time text instead
writexl::write_xlsx(
  chamber_greenhouse %>%
    mutate(across(c(Chamber_Start_DateTime, Chamber_End_DateTime),
                  \(x) format(x, "%Y-%m-%d %H:%M:%S", tz = greenhouse_tz))),
  file.path(base, "output/tables/chamber_window_greenhouse.xlsx")
)

# Date range and occasion timing come from the measurement sheet
# (metadata_joined): one shaded band per measurement day, from the first chamber
# closure to the last (single 10-min windows are too narrow to see at this time
# scale), labelled with its occasion number. The plot spans the first to the
# last measurement day; the logger record outside that range is not shown.
measurement_sessions = measurement_days(metadata_joined)

occasion_labels = measurement_sessions %>%
  group_by(Order_Index) %>%
  summarise(time = min(start) + (max(end) - min(start)) / 2, .groups = "drop")

plot_range = c(as.POSIXct(paste(min(metadata_joined$Date), "00:00"), tz = greenhouse_tz),
               as.POSIXct(paste(max(metadata_joined$Date) + 1, "00:00"), tz = greenhouse_tz))

#' Plot one greenhouse variable over the measurement period
#'
#' Logged series, the imputed daily cycle over any unlogged days before the log
#' starts (none with the current log), measurement-day bands with occasion
#' numbers, and the value at each chamber window coloured by condition.
#'
#' @param v One entry of \code{greenhouse_variables}.
#' @param log,profile The variable's logged series and median daily cycle.
#' @return A ggplot object.
plot_greenhouse_variable = function(v, log, profile) {
  logged_range = range(log$time)
  imputed_times = if (plot_range[1] < logged_range[1] - 15 * 60) {
    seq(plot_range[1], logged_range[1] - 15 * 60, by = "15 min")
  } else {
    as.POSIXct(character(0), tz = greenhouse_tz)
  }
  imputed_series = data.frame(time = imputed_times) %>%
    mutate(slot = time_slot(time)) %>%
    left_join(profile, by = "slot")
  has_imputed = nrow(imputed_series) > 0

  observed_series = log %>%
    filter(between(as.numeric(time), as.numeric(plot_range[1]), as.numeric(plot_range[2])))
  windows = chamber_greenhouse %>% transmute(Chamber_Start_DateTime, Condition, value = .data[[v$value]])

  ggplot() +
    geom_rect(data = measurement_sessions,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = Session), alpha = 0.6) +
    geom_ribbon(data = imputed_series,
                aes(time, ymin = p10, ymax = p90, fill = "Imputed 10-90% range"), alpha = 0.35) +
    geom_line(data = imputed_series, aes(time, median, color = "Imputed (median daily cycle)"), linewidth = 0.3) +
    geom_line(data = observed_series, aes(time, value, color = "Observed"), linewidth = 0.3) +
    # chamber windows by condition: Light and Dark in two shades of blue
    geom_point(data = windows, aes(Chamber_Start_DateTime, value, color = Condition), size = 1) +
    geom_text(data = occasion_labels, aes(time, Inf, label = Order_Index), vjust = 1.4, size = 3, color = "grey25") +
    scale_color_manual(values = c("Observed" = "#C0392B", "Imputed (median daily cycle)" = "#555555",
                                  "Light" = "#4FA3E0", "Dark" = "#0B2E6B", "After_Watering" = "#8C8C8C"),
                       breaks = c("Observed", "Imputed (median daily cycle)", "Light", "Dark", "After_Watering"),
                       labels = c("Observed", "Imputed (median daily cycle)", "Light window", "Dark window", "After-watering window"),
                       name = NULL) +
    scale_fill_manual(values = c("Day 1 (blocks A-B)" = "#D3D7DC", "Day 2 (blocks C-D)" = "#9DA4AD",
                                 "Imputed 10-90% range" = "#9AA5B1"),
                      name = "Measurement day") +
    # legend keys: lines for the logged series, dots for the chamber windows
    guides(color = guide_legend(override.aes = list(
      linetype = c(1, if (has_imputed) 1, 0, 0, 0),
      shape = c(NA, if (has_imputed) NA, 16, 16, 16),
      size = c(NA, if (has_imputed) NA, 2.5, 2.5, 2.5)))) +
    scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d", timezone = greenhouse_tz,
                     limits = plot_range, expand = expansion(mult = 0.01)) +
    labs(title = v$title, subtitle = "Numbers: measurement occasion", x = NULL, y = v$ylab) +
    theme_bw() +
    theme(legend.position = "top", legend.title = element_text(size = rel(0.85)))
}

for (i in seq_along(greenhouse_variables)) {
  v = greenhouse_variables[[i]]
  p = plot_greenhouse_variable(v, greenhouse_logs[[i]], greenhouse_profiles[[i]])
  save_figure(file.path(base, "output/figures", paste0(v$file, ".pdf")), function() print(p), width = 17, height = 5)
}
