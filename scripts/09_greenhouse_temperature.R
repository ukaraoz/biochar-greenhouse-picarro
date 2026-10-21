library(ggplot2)

# Greenhouse indoor temperature for each chamber window.
# The logger CSV (on Google Drive) only starts on 2026-04-30, after the first
# eight measurement occasions. It shows no trend over the month it covers
# (the greenhouse is climate controlled), so earlier windows are imputed from
# the median daily cycle: the median temperature of each 15-min time-of-day slot.
temperature_url = "https://drive.google.com/file/d/1eyDQzHW4PIhj3ZT2eYb9SAsSlT4buZIM"
greenhouse_tz   = "America/Los_Angeles"

# plain CSV on Drive (not a Sheet), so googledrive rather than googlesheets4;
# interactive OAuth on first run, cached afterwards
googledrive::drive_auth()
temperature_file = tempfile(fileext = ".csv")
googledrive::drive_download(googledrive::as_id(temperature_url), path = temperature_file, overwrite = TRUE)

# rows with only some sensors logged have blank temperature; drop them
greenhouse_temperature = read.csv(temperature_file, check.names = FALSE) %>%
  transmute(
    time = as.POSIXct(DateTime, format = "%a %b %d %H:%M:%S %Y", tz = greenhouse_tz),
    Temp_C = `Indoor Temp (°C)`
  ) %>%
  filter(!is.na(time), !is.na(Temp_C)) %>%
  arrange(time)

# 15-min time-of-day slot (0-95) in greenhouse local time
time_slot = function(t) {
  lt = as.POSIXlt(t, tz = greenhouse_tz)
  lt$hour * 4 + lt$min %/% 15
}

# median daily cycle; p10/p90 express day-to-day spread as imputation uncertainty
temperature_profile = greenhouse_temperature %>%
  mutate(slot = time_slot(time)) %>%
  group_by(slot) %>%
  summarise(
    Temp_median = median(Temp_C),
    Temp_p10 = quantile(Temp_C, 0.1),
    Temp_p90 = quantile(Temp_C, 0.9),
    .groups = "drop"
  )

# Temperature at each chamber window's midpoint: linear interpolation of the log
# when the midpoint falls inside the logged period, otherwise the profile value.
logged_range = range(greenhouse_temperature$time)
chamber_temperature = metadata_joined %>%
  mutate(
    midpoint = Chamber_Start_DateTime + (Chamber_End_DateTime - Chamber_Start_DateTime) / 2,
    slot = time_slot(midpoint),
    Temp_observed = approx(greenhouse_temperature$time, greenhouse_temperature$Temp_C, xout = midpoint)$y
  ) %>%
  left_join(temperature_profile, by = "slot") %>%
  mutate(
    # compare as numbers: window and logger times may carry different tzone attributes
    Temp_source = if_else(between(as.numeric(midpoint), as.numeric(logged_range[1]), as.numeric(logged_range[2])),
                          "observed", "imputed"),
    Temp_C = if_else(Temp_source == "observed", Temp_observed, Temp_median),
    # the p10-p90 range only applies to imputed values
    Temp_p10 = if_else(Temp_source == "imputed", Temp_p10, NA_real_),
    Temp_p90 = if_else(Temp_source == "imputed", Temp_p90, NA_real_)
  ) %>%
  select(PotID, Date, Order_Index, Condition, Chamber_Start_DateTime, Chamber_End_DateTime,
         Temp_C, Temp_source, Temp_p10, Temp_p90)

# writexl drops time zones and writes UTC; format as local time text instead
writexl::write_xlsx(
  chamber_temperature %>%
    mutate(across(c(Chamber_Start_DateTime, Chamber_End_DateTime),
                  \(x) format(x, "%Y-%m-%d %H:%M:%S", tz = greenhouse_tz))),
  file.path(base, "output/tables/chamber_window_temperature.xlsx")
)

# observed series plus the imputed daily cycle repeated over the unlogged days
imputed_series = data.frame(
  time = seq(as.POSIXct(paste(min(metadata_joined$Date), "00:00"), tz = greenhouse_tz),
             logged_range[1] - 15 * 60, by = "15 min")
) %>%
  mutate(slot = time_slot(time)) %>%
  left_join(temperature_profile, by = "slot")

# one shaded band per measurement day, from the first chamber closure to the
# last; single 10-min windows are too narrow to see at this time scale
measurement_sessions = chamber_temperature %>%
  group_by(Date) %>%
  summarise(start = min(Chamber_Start_DateTime), end = max(Chamber_End_DateTime), .groups = "drop")

p = ggplot() +
  geom_rect(data = measurement_sessions,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = "Gas measurement session"), alpha = 0.3) +
  geom_ribbon(data = imputed_series,
              aes(time, ymin = Temp_p10, ymax = Temp_p90, fill = "Imputed 10-90% range"), alpha = 0.35) +
  geom_line(data = imputed_series, aes(time, Temp_median, color = "Imputed (median daily cycle)"), linewidth = 0.3) +
  geom_line(data = greenhouse_temperature, aes(time, Temp_C, color = "Observed"), linewidth = 0.3) +
  geom_point(data = chamber_temperature,
             aes(Chamber_Start_DateTime, Temp_C, shape = "Chamber window"), size = 0.8) +
  scale_color_manual(values = c("Observed" = "#C0392B", "Imputed (median daily cycle)" = "#555555"), name = NULL) +
  scale_fill_manual(values = c("Gas measurement session" = "#2080E0", "Imputed 10-90% range" = "#9AA5B1"), name = NULL) +
  scale_shape_manual(values = 16, name = NULL) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d", timezone = greenhouse_tz) +
  labs(title = "Greenhouse indoor temperature: observed and imputed",
       x = NULL, y = "Temperature (°C)") +
  theme_bw() +
  theme(legend.position = "top")

ggsave(file.path(base, "output/figures/greenhouse_temperature.pdf"), p, width = 13, height = 5)
