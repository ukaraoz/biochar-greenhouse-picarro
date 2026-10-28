library(ggplot2)

# Partition CO2 exchange per pot and occasion from the paired chamber windows
# (fluxes from 10_calculate_flux.R, µmol CO2 m-2 s-1, positive = to the air):
#   NEE = Light flux (net ecosystem exchange; negative = net uptake)
#   ER  = Dark flux  (ecosystem respiration: shoots, roots, soil microbes)
#   GPP = ER - NEE   (gross primary production; CO2 fixed by photosynthesis)
# The After_Watering window is also in the light, so it gives NEE after
# watering only; there is no dark window after watering, so GPP and ER cannot
# be partitioned there (re-wetting can raise soil respiration).
# GPP assumes respiration is the same in light and dark; leaf respiration is
# lower in light, so GPP is approximate.
light_dark_max_gap_min = 5   # the Dark window normally starts < 1 min after Light ends

co2_windows = flux_results %>%
  filter(gas == "CO2") %>%
  select(PotID, Soil, Treatment, Replicate, Block, Date, Order_Index, Condition,
         Chamber_Start_DateTime, best.flux, MDF, room_air_flag, Temp_C)

co2_wide = co2_windows %>%
  select(PotID, Order_Index, Condition, Chamber_Start_DateTime, best.flux, MDF, room_air_flag, Temp_C) %>%
  tidyr::pivot_wider(names_from = Condition,
                     values_from = c(Chamber_Start_DateTime, best.flux, MDF, room_air_flag, Temp_C))

co2_partitioning = co2_windows %>%
  distinct(PotID, Soil, Treatment, Replicate, Block, Order_Index) %>%
  left_join(co2_windows %>% filter(Condition == "Light") %>% distinct(PotID, Order_Index, Date),
            by = c("PotID", "Order_Index")) %>%
  left_join(co2_wide, by = c("PotID", "Order_Index")) %>%
  mutate(
    NEE = best.flux_Light,
    ER = best.flux_Dark,
    GPP = ER - NEE,
    NEE_after_watering = best.flux_After_Watering,
    Light_to_Dark_min = round(as.numeric(Chamber_Start_DateTime_Dark - Chamber_Start_DateTime_Light, units = "mins"), 1),
    # flags for review; nothing is removed
    flag_missing_window = is.na(NEE) | is.na(ER),
    flag_negative_ER = !is.na(ER) & ER < 0,                     # respiration cannot be uptake
    flag_negative_GPP = !is.na(GPP) & GPP < 0,                  # photosynthesis cannot release CO2
    flag_room_air = coalesce(room_air_flag_Light, FALSE) | coalesce(room_air_flag_Dark, FALSE),
    flag_window_timing = !is.na(Light_to_Dark_min) &
      (Light_to_Dark_min < 0 | Light_to_Dark_min > 10 + light_dark_max_gap_min),
    flag_below_MDF = (!is.na(NEE) & abs(NEE) < MDF_Light) | (!is.na(ER) & abs(ER) < MDF_Dark)
  ) %>%
  select(PotID, Soil, Treatment, Replicate, Block, Date, Order_Index,
         NEE, ER, GPP, NEE_after_watering,
         Temp_C_Light, Temp_C_Dark, Light_to_Dark_min, starts_with("flag_")) %>%
  arrange(Treatment, Replicate, Order_Index)

# treatment means per occasion (pots with both windows)
co2_partitioning_summary = co2_partitioning %>%
  tidyr::pivot_longer(c(NEE, ER, GPP, NEE_after_watering), names_to = "quantity", values_to = "flux") %>%
  filter(!is.na(flux)) %>%
  group_by(Treatment, Order_Index, quantity) %>%
  summarise(mean = mean(flux), sd = sd(flux), n = n(), .groups = "drop") %>%
  arrange(quantity, Treatment, Order_Index)

# --- Total gas exchange over the experiment ----------------------------------
# Fluxes are 10-min daytime snapshots, so totals need a daily scaling:
# photosynthesis runs only in the photoperiod (protocol: 14-16 h light), while
# respiration runs all day. Daily rates are integrated over time per pot with
# the trapezoid rule between measurement dates, from first to last occasion.
# Assumes the measured rates hold for the whole day (no diel change in light or
# temperature), so totals are estimates for comparing treatments.
photoperiod_h = 15
umol_co2_to_g_c = 12.011e-6   # g C per µmol CO2

trapezoid_cumulative = function(date, rate) {
  days = as.numeric(date)
  c(0, cumsum(diff(days) * (head(rate, -1) + tail(rate, -1)) / 2))
}

co2_cumulative = co2_partitioning %>%
  filter(!is.na(GPP)) %>%
  mutate(Date = as.Date(Date),
         GPP_daily = GPP * photoperiod_h * 3600 * umol_co2_to_g_c,
         ER_daily = ER * 24 * 3600 * umol_co2_to_g_c,
         NEE_daily = ER_daily - GPP_daily) %>%
  arrange(PotID, Date) %>%
  group_by(PotID) %>%
  mutate(GPP_cumulative = trapezoid_cumulative(Date, GPP_daily),
         ER_cumulative = trapezoid_cumulative(Date, ER_daily),
         NEE_cumulative = trapezoid_cumulative(Date, NEE_daily)) %>%
  ungroup()

# end-point totals per pot (g C m-2 over the measurement period)
co2_totals = co2_cumulative %>%
  group_by(PotID, Soil, Treatment, Replicate, Block) %>%
  summarise(First_date = min(Date), Last_date = max(Date), Occasions = n(),
            GPP_total = last(GPP_cumulative), ER_total = last(ER_cumulative),
            NEE_total = last(NEE_cumulative), .groups = "drop")

co2_totals_long = co2_totals %>%
  tidyr::pivot_longer(c(GPP_total, ER_total, NEE_total), names_to = "quantity", values_to = "total") %>%
  mutate(quantity = sub("_total", "", quantity),
         Treatment = factor(Treatment, levels = treatment_levels))

co2_totals_means = co2_totals_long %>%
  group_by(quantity, Treatment) %>%
  summarise(mean = mean(total), sd = sd(total), se = sd / sqrt(n()), n = n(), .groups = "drop")

# Treatment comparison of the totals: Treatment + Block model (blocks were
# measured on different days), Tukey-adjusted pairwise comparisons
co2_totals_pairwise = co2_totals_long %>%
  group_by(quantity) %>%
  group_modify(\(d, key) {
    fit = lm(total ~ Treatment + Block, data = d)
    as.data.frame(summary(pairs(emmeans::emmeans(fit, "Treatment"), adjust = "tukey")))
  }) %>%
  ungroup()

writexl::write_xlsx(
  list(per_pot = co2_partitioning, treatment_means = co2_partitioning_summary,
       totals_per_pot = co2_totals, totals_means = co2_totals_means,
       totals_pairwise = co2_totals_pairwise),
  file.path(base, "output/tables/CO2_partitioning.xlsx")
)

# GPP, ER and NEE over the experiment: faint lines per pot, bold treatment mean
partition_colors = c(GPP = "#2f8a3e", ER = "#c2551f", NEE = "#3a5fa0")
plot_partitioning = co2_partitioning %>%
  tidyr::pivot_longer(c(GPP, ER, NEE), names_to = "quantity", values_to = "flux") %>%
  filter(!is.na(flux)) %>%
  mutate(quantity = factor(quantity, levels = names(partition_colors)),
         Treatment = factor(Treatment, levels = treatment_levels))

p = ggplot(plot_partitioning, aes(Order_Index, flux, color = quantity)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_line(aes(group = interaction(PotID, quantity)), alpha = 0.25, linewidth = 0.4) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
  stat_summary(fun = mean, geom = "point", size = 1.4) +
  facet_wrap(~ Treatment, ncol = 3) +
  scale_color_manual(values = partition_colors,
                     labels = c(GPP = "GPP (photosynthesis)", ER = "ER (respiration, dark)", NEE = "NEE (net, light)"),
                     name = NULL) +
  scale_x_continuous(breaks = 1:15) +
  labs(x = "Occasion", y = expression(CO[2]~flux~(mu*mol~m^{-2}~s^{-1}))) +
  theme_bw() +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(file.path(base, "output/figures/CO2_partitioning_by_occasion.pdf"), p, width = 13, height = 8)

# Cumulative exchange over time and end-point totals by treatment
plot_cumulative = co2_cumulative %>%
  tidyr::pivot_longer(c(GPP_cumulative, ER_cumulative, NEE_cumulative), names_to = "quantity", values_to = "cumulative") %>%
  mutate(quantity = factor(sub("_cumulative", "", quantity), levels = names(partition_colors)),
         Treatment = factor(Treatment, levels = treatment_levels))

p_cumulative = ggplot(plot_cumulative, aes(Date, cumulative, color = quantity)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_line(aes(group = interaction(PotID, quantity)), linewidth = 0.5, alpha = 0.6) +
  facet_wrap(~ Treatment, ncol = 3) +
  scale_color_manual(values = partition_colors, name = NULL,
                     labels = c(GPP = "GPP (fixed)", ER = "ER (respired)", NEE = "NEE (net; negative = uptake)")) +
  labs(x = NULL, y = expression(Cumulative~CO[2]~exchange~(g~C~m^{-2}))) +
  theme_bw() +
  theme(legend.position = "top", panel.grid.minor = element_blank())

p_totals = ggplot(co2_totals_long %>% mutate(quantity = factor(quantity, levels = names(partition_colors))),
                  aes(Treatment, total, color = quantity)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_point(position = position_jitter(width = 0.12, height = 0, seed = 1), size = 1.8, alpha = 0.6) +
  stat_summary(fun.data = mean_se, geom = "pointrange", color = "black", size = 0.4) +
  facet_wrap(~ quantity, scales = "free_y",
             labeller = labeller(quantity = c(GPP = "GPP (fixed)", ER = "ER (respired)", NEE = "NEE (net; negative = uptake)"))) +
  scale_color_manual(values = partition_colors, guide = "none") +
  labs(x = NULL, y = expression(Total~over~experiment~(g~C~m^{-2}))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid.minor = element_blank())

pdf(file.path(base, "output/figures/CO2_partitioning_cumulative.pdf"), width = 13, height = 8)
print(p_cumulative)
print(p_totals)
dev.off()
