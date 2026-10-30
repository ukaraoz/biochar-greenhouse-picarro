library(ggplot2)

# Partition CO2 exchange per pot and occasion from the paired chamber windows
# (fluxes from 05_calculate_flux.R, µmol CO2 m-2 s-1, positive = to the air):
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

# --- Variance explained by occasion after correcting for the environment -----
# How much of the variation in GPP, ER and NEE across pots and occasions is
# explained by the occasion (plant growth, watering cycle, day-to-day changes)
# once greenhouse temperature, PAR and relative humidity are accounted for.
# Each quantity gets the conditions of the windows it comes from: NEE the Light
# window, ER the Dark window, GPP the mean of both (window means from
# 04_greenhouse_environment.R, window_summary_method).
#   R2_* / dR2_*: linear models; dR2 = gain in R2 when a term is added, so
#     shares depend on what is already in the model (occasion and environment
#     overlap, and within a day the environment overlaps with treatment because
#     pots are always measured in the same order)
#   mixed model: pot as a random intercept (repeated measures); likelihood-ratio
#     test for occasion and its gain in marginal R2 (Nakagawa & Schielzeth 2013).
#     The pot variance is reported; it is ~0 here (pots differ no further once
#     treatment and block are in), so lme4 notes a singular fit, which is
#     silenced, and the mixed results match the linear models.
window_environment = chamber_greenhouse %>%
  filter(Condition %in% c("Light", "Dark")) %>%
  select(PotID, Order_Index, Condition, Temp = Temp_mean, PAR = PAR_mean, RH = RH_mean) %>%
  tidyr::pivot_wider(names_from = Condition, values_from = c(Temp, PAR, RH))

variance_data = co2_partitioning %>%
  select(PotID, Treatment, Block, Order_Index, GPP, ER, NEE) %>%
  left_join(window_environment, by = c("PotID", "Order_Index")) %>%
  mutate(Occasion = factor(Order_Index), Treatment = factor(Treatment), Block = factor(Block),
         Temp_Both = (Temp_Light + Temp_Dark) / 2, PAR_Both = (PAR_Light + PAR_Dark) / 2,
         RH_Both = (RH_Light + RH_Dark) / 2)
environment_window = c(NEE = "Light", ER = "Dark", GPP = "Both")

marginal_r2 = function(fit) {
  var_fixed = var(predict(fit, re.form = NA))
  var_fixed / (var_fixed + sum(as.data.frame(lme4::VarCorr(fit))$vcov))
}

variance_partition = bind_rows(lapply(names(environment_window), \(quantity) {
  w = environment_window[[quantity]]
  d = variance_data %>%
    transmute(y = .data[[quantity]], Temp = .data[[paste0("Temp_", w)]], PAR = .data[[paste0("PAR_", w)]],
              RH = .data[[paste0("RH_", w)]], Occasion, Treatment, Block, PotID) %>%
    filter(complete.cases(.))
  r2 = function(terms) if (terms == "") 0 else summary(lm(as.formula(paste("y ~", terms)), d))$r.squared
  env = "Temp + PAR + RH"
  base_terms = paste(env, "+ Treatment + Block")
  fit_mixed = \(terms) suppressMessages(lme4::lmer(as.formula(paste("y ~", terms, "+ (1 | PotID)")), d, REML = FALSE))
  full = fit_mixed(paste(base_terms, "+ Occasion"))
  no_occasion = fit_mixed(base_terms)
  lrt = anova(no_occasion, full)
  data.frame(
    quantity, environment_window = w, n = nrow(d),
    R2_occasion_alone = r2("Occasion"),
    R2_environment_alone = r2(env),
    dR2_occasion_after_environment = r2(paste(env, "+ Occasion")) - r2(env),
    dR2_occasion_after_environment_treatment_block = r2(paste(base_terms, "+ Occasion")) - r2(base_terms),
    dR2_environment_after_occasion_treatment_block = r2(paste(base_terms, "+ Occasion")) - r2("Treatment + Block + Occasion"),
    dR2_treatment_after_environment_occasion_block = r2(paste(base_terms, "+ Occasion")) - r2(paste(env, "+ Block + Occasion")),
    R2_full = r2(paste(base_terms, "+ Occasion")),
    mixed_pot_variance = as.data.frame(lme4::VarCorr(full))$vcov[1],
    mixed_residual_variance = sigma(full)^2,
    mixed_marginal_R2_full = marginal_r2(full),
    mixed_dR2m_occasion = marginal_r2(full) - marginal_r2(no_occasion),
    mixed_LRT_chisq_occasion = lrt$Chisq[2], mixed_LRT_df = lrt$Df[2], mixed_LRT_p_occasion = lrt$`Pr(>Chisq)`[2]
  )
}))

writexl::write_xlsx(
  list(per_pot = co2_partitioning, treatment_means = co2_partitioning_summary,
       totals_per_pot = co2_totals, totals_means = co2_totals_means,
       totals_pairwise = co2_totals_pairwise, variance_partition = variance_partition),
  file.path(base, "output/tables/CO2_partitioning.xlsx")
)

# GPP, ER and NEE over the experiment, with the greenhouse conditions during
# the same windows underneath: one column per treatment; rows for the CO2
# fluxes, temperature, PAR and relative humidity. Faint lines per pot, bold
# treatment mean. Conditions are the window means (window_summary_method, from
# 04_greenhouse_environment.R) averaged over each pot's Light and Dark windows,
# the two windows GPP is computed from.
partition_colors = c(GPP = "#2f8a3e", ER = "#c2551f", NEE = "#3a5fa0")
environment_colors = c(Temperature = "#B03A2E", PAR = "#B7950B", RH = "#2874A6")
panel_labels = c(flux = "CO\u2082 flux (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)", Temperature = "Temperature (\u00b0C)",
                 PAR = "PAR light", RH = "Relative humidity (%)")

plot_partitioning = co2_partitioning %>%
  tidyr::pivot_longer(c(GPP, ER, NEE), names_to = "series", values_to = "value") %>%
  filter(!is.na(value)) %>%
  transmute(PotID, Treatment, Order_Index, series, value, panel = "flux")

plot_environment = chamber_greenhouse %>%
  filter(Condition %in% c("Light", "Dark")) %>%
  group_by(PotID, Order_Index) %>%
  summarise(Temperature = mean(Temp_mean), PAR = mean(PAR_mean), RH = mean(RH_mean), .groups = "drop") %>%
  left_join(distinct(co2_partitioning, PotID, Treatment), by = "PotID") %>%
  tidyr::pivot_longer(c(Temperature, PAR, RH), names_to = "series", values_to = "value") %>%
  mutate(panel = series)

plot_occasion = bind_rows(plot_partitioning, plot_environment) %>%
  mutate(series = factor(series, levels = c(names(partition_colors), names(environment_colors))),
         panel = factor(panel, levels = names(panel_labels), labels = panel_labels),
         Treatment = factor(Treatment, levels = treatment_levels))

p = ggplot(plot_occasion, aes(Order_Index, value, color = series)) +
  geom_hline(data = data.frame(panel = factor(panel_labels[["flux"]], levels = panel_labels), yintercept = 0),
             aes(yintercept = yintercept), linewidth = 0.3, color = "grey50") +
  geom_line(aes(group = interaction(PotID, series)), alpha = 0.25, linewidth = 0.4) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
  stat_summary(fun = mean, geom = "point", size = 1.4) +
  facet_grid(panel ~ Treatment, scales = "free_y", switch = "y",
             labeller = labeller(Treatment = treatment_labels_wrapped)) +
  ggh4x::force_panelsizes(rows = c(2.2, 1, 1, 1)) +
  scale_color_manual(values = c(partition_colors, environment_colors),
                     breaks = names(partition_colors),
                     labels = c(GPP = "GPP (photosynthesis)", ER = "ER (respiration, dark)", NEE = "NEE (net, light)"),
                     name = NULL) +
  scale_x_continuous(breaks = 1:15) +
  labs(x = "Occasion", y = NULL) +
  theme_bw() +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.placement = "outside", strip.background.y = element_blank(), strip.text.y.left = element_text(angle = 90))

save_figure(file.path(base, "output/figures/CO2_partitioning_by_occasion.pdf"), function() print(p),
            width = 18, height = 11, pdf_device = grDevices::cairo_pdf)

# Cumulative exchange over time and end-point totals by treatment
plot_cumulative = co2_cumulative %>%
  tidyr::pivot_longer(c(GPP_cumulative, ER_cumulative, NEE_cumulative), names_to = "quantity", values_to = "cumulative") %>%
  mutate(quantity = factor(sub("_cumulative", "", quantity), levels = names(partition_colors)),
         Treatment = factor(Treatment, levels = treatment_levels))

p_cumulative = ggplot(plot_cumulative, aes(Date, cumulative, color = quantity)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_line(aes(group = interaction(PotID, quantity)), linewidth = 0.5, alpha = 0.6) +
  facet_wrap(~ Treatment, ncol = 3, labeller = labeller(Treatment = treatment_labels)) +
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
  scale_x_discrete(labels = treatment_labels_wrapped) +
  labs(x = NULL, y = expression(Total~over~experiment~(g~C~m^{-2}))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = rel(0.8)), panel.grid.minor = element_blank())

save_figure(file.path(base, "output/figures/CO2_partitioning_cumulative.pdf"),
            function() { print(p_cumulative); print(p_totals) }, width = 13, height = 8)
