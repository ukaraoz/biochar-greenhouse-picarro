library(ggplot2)

# Flux-fit diagnostics: dry mole fraction vs time since chamber closure, with
# the fitted model, for every window. One figure per gas × treatment: rows are
# occasions, columns are conditions, the four replicates overlaid. Written as
# 300-dpi PNGs to output/figures/flux_fits/. Uses flux_input, flux_results and
# deadband_s from 05_calculate_flux.R.
condition_labels = c("Light" = "Light", "Dark" = "Dark",
                     "After_Watering" = "After watering")
gas_columns = c(CO2 = "CO2dry_ppm", CH4 = "CH4dry_ppb", N2O = "N2Odry_ppb")
gas_axis_labels = c(CO2 = "CO2 dry mole fraction (ppm)", CH4 = "CH4 dry mole fraction (ppb)",
                    N2O = "N2O dry mole fraction (ppb)")

# readings: t = seconds since closure; Etime is measured from the end of the deadband
fit_readings = flux_input %>%
  mutate(t = Etime + deadband_s,
         Replicate = factor(Replicate),
         Condition = factor(condition_labels[Condition], levels = condition_labels))

fit_results = flux_results %>%
  mutate(Replicate = factor(Replicate),
         Condition = factor(condition_labels[Condition], levels = condition_labels))

# chosen model evaluated over the fitted part of each window:
# LM: C0 + slope·t; HM: Ci + (C0 − Ci)·exp(−k·t)
fit_curves = fit_results %>%
  inner_join(
    fit_readings %>% filter(flag == 1) %>%
      group_by(PotID, Order_Index, Condition) %>%
      summarise(t_end = max(Etime), .groups = "drop"),
    by = c("PotID", "Order_Index", "Condition")
  ) %>%
  rowwise() %>%
  reframe(PotID, Order_Index, Treatment, Replicate, Condition, gas,
          model, LM.C0, LM.slope, HM.C0, HM.Ci, HM.k,
          te = seq(0, t_end, length.out = 60)) %>%
  mutate(t = te + deadband_s,
         C = if_else(model == "LM", LM.C0 + LM.slope * te, HM.Ci + (HM.C0 - HM.Ci) * exp(-HM.k * te)))

# one label line per replicate, stacked in the panel's top-left corner;
# "*" marks fluxes below the minimal detectable flux (MDF)
fit_labels = fit_results %>%
  mutate(label = sprintf("R%s: %.3g %s%s", Replicate, best.flux, model,
                         if_else(abs(best.flux) < MDF, "*", "")),
         vjust = 1.4 * as.integer(Replicate))

plot_flux_fit = function(gas_name, trt) {
  gas_col = gas_columns[[gas_name]]
  # Each panel's y-axis follows the fitted (post-deadband) readings; readings
  # beyond that range, mostly closure spikes inside the deadband, are pinned to
  # the panel edge so they stay visible without flattening the fitted part.
  readings = fit_readings %>%
    filter(Treatment == trt) %>%
    mutate(y = .data[[gas_col]]) %>%
    group_by(Order_Index, Condition) %>%
    mutate(y_lo = min(y[flag == 1], na.rm = TRUE),
           y_hi = max(y[flag == 1], na.rm = TRUE),
           pad = 0.1 * (y_hi - y_lo),
           y = pmin(pmax(y, y_lo - pad), y_hi + pad)) %>%
    ungroup()
  curves = filter(fit_curves, gas == gas_name, Treatment == trt)
  labels = filter(fit_labels, gas == gas_name, Treatment == trt)
  ggplot(readings, aes(t, y, color = Replicate)) +
    annotate("rect", xmin = 0, xmax = deadband_s, ymin = -Inf, ymax = Inf, fill = "grey88") +
    geom_point(size = 0.15, alpha = 0.35) +
    geom_line(data = curves, aes(t, C, group = Replicate), linewidth = 0.6) +
    geom_text(data = labels, aes(x = -Inf, y = Inf, label = label, vjust = vjust),
              hjust = -0.04, size = 2.6, show.legend = FALSE) +
    # independent y-axis per panel: Light and Dark ranges differ within an occasion
    ggh4x::facet_grid2(Order_Index ~ Condition, scales = "free_y", independent = "y",
                       labeller = labeller(Order_Index = function(o) paste("Occ", o))) +
    scale_color_brewer(palette = "Dark2", drop = FALSE) +
    # headroom at the top for the replicate flux labels
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.6)), n.breaks = 3) +
    guides(color = guide_legend(override.aes = list(size = 2, alpha = 1, linewidth = 1))) +
    labs(title = sprintf("%s flux fits: %s", gas_name, trt),
         subtitle = sprintf("Lines: chosen model (LM linear, HM Hutchinson-Mosier). Labels: flux in %s; * below MDF (minimal detectable flux).",
                            filter(fit_results, gas == gas_name)$units[1]),
         x = "Seconds since chamber closure", y = gas_axis_labels[[gas_name]], color = "Replicate") +
    theme_bw(base_size = 11) +
    theme(legend.position = "top", panel.spacing.y = unit(3, "pt"))
}

flux_fits_dir = file.path(base, "output/figures/flux_fits")
dir.create(flux_fits_dir, showWarnings = FALSE)
for (gas_name in names(gas_columns)) {
  for (trt in treatment_levels) {
    ggsave(file.path(flux_fits_dir, sprintf("flux_fits_%s_%s.png", gas_name, gsub(" ", "_", trt))),
           plot_flux_fit(gas_name, trt), width = 16, height = 30, dpi = 300, limitsize = FALSE)
  }
}
