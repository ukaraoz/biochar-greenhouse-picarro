library(goFlux)

# Closed-chamber fluxes of CO2, CH4 and N2O, one per chamber window, using
# goFlux (Rheault et al. 2024, JOSS, doi:10.21105/joss.06393). goFlux fits a
# linear model (LM) and the Hutchinson-Mosier model (HM) to each window, and
# best.flux() picks one using objective criteria incl. kappa-max (Hüppi et al.
# 2018). Flux = slope × V·P·(1 − H2O) / (R·T·A), on dry mole fractions.
# NH3 is not fitted: it adsorbs to chamber and tubing surfaces, so its
# rate of rise is not a soil flux (Picarro application note AN034).

# Chamber dimensions, copied from the "Chamber Calculation" table in the
# Biochar Greenhouse Experiment Master Document (see README Resources):
# inner diameter 28.5 cm, radius 14.25 cm, height above soil 65 cm.
# NOTE: if the pot sits inside the chamber, the emitting soil area is the pot
# surface and the air volume includes the space around the pot; these values
# assume the chamber footprint is the soil surface.
chamber_area_m2 = 0.064   # CHAMBER_AREA, π × radius²
chamber_vol_m3  = 0.041   # CHAMBER_VOL, area × height

# Pressure: standard atmospheric pressure. No chamber or ambient pressure was
# recorded; the Picarro CavityPressure column is the analyser's internal cell
# pressure and is not usable. Actual pressure can differ from standard by a few
# percent (weather, site elevation), and flux scales linearly with pressure.
chamber_pressure_kPa = 101.325

# Seconds discarded after chamber closure while the closed loop and analyser
# respond (~2 min for the G2508 closed system in AN034). Fits use the rest of
# the ~10-min window.
deadband_s = 120

# G2508 precision per gas, in goFlux units (goFlux import.G2508 defaults)
gas_precision = c(CO2dry_ppm = 0.24, CH4dry_ppb = 0.3, N2Odry_ppb = 5)

# goFlux output units: µmol m-2 s-1 for ppm gases, nmol m-2 s-1 for ppb gases
flux_units = c(CO2dry_ppm = "umol m-2 s-1", CH4dry_ppb = "nmol m-2 s-1", N2Odry_ppb = "nmol m-2 s-1")

# One row per reading in goFlux format. Units: Area cm², Vtot L, Pcham kPa,
# Tcham °C, gases as dry mole fractions. G2508 reports H2O in % (0.2-3.5 here),
# so H2O_ppm = % × 10,000; goFlux's own import.G2508 assumes mmol/mol, which
# would understate H2O tenfold, so the readings are converted here instead.
# Tcham is the greenhouse air temperature for the window: Temp_mean from
# 04_greenhouse_environment.R (window_summary_method, default "readings": mean of
# the logger readings inside the window, or the closest readings if none),
# falling back to the midpoint value Temp_C if Temp_mean is missing. Flux
# scales with 1/T (kelvin), so 1 °C changes it by ~0.34%.
flux_input = picarro_data_joined %>%
  inner_join(
    metadata_joined %>% select(PotID, Order_Index, Condition, Chamber_Start_DateTime),
    by = c("PotID", "Order_Index", "Condition")
  ) %>%
  inner_join(
    chamber_temperature %>%
      transmute(PotID, Order_Index, Condition,
                Temp_basis = if_else(is.na(Temp_mean), paste("midpoint", Temp_source), Temp_basis),
                Temp_C = coalesce(Temp_mean, Temp_C)),
    by = c("PotID", "Order_Index", "Condition")
  ) %>%
  mutate(
    UniqueID = paste(PotID, Order_Index, Condition, sep = "|"),
    Etime = as.numeric(timestamp - Chamber_Start_DateTime, units = "secs") - deadband_s,
    flag = as.numeric(Etime >= 0),
    CO2dry_ppm = CO2_dry,
    CH4dry_ppb = CH4_dry * 1000,
    N2Odry_ppb = N2O_dry * 1000,
    H2O_ppm = H2O * 10000,
    CO2_prec = gas_precision[["CO2dry_ppm"]],
    CH4_prec = gas_precision[["CH4dry_ppb"]],
    N2O_prec = gas_precision[["N2Odry_ppb"]],
    Area = chamber_area_m2 * 10000,
    Vtot = chamber_vol_m3 * 1000,
    Pcham = chamber_pressure_kPa,
    Tcham = Temp_C
  ) %>%
  arrange(UniqueID, Etime)

# window identifiers to attach back to the per-window flux results
window_info = flux_input %>%
  distinct(UniqueID, PotID, Soil, Treatment, Replicate, Block, Date, Order_Index, Condition,
           Chamber_Start_DateTime, Temp_C, Temp_basis)

flux_results = lapply(names(gas_precision), function(gastype) {
  goFlux(flux_input, gastype = gastype) %>%
    best.flux() %>%
    mutate(gas = sub("dry_pp[mb]$", "", gastype), units = flux_units[[gastype]])
}) %>%
  bind_rows() %>%
  left_join(window_info, by = "UniqueID") %>%
  select(PotID, Soil, Treatment, Replicate, Block, Date, Order_Index, Condition,
         Chamber_Start_DateTime, Temp_C, Temp_basis, gas, units,
         best.flux, model, quality.check, LM.flux, HM.flux, MDF, LM.r2, HM.r2,
         LM.p.val, HM.k, k.max, g.fact, nb.obs, C0, Ct, everything(), -UniqueID)

# Room-air flag: a window whose starting concentration is far from that day's
# typical start may have trapped unusual room air. Start = median of the first
# start_window_s after the deadband (robust to single noise spikes); flagged
# when more than room_air_mads MADs from the day's median start for that gas.
# Flag only, nothing is dropped. Meaning differs by gas: for CH4 it catches
# windows where trapped CH4-rich air leaks out (false negative fluxes, e.g.
# 2026-05-18 13:20-14:00); for CO2 high starts are common and usually harmless;
# for N2O flags are rare and often the first window of a day.
start_window_s = 30
room_air_mads  = 5
window_starts = flux_input %>%
  filter(Etime >= 0, Etime < start_window_s) %>%
  group_by(PotID, Order_Index, Condition) %>%
  summarise(across(all_of(names(gas_precision)), \(x) median(x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(all_of(names(gas_precision)), names_to = "gas", values_to = "C_start") %>%
  mutate(gas = sub("dry_pp[mb]$", "", gas))
flux_results = flux_results %>%
  left_join(window_starts, by = c("PotID", "Order_Index", "Condition", "gas")) %>%
  group_by(gas, Date) %>%
  mutate(C_start_day_median = median(C_start, na.rm = TRUE),
         C_start_offset = C_start - C_start_day_median,
         room_air_flag = abs(C_start_offset) > room_air_mads * mad(C_start, na.rm = TRUE)) %>%
  ungroup() %>%
  relocate(C_start, C_start_day_median, C_start_offset, room_air_flag, .after = quality.check)

# writexl drops time zones and writes UTC; format as local time text instead
writexl::write_xlsx(
  flux_results %>%
    mutate(Chamber_Start_DateTime = format(Chamber_Start_DateTime, "%Y-%m-%d %H:%M:%S",
                                           tz = "America/Los_Angeles")),
  file.path(base, "output/tables/flux_results.xlsx")
)
