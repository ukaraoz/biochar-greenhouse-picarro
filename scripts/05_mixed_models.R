library(nlme)
library(emmeans)

# Pairwise repeated-measures linear mixed models: one model per
# measurement type × gas × treatment comparison, each fitted on only the
# 8 pots (2 treatments × 4 replicates) of that comparison.
#
# Each pot is measured on 15 occasions; observations from the same pot closer
# together in time are expected to be more alike. The model accounts for this
# with a continuous-time AR(1) correlation on the window timestamp, since
# occasions are unevenly spaced (3–7 days apart, and blocks C/D are measured
# the day after blocks A/B).

# each comparison is c(reference, test); direction is reported as test vs reference
comparisons = list(
  c("Conv_Ctrl", "Org_Ctrl"),
  c("Conv_Ctrl", "Conv_BC"),
  c("Conv_BC",   "Conv_BC-SMT"),
  c("Conv_BC",   "Conv_BC-MPSS"),
  c("Conv_BC",   "Conv_BC-MPSS-SMT")
)

# timestamp of each chamber window (mid-point of its readings), as numeric
# seconds since epoch for corCAR1; Order_Index alone does not carry the real
# spacing between visits
window_times = picarro_data_joined %>%
  group_by(PotID, Order_Index, type) %>%
  summarise(window_time = mean(timestamp), .groups = "drop") %>%
  mutate(time = as.numeric(window_time),
         Order_Index = as.character(Order_Index))

# one row per chamber window (the per-window means from 03_average.R);
# TreatmentCode is the PotID prefix, e.g. "Conv_BC-MPSS" from "Conv_BC-MPSS_3_C"
model_data = picarro_data_averaged %>%
  mutate(Order_Index = as.character(Order_Index), type = as.character(type)) %>%
  left_join(window_times, by = c("PotID", "Order_Index", "type")) %>%
  mutate(
    TreatmentCode = sub("_[0-9]+_[A-D]$", "", PotID),
    Occasion      = factor(as.integer(Order_Index)),
    Replicate     = factor(Replicate),
    PotID         = factor(PotID)
  )

types = c("Light", "CO2_Fixation", "After_Watering")

# Fixed:  Treatment × Occasion — occasion means absorb conditions shared by all
#         pots on a visit; the interaction tests whether the treatment
#         difference changes over time.
# Random: Replicate (= Block) and PotID nested within it.
# Residual correlation: corCAR1 on the window timestamp within each pot. Time
#         is in seconds, so phi (correlation per second) must start near 1;
#         nlme's default start (0.2) means no correlation across days and the
#         optimizer never moves it. Start at a one-day correlation of 0.5.
fit_comparison_model = function(gas, type_name, ref, test) {
  d = model_data %>%
    filter(type == type_name, TreatmentCode %in% c(ref, test)) %>%
    mutate(Treatment = factor(TreatmentCode, levels = c(ref, test)))
  lme(as.formula(paste(gas, "~ Treatment * Occasion")),
      random      = ~ 1 | Replicate/PotID,
      correlation = corCAR1(value = 0.5^(1 / 86400), form = ~ time | Replicate/PotID),
      data        = d,
      method      = "REML",
      contrasts   = list(Treatment = "contr.sum", Occasion = "contr.sum"),
      control     = lmeControl(maxIter = 200, msMaxIter = 200))
}

model_grid = tidyr::expand_grid(
  type = types,
  gas  = measurements,
  tibble::tibble(ref = sapply(comparisons, `[`, 1), test = sapply(comparisons, `[`, 2))
) %>%
  mutate(comparison = paste(test, "vs", ref),
         model_id   = paste(type, gas, comparison, sep = " | "))

# a failed fit is recorded as NULL rather than stopping the whole run
models = setNames(lapply(seq_len(nrow(model_grid)), function(i) {
  g = model_grid[i, ]
  tryCatch(fit_comparison_model(g$gas, g$type, g$ref, g$test),
           error = function(e) {
             message("Fit failed: ", g$model_id, ": ", conditionMessage(e))
             NULL
           })
}), model_grid$model_id)

# Per model: Type III F-tests, the test − reference difference averaged over
# occasions (with 95% CI), and the variance / correlation parameters
results = bind_rows(lapply(seq_len(nrow(model_grid)), function(i) {
  g = model_grid[i, ]
  m = models[[i]]
  if (is.null(m)) return(mutate(g, fit_ok = FALSE))

  a     = anova(m, type = "marginal")
  emm   = suppressMessages(emmeans(m, ~ Treatment, mode = "containment"))
  means = as.data.frame(emm)
  diff  = as.data.frame(confint(contrast(emm, list(test_minus_ref = c(-1, 1)))))
  # reStruct stores each level's variance relative to the residual variance
  re_sd = sapply(m$modelStruct$reStruct, \(x) sqrt(as.matrix(x)[1, 1]) * m$sigma)
  phi   = coef(m$modelStruct$corStruct, unconstrained = FALSE)

  mutate(g,
    fit_ok         = TRUE,
    mean_ref       = means$emmean[1],
    mean_test      = means$emmean[2],
    difference     = diff$estimate,
    difference_SE  = diff$SE,
    df             = diff$df,
    CI_lower       = diff$lower.CL,
    CI_upper       = diff$upper.CL,
    percent_change = 100 * diff$estimate / means$emmean[1],
    p_treatment    = a["Treatment", "p-value"],
    p_occasion     = a["Occasion", "p-value"],
    p_interaction  = a["Treatment:Occasion", "p-value"],
    # direction of the test treatment relative to the reference
    direction      = if_else(difference > 0, "higher", "lower"),
    significant    = p_treatment < 0.05,
    sd_Replicate   = re_sd[["Replicate"]],
    sd_PotID       = re_sd[["PotID"]],
    sd_Residual    = m$sigma,
    # phi is per one-second lag; shown as the correlation at a one-day lag
    corr_1day      = phi^86400
  )
}))

# compact view: one row per type × gas × comparison
summary_table = results %>%
  select(type, gas, comparison, mean_ref, mean_test, difference, CI_lower, CI_upper,
         percent_change, p_treatment, direction, significant, p_interaction)

writexl::write_xlsx(
  list(summary = summary_table, full_results = select(results, -model_id)),
  file.path(base, "output/tables/mixed_model_results.xlsx")
)

# residual diagnostics: one page per type × comparison, one row per gas;
# normalized residuals account for the CAR1 correlation
pdf(file.path(base, "output/figures/mixed_model_diagnostics.pdf"), width = 10, height = 4 * length(measurements))
for (type_name in types) {
  for (cmp in unique(model_grid$comparison)) {
    par(mfrow = c(length(measurements), 2), oma = c(0, 0, 2, 0))
    for (gas in measurements) {
      m = models[[paste(type_name, gas, cmp, sep = " | ")]]
      if (is.null(m)) { plot.new(); plot.new(); next }
      r = resid(m, type = "normalized")
      plot(fitted(m), r, main = paste(gas, "- residuals vs fitted"),
           xlab = "Fitted", ylab = "Normalized residual")
      abline(h = 0, lty = 2)
      qqnorm(r, main = paste(gas, "- normal Q-Q")); qqline(r)
    }
    mtext(paste(type_name, "|", cmp), outer = TRUE, cex = 1.2)
  }
}
dev.off()
