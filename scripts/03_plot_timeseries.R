library(ggplot2)

# Raw (non-averaged) readings over each chamber window, replicates overlaid.
# Replicates are measured at different clock times, so time is expressed as
# minutes since the window's first reading; each condition is then shifted
# into its own slot on the x-axis (Light, Dark, After watering) so the three
# windows of one occasion sit side by side.
condition_slots  = c("Light" = 0, "Dark" = 1, "After_Watering" = 2)
condition_labels = c("Light", "Dark", "After watering")
slot_width = 12  # minutes; windows are ~10 min (max ~12)

timeseries_data = picarro_data_joined %>%
  group_by(PotID, Order_Index, Condition) %>%
  mutate(minutes = as.numeric(timestamp - min(timestamp), units = "mins")) %>%
  ungroup() %>%
  mutate(
    x = condition_slots[as.character(Condition)] * slot_width + minutes,
    Treatment = factor(Treatment, levels = treatment_levels),
    Order_Index = factor(Order_Index, levels = 1:15),
    Replicate = factor(Replicate)
  )

# shaded background per condition slot; labels go on the x-axis at slot centres
slot_bands = data.frame(
  xmin = condition_slots * slot_width,
  xmax = (condition_slots + 1) * slot_width,
  Condition = condition_labels
)

# one page per gas × treatment; panels = measurement occasion (Order_Index)
save_figure(file.path(base, "output/figures/timeseries_by_replicate.pdf"), function() {
  for (gas in measurements) {
    for (trt in treatment_levels) {
      p = ggplot(filter(timeseries_data, Treatment == trt),
                 aes(x = x, y = .data[[gas]], color = Replicate,
                     group = interaction(Replicate, Condition))) +
        geom_rect(data = slot_bands,
                  aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Condition),
                  inherit.aes = FALSE, alpha = 0.15) +
        geom_line(linewidth = 0.4) +
        facet_wrap(~ Order_Index, ncol = 5, scales = "free_y",
                   labeller = labeller(Order_Index = function(x) paste("Occasion", x))) +
        scale_x_continuous(breaks = (condition_slots + 0.5) * slot_width,
                           labels = condition_labels, expand = c(0, 0)) +
        scale_fill_manual(values = c("Light" = "#F0C000", "Dark" = "#404040",
                                     "After watering" = "#2080E0"),
                          breaks = condition_labels, guide = "none") +
        scale_color_brewer(palette = "Dark2") +
        labs(title = paste0(gas, " — ", treatment_labels[[trt]]),
             x = paste0("Condition (each range = ", slot_width, " min from chamber closure)"),
             y = gas, color = "Replicate") +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
      print(p)
    }
  }
}, width = 16, height = 10)
