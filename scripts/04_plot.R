library(ggplot2)

# explicit factor order controls legend and x-axis ordering in the plot
treatment_levels = c("Conventional soil", "Organic soil", "Biochar",
                     "Biochar+SMT", "Biochar+MPSS", "Biochar+MPSS+SMT")
# named vector so scale_fill_manual maps colors consistently regardless of
# the order treatments appear in the data
treatment_colors = c(
  "Conventional soil" = "#FF0000",
  "Organic soil"      = "#0000FF",
  "Biochar"           = "#FFFF00",
  "Biochar+SMT"       = "#FF8000",
  "Biochar+MPSS"      = "#8000FF",
  "Biochar+MPSS+SMT"  = "#00FF00"
)

# pivot to long format so all five gas measurements can be mapped to facets;
# each row becomes one (pot × occasion × gas) observation
plot_data = picarro_data_averaged %>%
  select(Soil, Treatment, type, all_of(measurements), PotID, Order_Index, Block) %>%
  tidyr::pivot_longer(cols = all_of(measurements), names_to = "measurement", values_to = "value")

p = ggplot(plot_data,
           aes(x = type,
               y = value,
               fill = factor(Treatment, levels = treatment_levels, ordered = TRUE))) +
  # outlier.shape = NA suppresses duplicate outlier points already shown by geom_point
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  # jitterdodge keeps individual points aligned with their respective treatment box
  geom_point(aes(shape = Block, group = Treatment),
             position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +
  # free_y allows each gas panel to use its own y-axis scale (ranges differ widely)
  facet_wrap(~ measurement, scales = "free_y") +
  labs(x = "Type", y = "Value", fill = "Treatment", shape = "Block") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = treatment_colors)

print(p)
ggsave(file.path(base, "output/figures/boxplot_by_treatment.pdf"), p, width = 22, height = 12)
