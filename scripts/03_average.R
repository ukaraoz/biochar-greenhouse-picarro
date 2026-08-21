picarro_data_averaged = picarro_data_joined %>%
  group_by(PotID, Replicate, Block, Soil, Treatment, Order_Index, type) %>%
  summarise(
    across(all_of(measurements), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    Order_Index = factor(Order_Index, levels = 1:15, ordered = TRUE),
    type = factor(type, levels = c("Light", "CO2_Fixation", "After_Watering"), ordered = TRUE),
    Order_Index_type = factor(
      interaction(Order_Index, type, sep = "."),
      levels = as.vector(outer(1:15, c("Light", "CO2_Fixation", "After_Watering"), paste, sep = ".")),
      ordered = TRUE
    )
  )

writexl::write_xlsx(picarro_data_averaged, file.path(base, "output/tables/picarro_data_averaged.xlsx"))
