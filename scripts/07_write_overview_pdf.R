library(gridExtra)
library(grid)

# Write the overview tables from the Markdown report to a one-page PDF: one
# table per gas, rows are measurement conditions, columns are comparisons,
# cells are "% change (p)" with significant differences in bold.
# Reuses cmp_levels, gas_label, gases and format_p from 06_write_markdown.R.

# comparison headers split over two lines so the columns stay narrow
cmp_headers = sub(" vs ", "\nvs ", cmp_levels)

overview_table = function(g) {
  d = filter(summary_table, gas == g)
  cells = sapply(cmp_levels, \(cmp) sapply(conditions, \(cond) {
    r = filter(d, comparison == cmp, Condition == cond)
    paste0(sprintf("%+.1f%%", r$percent_change), " (", format_p(r$p_treatment), ")")
  }))
  bold = sapply(cmp_levels, \(cmp) sapply(conditions, \(cond)
    filter(d, comparison == cmp, Condition == cond)$significant))

  tab = cbind(Condition = conditions, matrix(cells, nrow = length(conditions)))
  colnames(tab) = c("Condition", cmp_headers)

  # fontface per body cell: Condition column plain, significant cells bold
  faces = cbind("plain", ifelse(bold, "bold", "plain"))
  theme = ttheme_minimal(
    base_size = 9,
    core    = list(fg_params = list(fontface = faces, hjust = 0, x = 0.05),
                   bg_params = list(fill = c("grey97", "white"))),
    colhead = list(fg_params = list(fontface = "bold", hjust = 0, x = 0.05))
  )
  t = tableGrob(tab, rows = NULL, theme = theme)
  # same column widths in every table, left-aligned under the gas title
  t$widths = unit(c(1.0, rep(1.3, length(cmp_levels))), "in")
  t$vp = viewport(x = unit(0, "npc") + 0.5 * sum(t$widths))
  # don't clip text at cell edges; the longest header is slightly wider than its column
  t$layout$clip = "off"
  title = textGrob(gas_label[g], x = 0, hjust = 0, gp = gpar(fontsize = 12, fontface = "bold"))
  arrangeGrob(title, t, ncol = 1, heights = unit.c(unit(1.6, "lines"), sum(t$heights)))
}

page_title = textGrob("Pairwise mixed-model results: % change of test vs reference (p)",
                      x = 0, hjust = 0, gp = gpar(fontsize = 14, fontface = "bold"))
footnote = textGrob(paste0("Bold: p < 0.05 (Type III F-test for Treatment, 3 denominator df). No correction for multiple testing.\n",
                           "% change of the test treatment relative to the reference, averaged over the 15 measurement occasions."),
                    x = 0, hjust = 0, gp = gpar(fontsize = 8, col = "grey30"))

# cairo_pdf renders the Unicode subscripts in the gas names (CO₂, CH₄, ...)
cairo_pdf(file.path(base, "output/tables/mixed_model_overview.pdf"), width = 8.5, height = 11)
grid.arrange(grobs = c(list(page_title), lapply(gases, overview_table), list(footnote)),
             ncol = 1, heights = c(0.6, rep(1.6, length(gases)), 0.5),
             padding = unit(1, "lines"), vp = viewport(width = 0.9, height = 0.92))
dev.off()
