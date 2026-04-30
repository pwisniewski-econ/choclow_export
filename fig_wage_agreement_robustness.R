# Wage agreement event-study with three control specs (S2 = Export_20260423).
# Output: a single PDF with three overlaid event-study series.

library(tidyverse)

path_S2 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260423"
out_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/03_Draft/graphs/2026-04"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ci_scalar    <- 2.576
event_cutoff <- -0.5

theme_choclow <- function(){
  theme_minimal(16) +
    theme(
      axis.title.x = element_text(vjust = -1),
      axis.title.y = element_text(vjust = 3),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "grey20", linewidth = 0.6)
    )
}

ES <- read_csv(
  file.path(path_S2, "event_studies-elec_samples",
            "le5_panelsize0-acco_alt-res.csv"),
  show_col_types = FALSE) |>
  filter(treatment_path,
         dep_var == "wage_agreement_ind_07_alt",
         interaction_group == "none") |>
  mutate(spec = case_when(
    is.na(description)                        ~ "Baseline (no controls)",
    description == "controls: log(nbsa_ent)"  ~ "+ log(firm size)",
    description == "controls: log(nbsa_ent) + bins"
                                              ~ "+ log(firm size) + size bins"
  )) |>
  mutate(spec = factor(spec, levels = c(
    "Baseline (no controls)",
    "+ log(firm size)",
    "+ log(firm size) + size bins")))

# Offset distance slightly so error bars don't overlap.
ES <- ES |>
  group_by(spec) |>
  mutate(distance_offset = distance + (as.integer(spec) - 2) * 0.13) |>
  ungroup() |>
  mutate(ci_lower = beta - ci_scalar * std_error,
         ci_upper = beta + ci_scalar * std_error)

p <- ggplot(ES,
       aes(x = distance_offset, y = beta, ymin = ci_lower, ymax = ci_upper,
           color = spec, shape = spec, linetype = spec)) +
  geom_point(size = 3.5) +
  geom_line(aes(x = distance), linewidth = 0.8) +
  geom_vline(xintercept = event_cutoff, color = "grey30", linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.6) +
  geom_errorbar(width = 0.20, linewidth = 0.6) +
  scale_x_continuous(breaks = -5:6, limits = c(-5.5, 6.5),
                     expand = expansion(add = 0)) +
  scale_color_manual(values = c(
    "Baseline (no controls)"        = "#1e2d53",
    "+ log(firm size)"              = "#008bbc",
    "+ log(firm size) + size bins"  = "#91353b")) +
  scale_shape_manual(values = c(
    "Baseline (no controls)"        = 19,
    "+ log(firm size)"              = 17,
    "+ log(firm size) + size bins"  = 15)) +
  scale_linetype_manual(values = c(
    "Baseline (no controls)"        = "solid",
    "+ log(firm size)"              = "dashed",
    "+ log(firm size) + size bins"  = "dotted")) +
  theme_choclow() +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  labs(x = "", y = "", color = "", shape = "", linetype = "")

ggsave(file.path(out_dir, "fig_a_wage_agreement_size_controls.pdf"),
       p, height = 5, width = 7, units = "in", bg = "white")

cat("PDF saved at:\n  ",
    file.path(out_dir, "fig_a_wage_agreement_size_controls.pdf"), "\n",
    sep = "")
