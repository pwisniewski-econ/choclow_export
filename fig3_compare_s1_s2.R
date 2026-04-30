# Side-by-side Fig 3 comparison: Sortie 1 (20260331) vs Sortie 2 (20260423)
# Output to a comparison subfolder so we can decide visually.

library(tidyverse)

source_lines <- 1:20
src_path <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/plots2026/main_plots_2026-04_body.R"
src <- readLines(src_path)
# Reuse helpers from the body script.
src_block <- src[1:which(src == "# Load data ---------------------------------------------------------------")]
eval(parse(text = src_block))

paths <- list(
  S1 = path_S1,
  S2 = path_S2
)

cmp_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/plots2026/fig3_s1_vs_s2"
dir.create(cmp_dir, showWarnings = FALSE)

fig3_specs <- list(
  list(sample = "le5_panelsize0-acco_alt", dep = "wage_agreement_ind_07_alt",
       title  = "Indicator of wage agreement (2005-2007)",
       file   = "wage_agreement"),
  list(sample = "le5_panelsize0-election", dep = "participation_siren0912",
       title  = "Election turnout",
       file   = "election_turnout")
)

for (src_lbl in names(paths)) {
  ES <- load_event_studies(paths[[src_lbl]], "event_studies-elec_samples")
  for (spec in fig3_specs) {
    df <- ES |>
      filter(sample == spec$sample, dep_var == spec$dep,
             interaction_group == "none", is.na(description)) |>
      mutate(grp = "none")
    if (nrow(df) == 0) next
    p <- plot_event(df, default_map, "grp", legend_nrow = 1) +
      theme(legend.position = "none") +
      labs(subtitle = paste0(spec$title, "  [", src_lbl, "]"))
    ggsave(file.path(cmp_dir, paste0("fig_3_", spec$file, "_", src_lbl, ".pdf")),
           p, height = 5, width = 6, units = "in")
  }
}

cat("Comparison PDFs at:\n  ", cmp_dir, "\n", sep = "")
