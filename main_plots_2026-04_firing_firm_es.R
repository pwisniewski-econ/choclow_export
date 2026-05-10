# Firm-level event-study of laying-off firms' productivity (LE sample).
# Builds 3 PDFs (one per outcome) for R3 Item 8 of the EJ R1 revision.
#
# Inputs:
#   /Users/.../casd_export2/ref_answers-productivity/productivity-post_le_es.csv
#
# Outputs (in EJ_R1/figures-2026/):
#   firm_es_prod_res.pdf
#   firm_es_tfp_cd.pdf
#   firm_es_tfp_tl.pdf
#
# Specification:
#   sample      == "le5_panelsize0"
#   description == "LE-event def: first"
#   3 outcomes: prod_res_ma3, tfp_cd_ma3, tfp_tl_ma3 (centered MA3 over [t-1,t,t+1])
#   reference horizon h=-2 inserted explicitly with beta=SE=0
#   shaded transition zone: h in {-1, 0, +1}

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
})

# Paths --------------------------------------------------------------------
csv_path <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/context/choclow_export-main/data/external/casd_export2/ref_answers-productivity/productivity-post_le_es.csv"
out_dir  <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/figures-2026"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Constants ----------------------------------------------------------------
ci_scalar    <- 2.576    # 99% CI
event_cutoff <- -0.5     # consistent with body/appendix scripts

theme_choclow <- function(){
  theme_minimal(16) +
    theme(
      axis.title.x     = element_text(vjust = -1),
      axis.title.y     = element_text(vjust = 3),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      axis.line        = element_line(colour = "grey20", linewidth = 0.6)
    )
}

save_pdf <- function(p, filename, h_in = 5, w_in = 7){
  ggsave(file.path(out_dir, paste0(filename, ".pdf")),
         p, bg = "white", height = h_in, width = w_in, units = "in")
}

# Load and filter ----------------------------------------------------------
ES <- read_csv(csv_path, show_col_types = FALSE)

dep_targets <- c("prod_res_ma3", "tfp_cd_ma3", "tfp_tl_ma3")

ES <- ES |>
  filter(sample == "le5_panelsize0",
         description == "LE-event def: first",
         dep_var %in% dep_targets) |>
  mutate(h = as.integer(str_match(coefficient, "\\{h=(-?\\d+)\\}")[, 2]))

stopifnot(all(!is.na(ES$h)))

# Insert reference horizon h = -2 explicitly (beta=0, SE=0) ----------------
ref_rows <- tibble(
  coefficient = "d_treated_{h=-2}",
  beta        = 0,
  std_error   = 0,
  dep_var     = dep_targets,
  h           = -2L
)

ES <- bind_rows(ES |> select(coefficient, beta, std_error, dep_var, h),
                ref_rows) |>
  arrange(dep_var, h)

# Plot ---------------------------------------------------------------------
plot_one <- function(df_one){
  df_one <- df_one |>
    mutate(ci_lower = beta - ci_scalar * std_error,
           ci_upper = beta + ci_scalar * std_error,
           is_ref   = (h == -2))

  ggplot(df_one, aes(x = h, y = beta)) +
    # transition zone shading: horizons whose centered MA3 windows cover t^D
    annotate("rect", xmin = -1.5, xmax = 1.5,
             ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.4) +
    geom_vline(xintercept = event_cutoff,
               linetype = "dashed", color = "grey30", linewidth = 0.6) +
    geom_hline(yintercept = 0, color = "grey30", linewidth = 0.6) +
    geom_errorbar(data = filter(df_one, !is_ref),
                  aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.25, linewidth = 0.75) +
    geom_line(linewidth = 0.8, color = "#1e2d53") +
    geom_point(size = 3.5, color = "#1e2d53") +
    scale_x_continuous(breaks = -4:6,
                       limits = c(-4.5, 6.5),
                       expand = expansion(add = 0)) +
    theme_choclow() +
    labs(x = "Horizon h (years from LE event)", y = "")
}

for (dv in dep_targets){
  p <- plot_one(filter(ES, dep_var == dv))
  fname <- switch(dv,
                  prod_res_ma3 = "firm_es_prod_res",
                  tfp_cd_ma3   = "firm_es_tfp_cd",
                  tfp_tl_ma3   = "firm_es_tfp_tl")
  save_pdf(p, fname)
  message("Saved: ", file.path(out_dir, paste0(fname, ".pdf")))
}

message("Done.")
