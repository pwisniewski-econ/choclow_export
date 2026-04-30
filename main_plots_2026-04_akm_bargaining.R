# Binned scatter: firm wage premium vs bargaining outcomes (referee 1, 1b).
# Produces 2 PDFs in 03_Draft/graphs/2026-04/:
#   firm_LE_binscatter_wage_agreement.pdf
#   firm_LE_binscatter_election_turnout.pdf
#
# Source: Export_20260331/fig_a7-binned_scatter/akm_bargaining.csv
# Bins firms into 20 quantiles of AKM premium (fe0215_mlo_le, contamination-corrected),
# computes mean of wage_agreement_num_07 and participation_siren0912 in each bin.

library(tidyverse)
library(readr)

path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
out_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/03_Draft/graphs/2026-04"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

theme_choclow <- function(){
  theme_minimal(16) +
    theme(
      axis.title.x = element_text(vjust = -1),
      axis.title.y = element_text(vjust = 3),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "grey20", linewidth = 0.6)
    )
}

save_pdf <- function(p, filename, h_in = 5, w_in = 6){
  ggsave(file.path(out_dir, paste0(filename, ".pdf")),
         p, bg = "white", height = h_in, width = w_in, units = "in")
}

bs <- read_csv(file.path(path_S1, "fig_a7-binned_scatter/akm_bargaining.csv"),
               show_col_types = FALSE)

# Use the contamination-corrected AKM (fe0215_mlo_le) — matches paper baseline
bs_mlo_le <- bs |> filter(xname == "fe0215_mlo_le")

# Helper: drop endpoint outliers as in existing Fig A7 (bottom 1, top 1)
trim <- function(df) df |> filter(quantile > 1, quantile < max(quantile))

plot_binscatter <- function(df, yvar, ylab, file_stub) {
  yvals <- df[[yvar]]
  df_plot <- df |> mutate(y = yvals)
  fit <- lm(y ~ mean_xval, data = trim(df_plot))
  slope <- coef(fit)[2]

  p <- ggplot(df_plot, aes(x = mean_xval, y = y)) +
    geom_smooth(data = trim(df_plot), method = "lm", se = FALSE,
                color = "grey60", linewidth = 0.6, formula = y ~ x) +
    geom_point(color = "#1e2d53", size = 3) +
    theme_choclow() +
    labs(x = "Firm wage premium (AKM)", y = ylab) +
    annotate("text",
             x = quantile(df_plot$mean_xval, 0.05),
             y = max(df_plot$y, na.rm = TRUE),
             hjust = 0, vjust = 1,
             label = sprintf("Fitted slope = %.3f", slope),
             size = 4, color = "grey30")
  save_pdf(p, file_stub)
  invisible(slope)
}

s1 <- plot_binscatter(bs_mlo_le, "wage_agreement_num_07",
                      "Indicator of wage agreement",
                      "firm_LE_binscatter_wage_agreement")
s2 <- plot_binscatter(bs_mlo_le, "participation_siren0912",
                      "Election turnout",
                      "firm_LE_binscatter_election_turnout")

cat(sprintf("Wage agreement slope:  %.4f\n", s1))
cat(sprintf("Election turnout slope: %.4f\n", s2))
cat(sprintf("Done. PDFs written to:\n  %s\n", out_dir))
