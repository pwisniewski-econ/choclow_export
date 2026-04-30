# Robustness for referee 3 (timing of productivity measurement):
# reproduce Figure 2 (productivity / labor share / firm wage premium) using
# the firm-level outcome averaged over the entire sample period (2002-2015)
# instead of the 3-year moving average centered on the destination year.
#
# Source: Sortie 1 (Export_20260331), event_studies-main_samples.
# dep_vars used:
#   prod_res_mean_0215      (instead of prod_res_ma3 in the paper Fig 2)
#   ln_lshare_mean_0215     (instead of ln_lshare_ma3)
#   fe0215_mlo_le           (unchanged; AKM premium is already a 2002-2015 estimate)
#
# Output PDFs in 03_Draft/graphs/2026-04/:
#   fig_2_productivity_mean0215.pdf
#   fig_2_labor_share_mean0215.pdf
#   fig_2_firm_premium_mean0215.pdf  (= a copy of fig_2_firm_premium for layout)
#
# See referee 3 source_referee3.txt lines 119-129 for the timing comment.

library(tidyverse)
library(readr)

path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
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

save_pdf <- function(p, filename, h_in = 5, w_in = 6){
  ggsave(file.path(out_dir, paste0(filename, ".pdf")),
         p, bg = "white", height = h_in, width = w_in, units = "in")
}

decomp_map <- list(
  color    = c(`Overall effect` = "#1e2d53",
               `Within sector`  = "#a0a0a0",
               `Between sector` = "#82c0e9"),
  shape    = c(`Overall effect` = 19,
               `Within sector`  = 18,
               `Between sector` = 15),
  linetype = c(`Overall effect` = "solid",
               `Within sector`  = "solid",
               `Between sector` = "solid")
)

plot_event <- function(DF, scale_map, group_var, legend_nrow = 2,
                       x_breaks = -5:6){
  DF |>
    mutate(ci_lower = beta - ci_scalar * std_error,
           ci_upper = beta + ci_scalar * std_error) |>
    ggplot(aes(x = distance, y = beta, ymin = ci_lower, ymax = ci_upper,
               color = .data[[group_var]],
               linetype = .data[[group_var]],
               shape = .data[[group_var]])) +
    geom_point(size = 3.5) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = event_cutoff, color = "grey30", linewidth = 0.6) +
    geom_hline(yintercept = 0,           color = "grey30", linewidth = 0.6) +
    geom_errorbar(width = 0.25, linewidth = 0.75, linetype = "solid") +
    scale_x_continuous(breaks = x_breaks, limits = c(min(x_breaks) - 0.5,
                                                     max(x_breaks) + 0.5),
                       expand = expansion(add = 0)) +
    theme_choclow() +
    labs(x = "", y = "", color = "", linetype = "", shape = "") +
    scale_color_manual(values = scale_map$color) +
    scale_shape_manual(values = scale_map$shape) +
    scale_linetype_manual(values = scale_map$linetype) +
    guides(color    = guide_legend(nrow = legend_nrow),
           shape    = guide_legend(nrow = legend_nrow),
           linetype = guide_legend(nrow = legend_nrow))
}

draw_label_box <- function(p, xmin, xmax, ymin, ymax){
  p + annotate("rect", xmin = xmin, xmax = xmax,
               ymin = ymin, ymax = ymax,
               fill = "white", color = "grey75", linewidth = 0.35)
}

# Loader: same as body.R — adds d=-1 reference row for S1 implicit ref=-1 spec.
load_event_studies <- function(path_sortie, sub){
  files <- list.files(file.path(path_sortie, sub), full.names = TRUE,
                      pattern = "\\.csv$")
  ES <- map_df(files, read_csv, show_col_types = FALSE)

  if ("treatment_path" %in% names(ES)) {
    ES <- filter(ES, treatment_path)
  } else {
    ES <- ES |>
      group_by(across(any_of(c("sample", "dep_var", "interaction_group", "description")))) |>
      group_modify(~ add_row(.x, .before = 0,
                             distance = -1L, beta = 0,
                             std_error = NA_real_)) |>
      ungroup()
  }

  ES |>
    mutate(dep_var_label = case_when(
        dep_var == "fe0215_mlo_le" ~ "Firm Wage Premium",
        TRUE ~ dep_var),
      dep_var_label = str_replace(dep_var_label, "prod_res_mean_0215",  "Productivity"),
      dep_var_label = str_replace(dep_var_label, "ln_lshare_mean_0215", "Labor Share of Value-added"),
      dep_var_label = str_replace(dep_var_label, "ln_apl_mean_0215",    "Value-added per Worker"),
      dep_var_label = str_replace(dep_var_label, "fe0215_mlo_le",       "Firm Wage Premium"),
      effect_type = str_extract(dep_var_label, "_d2.*$"),
      effect_type = if_else(is.na(effect_type), "_overall", effect_type),
      varname     = str_remove(dep_var_label, effect_type),
      effect_type = case_when(
        effect_type == "_overall" ~ "Overall effect",
        effect_type == "_d2res"   ~ "Within sector",
        effect_type == "_d2FE"    ~ "Between sector"))
}

ES_S1_main <- load_event_studies(path_S1, "event_studies-main_samples")

# Figure 2 with _mean_0215 timing ----
fig2_specs <- list(
  c(label = "Productivity",                 file = "fig_2_productivity_mean0215"),
  c(label = "Labor Share of Value-added",   file = "fig_2_labor_share_mean0215"),
  c(label = "Firm Wage Premium",            file = "fig_2_firm_premium_mean0215")
)

for (spec in fig2_specs) {
  df <- ES_S1_main |>
    filter(sample == "le5_panelsize0",
           varname == spec[["label"]],
           interaction_group == "none",
           is.na(description),
           # Use mean_0215 versions for productivity and labor share;
           # firm wage premium fe0215_mlo_le is already a 2002-2015 estimate.
           dep_var %in% c("prod_res_mean_0215",
                          "prod_res_mean_0215_d2FE",
                          "prod_res_mean_0215_d2res",
                          "ln_lshare_mean_0215",
                          "ln_lshare_mean_0215_d2FE",
                          "ln_lshare_mean_0215_d2res",
                          "fe0215_mlo_le",
                          "fe0215_mlo_le_d2FE",
                          "fe0215_mlo_le_d2res")) |>
    mutate(effect_type = factor(effect_type,
      levels = c("Overall effect", "Between sector", "Within sector")))

  if (nrow(df) == 0) {
    warning("No rows for ", spec[["label"]])
    next
  }

  d_data_max <- max(df$distance[df$effect_type == "Within sector"], na.rm = TRUE)
  b_within  <- mean(df$beta[df$effect_type == "Within sector"  & df$distance == d_data_max], na.rm = TRUE)
  b_between <- mean(df$beta[df$effect_type == "Between sector" & df$distance == d_data_max], na.rm = TRUE)
  if (is.finite(b_within) && is.finite(b_between)) {
    within_share <- abs(b_within) / (abs(b_within) + abs(b_between))
    note_expr <- sprintf(
      'frac(group("|", beta[within], "|"), group("|", beta[within], "|") + group("|", beta[between], "|")) == "%.0f%% (d = %+d)"',
      100 * within_share, d_data_max
    )
  } else {
    note_expr <- NA_character_
  }

  panel_breaks <- if (d_data_max < 6) -5:5 else -5:6
  p <- plot_event(df, decomp_map, "effect_type", x_breaks = panel_breaks)
  if (!is.na(note_expr)) {
    yr <- range(c(df$beta - ci_scalar * df$std_error,
                  df$beta + ci_scalar * df$std_error,
                  df$beta, 0), na.rm = TRUE)
    box_h <- 0.20 * diff(yr)
    box_lift <- 0.06 * diff(yr)
    p <- draw_label_box(p,
                        xmin = panel_breaks[1] + 0.3,
                        xmax = panel_breaks[1] + 4.7,
                        ymin = yr[2] + box_lift,
                        ymax = yr[2] + box_lift + box_h) +
      annotate("text", x = panel_breaks[1] + 0.5,
               y = yr[2] + box_lift + box_h - 0.04 * diff(yr),
               hjust = 0, vjust = 1, label = note_expr,
               parse = TRUE, size = 3.5) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.30)))
  }
  save_pdf(p, spec[["file"]], h_in = 5, w_in = 6)
}

cat("Done. PDFs written to:\n  ", out_dir, "\n", sep = "")
