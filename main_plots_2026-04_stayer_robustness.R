# Robustness for referee 2 / stayer-control group: MLO30 sample.
# Produces 8 PDFs in 03_Draft/graphs/2026-04/:
#   fig_1_wage_decomposition_mlo30_{baseline,stayer}.pdf
#   fig_2_{productivity,labor_share,firm_premium}_mlo30_{baseline,stayer}.pdf
# Sample mapping:
#   panelpanSIZE2 = MLO30 baseline (never-displaced controls)
#   panelpanSIZE1 = MLO30 stayer controls (forward_tenure >= 6)
# See 12-mlo30-matching.do:38-40 and PLOTTING_STRATEGY.md.
#
# Sourcing rule: worker outcomes from S2; firm outcomes time-varying from S1.

library(tidyverse)
library(readr)

path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
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

save_pdf <- function(p, filename, h_in = 5, w_in = 7){
  ggsave(file.path(out_dir, paste0(filename, ".pdf")),
         p, bg = "white", height = h_in, width = w_in, units = "in")
}

main_map <- list(
  color = c(`Firm Wage Premium` = "#91353b",
            `Log Hourly Wage`   = "#008bbc",
            `Log Hours`         = "#82c0e9",
            `Log Earnings`      = "#1e2d53"),
  shape = c(`Firm Wage Premium` = 19,
            `Log Hourly Wage`   = 17,
            `Log Hours`         = 18,
            `Log Earnings`      = 15),
  linetype = c(`Firm Wage Premium` = "solid",
               `Log Hourly Wage`   = "dashed",
               `Log Hours`         = "dashed",
               `Log Earnings`      = "solid")
)

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

load_event_studies <- function(path_sortie, sub){
  files <- list.files(file.path(path_sortie, sub), full.names = TRUE,
                      pattern = "\\.csv$")
  ES <- map_df(files, read_csv, show_col_types = FALSE)

  if ("treatment_path" %in% names(ES)) {
    ES <- filter(ES, treatment_path)
  } else {
    # S1 omits the d = -1 reference period (implicit beta = 0). Insert it
    # explicitly so plots show a continuous timeline through the cutoff,
    # mirroring choclow_export-main/src/1-merge_exports.R.
    ES <- ES |>
      group_by(across(any_of(c("sample", "dep_var", "interaction_group", "description")))) |>
      group_modify(~ add_row(.x, .before = 0,
                             distance = -1L, beta = 0,
                             std_error = NA_real_)) |>
      ungroup()
  }

  ES |>
    mutate(dep_var_label = case_when(
        dep_var == "lnsbrhour"                  ~ "Log Hourly Wage",
        dep_var == "lnnbheur"                   ~ "Log Hours",
        dep_var == "lnsbr"                      ~ "Log Earnings",
        dep_var == "fe0215_mlo_le"              ~ "Firm Wage Premium",
        TRUE ~ dep_var),
      dep_var_label = str_replace(dep_var_label, "prod_res_ma3",  "Productivity"),
      dep_var_label = str_replace(dep_var_label, "ln_lshare_ma3", "Labor Share of Value-added"),
      dep_var_label = str_replace(dep_var_label, "ln_apl_ma3",    "Value-added per Worker"),
      dep_var_label = str_replace(dep_var_label, "fe0215_mlo_le", "Firm Wage Premium"),
      effect_type = str_extract(dep_var_label, "_d2.*$"),
      effect_type = if_else(is.na(effect_type), "_overall", effect_type),
      varname     = str_remove(dep_var_label, effect_type),
      effect_type = case_when(
        effect_type == "_overall" ~ "Overall effect",
        effect_type == "_d2res"   ~ "Within sector",
        effect_type == "_d2FE"    ~ "Between sector"))
}

ES_S1 <- load_event_studies(path_S1, "event_studies-main_samples")
ES_S2 <- load_event_studies(path_S2, "event_studies-main_samples")

# Helpers ------------------------------------------------------------------
get_beta <- function(df, label, d) {
  v <- df$beta[df$dep_var_label == label & df$distance == d]
  if (length(v) == 0) NA_real_ else v[1]
}
pct <- function(num, den) sprintf("%.0f", 100 * num / den)

build_fig1 <- function(sample_name) {
  indiv <- ES_S2 |>
    filter(sample == sample_name,
           dep_var %in% c("lnsbrhour", "lnnbheur", "lnsbr"),
           interaction_group == "none",
           is.na(description)) |>
    select(distance, beta, std_error, dep_var_label)

  firm <- ES_S1 |>
    filter(sample == sample_name,
           dep_var == "fe0215_mlo_le",
           interaction_group == "none") |>
    select(distance, beta, std_error, dep_var_label)

  if (nrow(indiv) == 0 || nrow(firm) == 0) {
    stop("Missing rows for sample ", sample_name)
  }

  bind_rows(firm, indiv) |>
    mutate(dep_var_label = factor(dep_var_label,
      levels = c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings")))
}

plot_fig1 <- function(df_fig1, file_stub) {
  b_AKM_2     <- get_beta(df_fig1, "Firm Wage Premium", 2)
  b_AKM_6     <- get_beta(df_fig1, "Firm Wage Premium", 6)
  b_hourly_2  <- get_beta(df_fig1, "Log Hourly Wage",   2)
  b_hourly_6  <- get_beta(df_fig1, "Log Hourly Wage",   6)
  b_earn_2    <- get_beta(df_fig1, "Log Earnings",      2)
  b_earn_6    <- get_beta(df_fig1, "Log Earnings",      6)

  ratio_lines <- c(
    '"Ratios at d = 2, d = 6:"',
    paste0('beta[hourly]/beta[earnings] == "',
           pct(b_hourly_2, b_earn_2), '%, ',
           pct(b_hourly_6, b_earn_6), '%"'),
    paste0('beta[AKM]/beta[hourly] == "',
           pct(b_AKM_2, b_hourly_2), '%, ',
           pct(b_AKM_6, b_hourly_6), '%"')
  )

  x_anchor <- 2.6
  y_top    <- -0.60
  y_step   <- 0.09
  p <- plot_event(df_fig1, main_map, "dep_var_label") +
    scale_y_continuous(breaks = seq(-1, .2, .2), limits = c(-1, .2))
  p <- draw_label_box(p,
                      xmin = x_anchor - 0.4, xmax = 6.4,
                      ymin = y_top - 2.7 * y_step,
                      ymax = y_top + 0.5 * y_step) +
    annotate("text", x = x_anchor, y = y_top,            hjust = 0, vjust = 1,
             label = ratio_lines[1], parse = TRUE, size = 3.6) +
    annotate("text", x = x_anchor, y = y_top - y_step,   hjust = 0, vjust = 1,
             label = ratio_lines[2], parse = TRUE, size = 3.6) +
    annotate("text", x = x_anchor, y = y_top - 2*y_step, hjust = 0, vjust = 1,
             label = ratio_lines[3], parse = TRUE, size = 3.6)
  save_pdf(p, file_stub)
}

# Fig 1 ----------------------------------------------------------------------
plot_fig1(build_fig1("panelpanSIZE2"), "fig_1_wage_decomposition_mlo30_baseline")
plot_fig1(build_fig1("panelpanSIZE1"), "fig_1_wage_decomposition_mlo30_stayer")

# Fig 2 ----------------------------------------------------------------------
fig2_specs <- list(
  c(label = "Productivity",                 file = "fig_2_productivity"),
  c(label = "Labor Share of Value-added",   file = "fig_2_labor_share"),
  c(label = "Firm Wage Premium",            file = "fig_2_firm_premium")
)

plot_fig2_panel <- function(sample_name, label, file_stub) {
  df <- ES_S1 |>
    filter(sample == sample_name,
           varname == label,
           interaction_group == "none",
           is.na(description)) |>
    mutate(effect_type = factor(effect_type,
      levels = c("Overall effect", "Between sector", "Within sector")))
  if (nrow(df) == 0) {
    warning("No rows for Fig 2 / ", label, " / ", sample_name)
    return(invisible(NULL))
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
  save_pdf(p, file_stub, h_in = 5, w_in = 6)
}

for (samp in c(c(panelpanSIZE2 = "baseline"), c(panelpanSIZE1 = "stayer"))) {
  # placeholder (see explicit loop below)
}

samples <- list(panelpanSIZE2 = "baseline", panelpanSIZE1 = "stayer")
for (samp_name in names(samples)) {
  tag <- samples[[samp_name]]
  for (spec in fig2_specs) {
    plot_fig2_panel(samp_name, spec[["label"]],
                    paste0(spec[["file"]], "_mlo30_", tag))
  }
}

cat("Done. PDFs written to:\n  ", out_dir, "\n", sep = "")
