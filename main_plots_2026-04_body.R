# Generate body-paper figures (Fig 1, 2, 3) as PDFs
# - Fig 1 (wage decomposition): hybrid — FE from S1, individual outcomes from S2
# - Fig 2 (productivity / labor share / firm premium): firm outcomes from S1
# - Fig 3 (wage agreement / election turnout): firm-level from S1
# Output: 03_Draft/graphs/2026-04/*.pdf
# See PLOTTING_STRATEGY.md (in Export_20260423/updated_scripts/) for the rationale.

library(tidyverse)
library(readr)

# Paths --------------------------------------------------------------------
path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
path_S2 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260423"
out_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/03_Draft/graphs/2026-04"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Constants ----------------------------------------------------------------
ci_scalar    <- 2.576   # 99% CI
event_cutoff <- -0.5

# Theme --------------------------------------------------------------------
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

# Color/shape/linetype maps (copied from main_plots.R) --------------------
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

default_map <- list(color = c(none = "black"),
                    shape = c(none = 19),
                    linetype = c(none = "solid"))

# Plot helper --------------------------------------------------------------
# x_breaks defaults to -5:6 so all panels share the same timeline (some
# outcomes — productivity in particular — only have data up to +5 due to
# balance-sheet lag, but we still show the +6 tick for visual consistency).
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

# Subtle, journal-style label box.
draw_label_box <- function(p, xmin, xmax, ymin, ymax){
  p + annotate("rect", xmin = xmin, xmax = xmax,
               ymin = ymin, ymax = ymax,
               fill = "white", color = "grey75", linewidth = 0.35)
}

# Loader: harmonize labels and (S2) filter treatment_path == TRUE ----------
load_event_studies <- function(path_sortie, sub){
  files <- list.files(file.path(path_sortie, sub), full.names = TRUE,
                      pattern = "\\.csv$")
  ES <- map_df(files, read_csv, show_col_types = FALSE)

  if ("treatment_path" %in% names(ES)) {
    ES <- filter(ES, treatment_path)
  } else {
    # S1 omits the d = -1 reference period (implicit beta = 0). Insert it
    # explicitly so plots show a continuous timeline through the cutoff,
    # mirroring choclow_export-main/src/1-merge_exports.R. This affects:
    #   - Fig 1 firm wage premium curve (visually invisible: flat near 0)
    #   - Fig 2 productivity, labor share, firm wage premium (visible gap)
    #   - Fig 3 wage agreement and election turnout (visible gap)
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
        dep_var == "wage_agreement_ind_07_alt"  ~ "Indicator of Wage Agreement",
        dep_var == "participation_siren0912"   ~ "Election Turnout",
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

# Load data ---------------------------------------------------------------
ES_S1_main <- load_event_studies(path_S1, "event_studies-main_samples")
ES_S1_elec <- load_event_studies(path_S1, "event_studies-elec_samples")
ES_S2_main <- load_event_studies(path_S2, "event_studies-main_samples")

# Figure 1 — wage decomposition (all curves from S2 to align reference period
# at d=-2; previously was hybrid with firm wage premium from S1, but S1 uses
# ref="-1" while S2 uses ref="-2", so superposition was inconsistent). ------
fig1_individual <- ES_S2_main |>
  filter(sample == "le5_panelsize0",
         dep_var %in% c("lnsbrhour", "lnnbheur", "lnsbr"),
         interaction_group == "none",
         is.na(description)) |>
  select(distance, beta, std_error, dep_var_label)

fig1_firm <- ES_S2_main |>
  filter(sample == "le5_panelsize0",
         dep_var == "fe0215_mlo_le",
         interaction_group == "none") |>
  select(distance, beta, std_error, dep_var_label)

fig1 <- bind_rows(fig1_firm, fig1_individual) |>
  mutate(dep_var_label = factor(dep_var_label,
    levels = c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings")))

# Ratios at d=2 and d=6 ----
get_beta <- function(df, label, d) {
  v <- df$beta[df$dep_var_label == label & df$distance == d]
  if (length(v) == 0) NA_real_ else v[1]
}
b_AKM_2     <- get_beta(fig1, "Firm Wage Premium", 2)
b_AKM_6     <- get_beta(fig1, "Firm Wage Premium", 6)
b_hourly_2  <- get_beta(fig1, "Log Hourly Wage",   2)
b_hourly_6  <- get_beta(fig1, "Log Hourly Wage",   6)
b_earn_2    <- get_beta(fig1, "Log Earnings",      2)
b_earn_6    <- get_beta(fig1, "Log Earnings",      6)

pct <- function(num, den) sprintf("%.0f", 100 * num / den)
# Plotmath strings (rendered as expressions via parse=TRUE). Greek beta
# and subscripts render natively with the default pdf device.
ratio_lines <- c(
  '"Ratios at d = 2, d = 6:"',
  paste0('beta[hourly]/beta[earnings] == "',
         pct(b_hourly_2, b_earn_2), '%, ',
         pct(b_hourly_6, b_earn_6), '%"'),
  paste0('beta[AKM]/beta[hourly] == "',
         pct(b_AKM_2, b_hourly_2), '%, ',
         pct(b_AKM_6, b_hourly_6), '%"')
)

# Empty zone: bottom-right (x in [2.5, 6], y in [-1, -0.55]).
# All curves at d>=2 are above y=-0.55; the dip at d=0 (~-0.95) is to the left.
x_anchor <- 2.6
y_top    <- -0.60
y_step   <- 0.09
p1 <- plot_event(fig1, main_map, "dep_var_label") +
  scale_y_continuous(breaks = seq(-1, .2, .2), limits = c(-1, .2))
p1 <- draw_label_box(p1,
                     xmin = x_anchor - 0.4, xmax = 6.4,
                     ymin = y_top - 2.7 * y_step,
                     ymax = y_top + 0.5 * y_step) +
  annotate("text", x = x_anchor, y = y_top,            hjust = 0, vjust = 1,
           label = ratio_lines[1], parse = TRUE, size = 3.6) +
  annotate("text", x = x_anchor, y = y_top - y_step,   hjust = 0, vjust = 1,
           label = ratio_lines[2], parse = TRUE, size = 3.6) +
  annotate("text", x = x_anchor, y = y_top - 2*y_step, hjust = 0, vjust = 1,
           label = ratio_lines[3], parse = TRUE, size = 3.6)
save_pdf(p1, "fig_1_wage_decomposition")

# Robustness: Figure 1 with the unrestricted AKM (fe0215_god2 — no exclusion of
# LE/MLO trajectories from the AKM estimation sample). Used in the response to
# Referee 2 to illustrate how the contamination correction shifts the share
# of the hourly-wage scar attributable to the firm premium.
fig1_firm_god2 <- ES_S2_main |>
  filter(sample == "le5_panelsize0",
         dep_var == "fe0215_god2",
         interaction_group == "none",
         is.na(description)) |>
  mutate(dep_var_label = "Firm Wage Premium") |>
  select(distance, beta, std_error, dep_var_label)

fig1_god2 <- bind_rows(fig1_firm_god2, fig1_individual) |>
  mutate(dep_var_label = factor(dep_var_label,
    levels = c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings")))

b_AKM_2_god2    <- get_beta(fig1_god2, "Firm Wage Premium", 2)
b_AKM_6_god2    <- get_beta(fig1_god2, "Firm Wage Premium", 6)

ratio_lines_god2 <- c(
  '"Ratios at d = 2, d = 6:"',
  paste0('beta[hourly]/beta[earnings] == "',
         pct(b_hourly_2, b_earn_2), '%, ',
         pct(b_hourly_6, b_earn_6), '%"'),
  paste0('beta[AKM]/beta[hourly] == "',
         pct(b_AKM_2_god2, b_hourly_2), '%, ',
         pct(b_AKM_6_god2, b_hourly_6), '%"')
)

p1_god2 <- plot_event(fig1_god2, main_map, "dep_var_label") +
  scale_y_continuous(breaks = seq(-1, .2, .2), limits = c(-1, .2))
p1_god2 <- draw_label_box(p1_god2,
                          xmin = x_anchor - 0.4, xmax = 6.4,
                          ymin = y_top - 2.7 * y_step,
                          ymax = y_top + 0.5 * y_step) +
  annotate("text", x = x_anchor, y = y_top,            hjust = 0, vjust = 1,
           label = ratio_lines_god2[1], parse = TRUE, size = 3.6) +
  annotate("text", x = x_anchor, y = y_top - y_step,   hjust = 0, vjust = 1,
           label = ratio_lines_god2[2], parse = TRUE, size = 3.6) +
  annotate("text", x = x_anchor, y = y_top - 2*y_step, hjust = 0, vjust = 1,
           label = ratio_lines_god2[3], parse = TRUE, size = 3.6)
save_pdf(p1_god2, "fig_1_wage_decomposition_god2")

# Robustness: Figure 1 on the LE sample restricted to firms with at least 10
# employees (sample = le5_panelsize10). Used in the response to Referee 1 to
# show that the wage decomposition is essentially unchanged on this larger
# subset of firms.
fig1_indiv_size10 <- ES_S2_main |>
  filter(sample == "le5_panelsize10",
         dep_var %in% c("lnsbrhour", "lnnbheur", "lnsbr"),
         interaction_group == "none",
         is.na(description)) |>
  select(distance, beta, std_error, dep_var_label)

fig1_firm_size10 <- ES_S2_main |>
  filter(sample == "le5_panelsize10",
         dep_var == "fe0215_mlo_le",
         interaction_group == "none",
         is.na(description)) |>
  select(distance, beta, std_error, dep_var_label)

if (nrow(fig1_indiv_size10) > 0 && nrow(fig1_firm_size10) > 0) {
  fig1_size10 <- bind_rows(fig1_firm_size10, fig1_indiv_size10) |>
    mutate(dep_var_label = factor(dep_var_label,
      levels = c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings")))

  b_AKM_2_s10    <- get_beta(fig1_size10, "Firm Wage Premium", 2)
  b_AKM_6_s10    <- get_beta(fig1_size10, "Firm Wage Premium", 6)
  b_hourly_2_s10 <- get_beta(fig1_size10, "Log Hourly Wage",   2)
  b_hourly_6_s10 <- get_beta(fig1_size10, "Log Hourly Wage",   6)
  b_earn_2_s10   <- get_beta(fig1_size10, "Log Earnings",      2)
  b_earn_6_s10   <- get_beta(fig1_size10, "Log Earnings",      6)

  ratio_lines_s10 <- c(
    '"Ratios at d = 2, d = 6:"',
    paste0('beta[hourly]/beta[earnings] == "',
           pct(b_hourly_2_s10, b_earn_2_s10), '%, ',
           pct(b_hourly_6_s10, b_earn_6_s10), '%"'),
    paste0('beta[AKM]/beta[hourly] == "',
           pct(b_AKM_2_s10, b_hourly_2_s10), '%, ',
           pct(b_AKM_6_s10, b_hourly_6_s10), '%"')
  )

  p1_size10 <- plot_event(fig1_size10, main_map, "dep_var_label") +
    scale_y_continuous(breaks = seq(-1, .2, .2), limits = c(-1, .2))
  p1_size10 <- draw_label_box(p1_size10,
                              xmin = x_anchor - 0.4, xmax = 6.4,
                              ymin = y_top - 2.7 * y_step,
                              ymax = y_top + 0.5 * y_step) +
    annotate("text", x = x_anchor, y = y_top,            hjust = 0, vjust = 1,
             label = ratio_lines_s10[1], parse = TRUE, size = 3.6) +
    annotate("text", x = x_anchor, y = y_top - y_step,   hjust = 0, vjust = 1,
             label = ratio_lines_s10[2], parse = TRUE, size = 3.6) +
    annotate("text", x = x_anchor, y = y_top - 2*y_step, hjust = 0, vjust = 1,
             label = ratio_lines_s10[3], parse = TRUE, size = 3.6)
  save_pdf(p1_size10, "fig_1_wage_decomposition_size10")
}

# Figure 2 — productivity, labor share, firm premium (S1, decomposition) --
fig2_specs <- list(
  c(label = "Productivity",                 file = "fig_2_productivity"),
  c(label = "Labor Share of Value-added",   file = "fig_2_labor_share"),
  c(label = "Firm Wage Premium",            file = "fig_2_firm_premium")
)

for (spec in fig2_specs) {
  df <- ES_S1_main |>
    filter(sample == "le5_panelsize0",
           varname == spec[["label"]],
           interaction_group == "none",
           is.na(description)) |>
    mutate(effect_type = factor(effect_type,
      levels = c("Overall effect", "Between sector", "Within sector")))
  if (nrow(df) == 0) {
    warning("No rows for Fig 2 / ", spec[["label"]])
    next
  }
  # Within share at the latest available distance (legacy used d=+6, but
  # firm productivity stops at d=+5 due to balance-sheet lag — fall back).
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

  # Per-panel x range: productivity stops at d=+5 (balance-sheet lag),
  # so we don't pad an empty +6 tick. Other firm outcomes go to d=+6.
  panel_breaks <- if (d_data_max < 6) -5:5 else -5:6
  p <- plot_event(df, decomp_map, "effect_type", x_breaks = panel_breaks)
  if (!is.na(note_expr)) {
    # Place the box ABOVE the data range (in extra y headroom). This avoids
    # crossing the y=0 line and the y-axis tick labels. Box xmin/xmax stay
    # inside the data area (away from the left y-axis labels).
    yr <- range(c(df$beta - ci_scalar * df$std_error,
                  df$beta + ci_scalar * df$std_error,
                  df$beta, 0), na.rm = TRUE)
    box_h <- 0.20 * diff(yr)   # box vertical height (in data units)
    box_lift <- 0.06 * diff(yr) # gap above data
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

# Figure 3 — wage agreement & election turnout (S1) -----------------------
fig3_specs <- list(
  list(sample = "le5_panelsize0-acco_alt",  dep = "wage_agreement_ind_07_alt", file = "fig_3_wage_agreement"),
  list(sample = "le5_panelsize0-election",  dep = "participation_siren0912",   file = "fig_3_election_turnout")
)

for (spec in fig3_specs) {
  df <- ES_S1_elec |>
    filter(sample == spec$sample,
           dep_var == spec$dep,
           interaction_group == "none",
           is.na(description)) |>
    mutate(grp = "none")
  if (nrow(df) == 0) {
    warning("No rows for Fig 3 / ", spec$dep)
    next
  }
  p <- plot_event(df, default_map, "grp") + theme(legend.position = "none")
  save_pdf(p, spec$file, h_in = 5, w_in = 6)
}

cat("Done. PDFs written to:\n  ", out_dir, "\n", sep = "")
