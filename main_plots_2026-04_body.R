# Generate body-paper figures (Fig 1, 2, 3) as PDFs
# - Fig 1 (wage decomposition): hybrid — FE from S1, individual outcomes from S2
# - Fig 2 (productivity / labor share / firm premium): firm outcomes from S1
# - Fig 3 (wage agreement / election turnout): firm-level from S1
# Output: EJ_R1/figures-2026/*.pdf
# See PLOTTING_STRATEGY.md (in Export_20260423/updated_scripts/) for the rationale.

library(tidyverse)
library(readr)

# Paths --------------------------------------------------------------------
path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
path_S2 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260423"
out_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/figures-2026"
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

# Subtle, journal-style label box. Fill is transparent (NA) so panel
# gridlines and any data behind the box show through; only the thin
# grey border remains visible.
draw_label_box <- function(p, xmin, xmax, ymin, ymax){
  p + annotate("rect", xmin = xmin, xmax = xmax,
               ymin = ymin, ymax = ymax,
               fill = NA, color = "#525c66", linewidth = 0.35)
}

# Pack multiple plotmath expression strings into one atop() string
# suitable for parse = TRUE. Used to stack 3 ratio lines inside a
# single annotate("label", ...) call so the bounding box has uniform
# label.padding on all four sides (avoids the manually-tuned rect with
# asymmetric margins).
stack_atop <- function(lines){
  if (length(lines) == 1L) return(lines[[1]])
  expr <- lines[[length(lines)]]
  for (i in (length(lines) - 1L):1L) {
    expr <- paste0("atop(", lines[[i]], ", ", expr, ")")
  }
  expr
}

# Annotate-label box with constant padding around (multi-line) plotmath.
# Replaces the rect + per-line annotate("text") combination used earlier.
add_ratio_box <- function(p, x, y, lines, size = 3.6,
                          color = "#525c66", fill = NA){
  p + annotate("label",
               x = x, y = y,
               label = stack_atop(lines), parse = TRUE,
               hjust = 0, vjust = 1, size = size,
               label.padding = unit(0.5, "lines"),
               label.r       = unit(0.15, "lines"),
               label.size    = 0.3,
               color = color,
               fill  = fill)
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

# Auto-compute y range from the actual coefficients + 99% CIs, rounded
# down/up to the nearest 0.1. Avoids the dead headroom that came from
# the previously-hardcoded c(-1, 0.2) range.
yr1 <- range(c(fig1$beta - ci_scalar * fig1$std_error,
               fig1$beta + ci_scalar * fig1$std_error, 0),
             na.rm = TRUE)
y_low_1  <- floor(yr1[1] * 10) / 10
y_high_1 <- ceiling(yr1[2] * 10) / 10

# Empty zone: bottom-left (x in [-5, -1], y in [y_low_1, ~-0.4]).
# All curves at d in [-5, -1] sit near y=0; the dramatic drop is at d=0,
# so the box does not overlap any data lines or markers.
p1 <- plot_event(fig1, main_map, "dep_var_label") +
  scale_y_continuous(breaks = seq(y_low_1, 0, 0.2),
                     limits = c(y_low_1, y_high_1))
p1 <- add_ratio_box(p1, x = -4.8, y = -0.40, lines = ratio_lines, size = 3.6)
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
p1_god2 <- add_ratio_box(p1_god2, x = -4.8, y = -0.50,
                         lines = ratio_lines_god2, size = 3.6)
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

  # Place the ratio box in the bottom-left empty zone, where pre-period
  # data (d in [-5, -1]) sit near y = 0 and the box does not overlap any
  # of the four event-study curves.
  p1_size10 <- plot_event(fig1_size10, main_map, "dep_var_label") +
    scale_y_continuous(breaks = seq(-1, .2, .2), limits = c(-1, .2))
  p1_size10 <- add_ratio_box(p1_size10, x = -4.8, y = -0.50,
                             lines = ratio_lines_s10, size = 3.6)
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
      'frac(group("|", beta[within], "|"), group("|", beta[within], "|") + group("|", beta[between], "|")) == "%.0f%%"',
      100 * within_share
    )
  } else {
    note_expr <- NA_character_
  }

  # Per-panel x range: productivity stops at d=+5 (balance-sheet lag),
  # so we don't pad an empty +6 tick. Other firm outcomes go to d=+6.
  panel_breaks <- if (d_data_max < 6) -5:5 else -5:6

  # Auto-compute y range from coefficients + 99% CIs, rounded to 0.05.
  # This eliminates the previous expansion/headroom approach (the box now
  # sits inside the empty top-left zone of the data range, like Fig 1).
  yr <- range(c(df$beta - ci_scalar * df$std_error,
                df$beta + ci_scalar * df$std_error, 0), na.rm = TRUE)
  y_low_2  <- floor(yr[1]   * 20) / 20
  y_high_2 <- ceiling(yr[2] * 20) / 20
  # round() avoids the seq() floating-point glitch that produced labels
  # like "2.775558e-17" instead of "0.00".
  y_breaks_2 <- round(seq(y_low_2, y_high_2, 0.05), 2)

  p <- plot_event(df, decomp_map, "effect_type", x_breaks = panel_breaks) +
    scale_y_continuous(limits = c(y_low_2, y_high_2),
                       breaks = y_breaks_2,
                       labels = function(x) sprintf("%.2f", x))

  if (!is.na(note_expr)) {
    # Box anchored in the top-left empty zone: pre-period (d in [-5, -1])
    # data sit near 0, so y close to y_high_2 is empty. hjust=0/vjust=1
    # places the top-left corner of the label box at (x, y).
    p <- p + annotate("label",
                      x = panel_breaks[1] + 0.1, y = y_high_2,
                      label = note_expr, parse = TRUE,
                      hjust = 0, vjust = 1, size = 3.2,
                      label.padding = unit(0.5, "lines"),
                      label.r       = unit(0.15, "lines"),
                      label.size    = 0.3,
                      color = "#525c66",
                      fill  = NA)
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
  # Restrict the x-axis to the horizons where the coefficient is actually
  # defined (the bargaining-data window stops at d=+4 for accords and
  # d=+3 for elections, while other figures span d in [-5, +6]).
  panel_breaks_3 <- seq(min(df$distance, na.rm = TRUE),
                        max(df$distance, na.rm = TRUE))
  p <- plot_event(df, default_map, "grp", x_breaks = panel_breaks_3) +
    theme(legend.position = "none")
  save_pdf(p, spec$file, h_in = 5, w_in = 6)
}

cat("Done. PDFs written to:\n  ", out_dir, "\n", sep = "")
