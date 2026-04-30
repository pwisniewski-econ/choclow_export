# Generate appendix figures (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, plus the
# "loss in premium" robustness sub-figures from D_negociation.tex) as PDFs.
# Mirrors plots2026/main_plots_2026-04_body.R conventions.
#
# Figure-by-figure provenance is documented inline; see also
#   00_ExportsCASD/Export_20260423/updated_scripts/PLOTTING_STRATEGY.md
# for the broad rule (firm outcomes -> S1; individual outcomes -> S2).
#
# Output: 03_Draft/graphs/2026-04/*.pdf
# Filenames mirror the legacy ones so the LaTeX path swap is mechanical.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
})

# Paths --------------------------------------------------------------------
path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
path_S2 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260423"
out_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/03_Draft/graphs/2026-04"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Constants / theme / helpers (mirror body script) -------------------------
ci_scalar    <- 2.576   # 99% CI
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

default_map <- list(color = c(none = "black"),
                    shape = c(none = 19),
                    linetype = c(none = "solid"))

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

# Loader (filter treatment_path on S2; harmonize labels) -------------------
load_event_studies <- function(path_sortie, sub){
  files <- list.files(file.path(path_sortie, sub), full.names = TRUE,
                      pattern = "\\.csv$")
  ES <- map_df(files, read_csv, show_col_types = FALSE)

  if ("treatment_path" %in% names(ES)) {
    ES <- filter(ES, treatment_path)
  }

  ES |>
    mutate(dep_var_label = case_when(
        dep_var == "lnsbrhour"                  ~ "Log Hourly Wage",
        dep_var == "lnnbheur"                   ~ "Log Hours",
        dep_var == "lnsbr"                      ~ "Log Earnings",
        dep_var == "fe0215_mlo_le"              ~ "Firm Wage Premium",
        TRUE ~ dep_var))
}

ES_S1_main <- load_event_studies(path_S1, "event_studies-main_samples")
ES_S2_main <- load_event_studies(path_S2, "event_studies-main_samples")
ES_S1_elec <- load_event_studies(path_S1, "event_studies-elec_samples")

# =========================================================================
# FIG A1 — LE descriptives (sector / occupation / education)
# =========================================================================
# Source: misc-descriptive_statistics/cs1_sec10_samples.csv (S2)
# - cs1     -> occupation (PCS 1-digit)
# - sec_a10 -> sector (NACE A10)
# - dip_tot -> educational attainment
# Filter sample == le5_panelsize0; treated in {0,1}.
desc_S2 <- read_csv(file.path(path_S2, "misc-descriptive_statistics/cs1_sec10_samples.csv"),
                    show_col_types = FALSE)

# Sector / occupation / diploma label tables -------------------------------
sec_a10_lab <- c(
  "1"  = "Agriculture",
  "2"  = "Manufacturing",
  "3"  = "Energy / utilities",
  "4"  = "Construction",
  "5"  = "Retail / trade",
  "6"  = "Transportation",
  "7"  = "Hospitality",
  "8"  = "Information / com.",
  "9"  = "Finance / real est.",
  "10" = "Other services"
)
cs1_lab <- c(
  "1" = "Farmers",
  "2" = "Self-employed",
  "3" = "Managers",
  "4" = "Intermediate prof.",
  "5" = "Employees",
  "6" = "Blue collar",
  "7" = "Other / unknown"
)
dip_lab <- c(
  "1" = "Higher education (>=2y)",
  "2" = "Higher educ. (other)",
  "3" = "Bac+2",
  "4" = "Baccalaureate",
  "5" = "Vocational dipl. (CAP/BEP)",
  "6" = "Brevet",
  "7" = "No diploma",
  "8" = "Other"
)

plot_descriptive_share <- function(df, varname, label_vec){
  d <- df |>
    filter(variable == varname,
           sample %in% c("le5_panelsize0", "panelpanSIZE2"),
           treated %in% c(0, 1)) |>
    filter(sample == sample[1] | TRUE) |>  # placeholder, real filter below
    select(value, n_individuals, sample, treated)
  # Caller has already restricted to one sample, so just compute shares.
  d |>
    group_by(sample, treated) |>
    mutate(share = n_individuals / sum(n_individuals, na.rm = TRUE)) |>
    ungroup() |>
    mutate(value_chr = as.character(value),
           label = ifelse(value_chr %in% names(label_vec),
                          label_vec[value_chr], "Missing"))
}

# Helper to plot a side-by-side bar chart of treated vs control distributions.
plot_balance_distribution <- function(df, varname, label_vec, samp){
  d <- df |>
    filter(variable == varname,
           sample == samp,
           treated %in% c(0, 1)) |>
    select(value, n_individuals, treated)
  d <- d |>
    mutate(value_chr = ifelse(is.na(value), "NA", as.character(value)),
           label = ifelse(value_chr %in% names(label_vec),
                          label_vec[value_chr],
                          ifelse(value_chr == "NA", "Missing", value_chr)))
  shares <- d |>
    group_by(treated) |>
    mutate(share = n_individuals / sum(n_individuals, na.rm = TRUE)) |>
    ungroup() |>
    mutate(group = if_else(treated == 1, "Displaced", "Control"))

  # Drop missing for plotting if too small (still keep if reasonable)
  shares <- shares |>
    filter(!is.na(value))

  shares <- shares |>
    mutate(label = factor(label, levels = unname(label_vec)))

  ggplot(shares, aes(x = label, y = share, fill = group)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    scale_fill_manual(values = c(Displaced = "#91353b", Control = "#1e2d53"),
                      name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "", y = "Share of workers") +
    theme_choclow() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))
}

# Produce LE figures
fA1_sec <- plot_balance_distribution(desc_S2, "sec_a10", sec_a10_lab, "le5_panelsize0")
fA1_occ <- plot_balance_distribution(desc_S2, "cs1",     cs1_lab,     "le5_panelsize0")
fA1_dip <- plot_balance_distribution(desc_S2, "dip_tot", dip_lab,     "le5_panelsize0")
save_pdf(fA1_sec, "LEAlldesc_sec",   h_in = 4, w_in = 9)
save_pdf(fA1_occ, "LEAlldesc_occup", h_in = 4, w_in = 9)
save_pdf(fA1_dip, "LEAlldesc_dip",   h_in = 4, w_in = 9)

# =========================================================================
# FIG A2 — LE displacement effects in level (earnings, employment, hours)
# =========================================================================
# Source: event_studies-main_samples/le5_panelsize0-res.csv (S2, individual)
fig_a2_specs <- list(
  list(dep = "sbr",      file = "event_sbr_All",      ylab = "Annual earnings (euros)"),
  list(dep = "employed", file = "event_employed_All", ylab = "Employed (probability)"),
  list(dep = "nbheur",   file = "event_nbheur_All",   ylab = "Hours worked (per year)")
)
for (spec in fig_a2_specs){
  df <- ES_S2_main |>
    filter(sample == "le5_panelsize0",
           dep_var == spec$dep,
           interaction_group == "none",
           is.na(description)) |>
    mutate(grp = "none")
  if (nrow(df) == 0){
    warning("No rows for fig A2 / ", spec$dep); next
  }
  p <- plot_event(df, default_map, "grp") +
    theme(legend.position = "none") +
    labs(y = spec$ylab)
  save_pdf(p, spec$file, h_in = 5, w_in = 6)
}

# =========================================================================
# FIG A3 — MLO loss-in-wage-premium (FE + 3 individual outcomes), pretrends
# =========================================================================
# Hybrid (Fig 1 analog): firm FE from S1, individual from S2.
samp_mlo <- "panelpanSIZE2"

fig_a3_individual <- ES_S2_main |>
  filter(sample == samp_mlo,
         dep_var %in% c("lnsbrhour", "lnnbheur", "lnsbr"),
         interaction_group == "none",
         is.na(description)) |>
  select(distance, beta, std_error, dep_var_label)
fig_a3_firm <- ES_S1_main |>
  filter(sample == samp_mlo,
         dep_var == "fe0215_mlo_le",
         interaction_group == "none") |>
  select(distance, beta, std_error, dep_var_label)
fig_a3 <- bind_rows(fig_a3_firm, fig_a3_individual) |>
  mutate(dep_var_label = factor(dep_var_label,
    levels = c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings")))

if (nrow(fig_a3) > 0){
  p_a3 <- plot_event(fig_a3, main_map, "dep_var_label", x_breaks = -5:6)
  save_pdf(p_a3, "event_allcoef_mlo_PreTrend_fe0215_god", h_in = 5.5, w_in = 8.5)
}

# =========================================================================
# FIG A4 — MLO firm characteristics 2001-2004 (productivity, VA/W, lab share)
# =========================================================================
# Source: S1 (firm outcomes); sample = panelpanSIZE2.
fig_a4_specs <- list(
  list(dep = "prod_res_ma3",  file = "event_firmfeat_prod_res_mlo_Log",  ylab = "Productivity"),
  list(dep = "ln_apl_ma3",    file = "event_firmfeat_ln_apl_mlo_Log",    ylab = "Log VA / worker"),
  list(dep = "ln_lshare_ma3", file = "event_firmfeat_ln_Lshare_mlo_Log", ylab = "Log labor share")
)
for (spec in fig_a4_specs){
  df <- ES_S1_main |>
    filter(sample == samp_mlo,
           dep_var == spec$dep,
           interaction_group == "none",
           is.na(description)) |>
    mutate(grp = "none")
  if (nrow(df) == 0){ warning("No rows for fig A4 / ", spec$dep); next }
  p <- plot_event(df, default_map, "grp") +
    theme(legend.position = "none") +
    labs(y = spec$ylab)
  save_pdf(p, spec$file, h_in = 5, w_in = 6)
}

# =========================================================================
# FIG A5 — LE firm characteristics, ROLLING WINDOW [tD-7, tD-5]
# =========================================================================
# Use S2's _dlag4 variants (correspond to legacy "_Roll" specs);
# S1 does not contain these.
ES_S2_main_full <- {
  files <- list.files(file.path(path_S2, "event_studies-main_samples"),
                      full.names = TRUE, pattern = "\\.csv$")
  map_df(files, read_csv, show_col_types = FALSE) |>
    filter(treatment_path)
}

fig_a5_specs <- list(
  list(dep = "prod_res_ma3_dlag4",  file = "event_firmfeat_prod_resRoll_Log",     ylab = "Productivity"),
  list(dep = "ln_apl_ma3_dlag4",    file = "event_firmfeat_ln_aplRoll_Log",       ylab = "Log VA / worker"),
  list(dep = "ln_lshare_ma3_dlag4", file = "event_firmfeat_ln_LshareRoll_Log",    ylab = "Log labor share")
)
for (spec in fig_a5_specs){
  df <- ES_S2_main_full |>
    filter(sample == "le5_panelsize0",
           dep_var == spec$dep,
           interaction_group == "none",
           is.na(description)) |>
    mutate(grp = "none")
  if (nrow(df) == 0){ warning("No rows for fig A5 / ", spec$dep); next }
  p <- plot_event(df, default_map, "grp") +
    theme(legend.position = "none") +
    labs(y = spec$ylab)
  save_pdf(p, spec$file, h_in = 5, w_in = 6)
}

# =========================================================================
# FIG A6 — MLO firm characteristics, ROLLING WINDOW [tD-7, tD-5]
# =========================================================================
fig_a6_specs <- list(
  list(dep = "prod_res_ma3_dlag4",  file = "event_firmfeat_prod_resRoll_mlo_Log",     ylab = "Productivity"),
  list(dep = "ln_apl_ma3_dlag4",    file = "event_firmfeat_ln_aplRoll_mlo_Log",       ylab = "Log VA / worker"),
  list(dep = "ln_lshare_ma3_dlag4", file = "event_firmfeat_ln_LshareRoll_mlo_Log",    ylab = "Log labor share")
)
for (spec in fig_a6_specs){
  df <- ES_S2_main_full |>
    filter(sample == samp_mlo,
           dep_var == spec$dep,
           interaction_group == "none",
           is.na(description)) |>
    mutate(grp = "none")
  if (nrow(df) == 0){ warning("No rows for fig A6 / ", spec$dep); next }
  p <- plot_event(df, default_map, "grp") +
    theme(legend.position = "none") +
    labs(y = spec$ylab)
  save_pdf(p, spec$file, h_in = 5, w_in = 6)
}

# =========================================================================
# FIG A7 — Binscatter: firm AKM ~ ln(VA/worker), firm AKM ~ productivity
# =========================================================================
# S1-only (folder absent from S2). Use fe0215_mlo_le as the firm AKM measure.
binsc <- read_csv(file.path(path_S1, "fig_a7-binned_scatter/productivity_akm.csv"),
                  show_col_types = FALSE)

plot_binscatter <- function(xname_keep, file_out, xlab){
  d <- binsc |> filter(xname == xname_keep) |>
    select(quantile, fe0215_mlo_le, mean_xval) |>
    rename(akm = fe0215_mlo_le, x = mean_xval)
  fit_d <- d |> filter(quantile > 5, quantile < 50)
  fit <- lm(akm ~ x, data = fit_d)
  slope <- coef(fit)[2]

  p <- ggplot(d, aes(x = x, y = akm)) +
    geom_point(color = "#1e2d53", size = 2.2) +
    geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],
                color = "grey60", linewidth = 0.6) +
    labs(x = xlab, y = "Firm wage premium (AKM)") +
    annotate("label", x = -Inf, y = Inf,
             label = sprintf("slope = %.3f", slope),
             hjust = -0.05, vjust = 1.2, label.size = 0.2,
             fill = "white", color = "grey25", size = 4) +
    theme_choclow()
  save_pdf(p, file_out, h_in = 5, w_in = 6)
}
plot_binscatter("ln_apl_mean_0215",   "firm_LE_binscatter_ln_apl",
                "Log of value-added per worker (mean 2002-2015)")
plot_binscatter("prod_res_mean_0215", "firm_LE_binscatter_prod",
                "Productivity (residual, mean 2002-2015)")

# =========================================================================
# FIG A8 — AKM FE distribution (firm/sample comparison)
# =========================================================================
# Source: misc-descriptive_statistics/quantiles_split.csv (S2).
qs <- read_csv(file.path(path_S2, "misc-descriptive_statistics/quantiles_split.csv"),
               show_col_types = FALSE)

# Use the FE-on-le2002-2015 (fe0215_mlo_le) split into 5 quintiles, for
# samples le5_panelsize0 (LE) and panelpanSIZE2 (MLO).
fA8_dat <- qs |>
  filter(type_akm == "fe0215_mlo_le_q5_full",
         sample %in% c("le5_panelsize0", "panelpanSIZE2")) |>
  group_by(sample) |>
  mutate(share = n_ind / sum(n_ind, na.rm = TRUE)) |>
  ungroup() |>
  mutate(sample_lab = recode(sample,
    le5_panelsize0 = "LE",
    panelpanSIZE2  = "MLO"))

p_a8 <- ggplot(fA8_dat, aes(x = factor(quantile), y = share, fill = sample_lab)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(LE = "#91353b", MLO = "#008bbc"),
                    name = "Sample") +
  labs(x = "AKM firm-FE quintile (population)",
       y = "Share of workers in sample") +
  theme_choclow()
save_pdf(p_a8, "sample_FEdist", h_in = 5, w_in = 7)

# =========================================================================
# FIG A9 / A10 — Raw means of annual earnings / hours
# =========================================================================
# Source: event_studies-main_samples/le5_panelsize0-res.csv (S2);
# plot mean_treated and mean_control by distance for sbr/nbheur.
plot_raw_means <- function(dep, file_out, ylab){
  d <- ES_S2_main |>
    filter(sample == "le5_panelsize0",
           dep_var == dep,
           interaction_group == "none",
           is.na(description)) |>
    select(distance, mean_treated, mean_control) |>
    distinct() |>
    pivot_longer(c(mean_treated, mean_control),
                 names_to = "group", values_to = "mean_value") |>
    mutate(group = recode(group,
      mean_treated = "Displaced",
      mean_control = "Control"))
  if (nrow(d) == 0){ warning("No rows for raw means / ", dep); return(invisible(NULL)) }
  p <- ggplot(d, aes(x = distance, y = mean_value, color = group, shape = group,
                     linetype = group)) +
    geom_point(size = 3.5) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = event_cutoff, color = "grey30", linewidth = 0.6) +
    scale_x_continuous(breaks = -5:6, limits = c(-5.5, 6.5),
                       expand = expansion(add = 0)) +
    scale_color_manual(values = c(Displaced = "#91353b", Control = "#1e2d53"),
                       name = "") +
    scale_shape_manual(values = c(Displaced = 19, Control = 17), name = "") +
    scale_linetype_manual(values = c(Displaced = "solid", Control = "dashed"),
                          name = "") +
    labs(x = "Distance to displacement", y = ylab) +
    theme_choclow()
  save_pdf(p, file_out, h_in = 5, w_in = 7)
}
plot_raw_means("sbr",    "LEAllraw_sbr",   "Annual earnings (euros)")
plot_raw_means("nbheur", "LEAllraw_hours", "Annual hours worked")

# =========================================================================
# FIG D — MLO Sample Sector / Occupation (C_MLO30.tex)
# =========================================================================
# Same as Fig A1 but for the MLO sample (panelpanSIZE2).
fD_sec <- plot_balance_distribution(desc_S2, "sec_a10", sec_a10_lab, "panelpanSIZE2")
fD_occ <- plot_balance_distribution(desc_S2, "cs1",     cs1_lab,     "panelpanSIZE2")
save_pdf(fD_sec, "MLOAlldesc_sec",   h_in = 4, w_in = 9)
save_pdf(fD_occ, "MLOAlldesc_occup", h_in = 4, w_in = 9)

# =========================================================================
# FIG D (negotiation) — Loss in premium for accords / elections sub-samples
# =========================================================================
# Hybrid (Fig 1 analog) on the elec/acco sub-samples. Sample names:
#   accords  -> le5_panelsize0-acco_alt
#   elections-> le5_panelsize0-election
# Individual outcomes & FE both come from the elec_samples folder (which has
# them on both S1 and S2). FE -> S1, individual -> S2.
ES_S2_elec <- load_event_studies(path_S2, "event_studies-elec_samples")

build_loss_premium <- function(samp_str, file_out){
  fig_indiv <- ES_S2_elec |>
    filter(sample == samp_str,
           dep_var %in% c("lnsbrhour", "lnnbheur", "lnsbr"),
           interaction_group == "none",
           is.na(description)) |>
    select(distance, beta, std_error, dep_var_label)
  fig_firm <- ES_S1_elec |>
    filter(sample == samp_str,
           dep_var == "fe0215_mlo_le",
           interaction_group == "none") |>
    select(distance, beta, std_error, dep_var_label)
  if (nrow(fig_indiv) == 0 || nrow(fig_firm) == 0){
    warning("Missing rows for D figure / ", samp_str); return(invisible(NULL))
  }
  fig <- bind_rows(fig_firm, fig_indiv) |>
    mutate(dep_var_label = factor(dep_var_label,
      levels = c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings")))
  p <- plot_event(fig, main_map, "dep_var_label")
  save_pdf(p, file_out, h_in = 5.5, w_in = 8.5)
}
build_loss_premium("le5_panelsize0-acco_alt", "LE_accords_age_roblabor")
build_loss_premium("le5_panelsize0-election", "LE_elections_roblabor")

cat("Done. PDFs written to:\n  ", out_dir, "\n", sep = "")
