# Generate appendix figures (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, plus the
# "loss in premium" robustness sub-figures from D_negociation.tex) as PDFs.
# Mirrors plots2026/main_plots_2026-04_body.R conventions.
#
# Figure-by-figure provenance is documented inline; see also
#   00_ExportsCASD/Export_20260423/updated_scripts/PLOTTING_STRATEGY.md
# for the broad rule (firm outcomes -> S1; individual outcomes -> S2).
#
# Output: EJ_R1/figures-2026/*.pdf
# Filenames mirror the legacy ones so the LaTeX path swap is mechanical.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
})

# Paths --------------------------------------------------------------------
path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
path_S2 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260423"
out_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/figures-2026"
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
# S1 omits the d = -1 reference period (implicit beta = 0). When loading S1
# we insert it explicitly so plots show a continuous timeline through the
# cutoff, mirroring the body script's loader. Without this, Fig A14 panels
# show a visible gap between d = -2 and d = 0.
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
# sec_a10 is built from NAF 2008 (2-digit) via the recode in
# 12-mlo30-matching.do:77 (and parallel scripts):
#   1: NAF  1- 3 Agriculture
#   2: NAF  5-39 Manufacturing, mining, utilities, water, waste
#   3: NAF 41-43 Construction
#   4: NAF 45-56 Wholesale/retail/transport/accommodation/food
#   5: NAF 58-63 Information & communication
#   6: NAF 64-66 Finance & insurance
#   7: NAF 68    Real estate
#   8: NAF 69-82 Professional, scientific & administrative services
#   9: NAF 84-88 Public administration, education, health, social
#  10: NAF 90-99 Arts, entertainment, household & other services
# Labels here MUST match this numerical recoding. Earlier versions used
# the NACE A10 short labels but applied them to the wrong codes; that
# silently produced wrong-labeled bars in LEAlldesc_sec.pdf and
# MLOAlldesc_sec.pdf prior to 2026-05.
sec_a10_lab <- c(
  "1"  = "Agriculture",
  "2"  = "Manufacturing",
  "3"  = "Construction",
  "4"  = "Trade, transp. & hotels",
  "5"  = "Information & com.",
  "6"  = "Finance",
  "7"  = "Real estate",
  "8"  = "Services to firms",
  "9"  = "Public / edu / health",
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

# Helper to plot a side-by-side bar chart of (treated, control, population)
# distributions for the sectoral figure. The population reference is the
# DADS panel 2005-2012 ("dads-2005_2012") computed at the worker level.
plot_balance_distribution_with_pop <- function(df, varname, label_vec, samp,
                                               pop_sample = "dads-2005_2012"){
  d <- df |>
    filter(variable == varname,
           sample %in% c(samp, pop_sample)) |>
    select(value, n_individuals, sample, treated)

  d <- d |>
    mutate(group = case_when(
      sample == pop_sample        ~ "Population (2005-2012)",
      treated == 1                ~ "Displaced",
      treated == 0                ~ "Control",
      TRUE                        ~ NA_character_
    )) |>
    filter(!is.na(group))

  shares <- d |>
    group_by(group) |>
    mutate(share = n_individuals / sum(n_individuals, na.rm = TRUE)) |>
    ungroup() |>
    mutate(value_chr = ifelse(is.na(value), "NA", as.character(value)),
           label = ifelse(value_chr %in% names(label_vec),
                          label_vec[value_chr],
                          ifelse(value_chr == "NA", "Missing", value_chr))) |>
    filter(!is.na(value)) |>
    mutate(label = factor(label, levels = unname(label_vec)),
           group = factor(group,
                          levels = c("Displaced", "Control",
                                     "Population (2005-2012)")))

  ggplot(shares, aes(x = label, y = share, fill = group)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.72) +
    scale_fill_manual(values = c(
        Displaced                = "#91353b",
        Control                  = "#1e2d53",
        `Population (2005-2012)` = "#a0a0a0"),
      name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "", y = "Share of workers") +
    theme_choclow() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))
}

# Helper: plot Estimation-sample (controls only) vs Population, for the
# sectoral figure. Per the matching design, controls and treated have
# identical pre-period sectoral distributions by construction (matching is
# exact on year x sec_a10 x size-quartile cells). Controls measured at the
# event year are very close to that pre-period composition because controls
# don't move much between t-1 and t. We therefore plot the control bar only
# and label it "Estimation sample" to avoid the spurious imbalance that
# arises from displaying the treated workers' year-of-event sector (which
# already reflects post-displacement reallocation).
plot_estimation_vs_population <- function(df, varname, label_vec, samp,
                                          pop_sample = "dads-2005_2012"){
  d <- df |>
    filter(variable == varname,
           sample %in% c(samp, pop_sample)) |>
    select(value, n_individuals, sample, treated)

  d <- d |>
    mutate(group = case_when(
      sample == pop_sample ~ "Panel DADS (2005-2012)",
      treated == 0         ~ "Estimation sample",
      TRUE                 ~ NA_character_
    )) |>
    filter(!is.na(group))

  shares <- d |>
    group_by(group) |>
    mutate(share = n_individuals / sum(n_individuals, na.rm = TRUE)) |>
    ungroup() |>
    mutate(value_chr = ifelse(is.na(value), "NA", as.character(value)),
           label = ifelse(value_chr %in% names(label_vec),
                          label_vec[value_chr],
                          ifelse(value_chr == "NA", "Missing", value_chr))) |>
    filter(!is.na(value)) |>
    mutate(label = factor(label, levels = unname(label_vec)),
           group = factor(group,
                          levels = c("Estimation sample",
                                     "Panel DADS (2005-2012)")))

  ggplot(shares, aes(x = label, y = share, fill = group)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.72) +
    scale_fill_manual(values = c(
        `Estimation sample`      = "#1e2d53",
        `Panel DADS (2005-2012)` = "#a0a0a0"),
      name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "", y = "Share of workers") +
    theme_choclow() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))
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

# Produce LE figures. The sector figure now also reports the DADS panel
# 2005-2012 population shares as a third bar; occupation and diploma
# remain a 2-bar Displaced-vs-Control comparison.
fA1_sec <- plot_estimation_vs_population(desc_S2, "sec_a10",
                                         sec_a10_lab, "le5_panelsize0")
fA1_occ <- plot_balance_distribution(desc_S2, "cs1",     cs1_lab,     "le5_panelsize0")
fA1_dip <- plot_balance_distribution(desc_S2, "dip_tot", dip_lab,     "le5_panelsize0")
save_pdf(fA1_sec, "LEAlldesc_sec",   h_in = 4, w_in = 10)
save_pdf(fA1_occ, "LEAlldesc_occup", h_in = 4, w_in = 9)
save_pdf(fA1_dip, "LEAlldesc_dip",   h_in = 4, w_in = 9)
ggsave(file.path("/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/figures-2026",
                 "LEAlldesc_sec.pdf"),
       fA1_sec, bg = "white", height = 4, width = 10, units = "in")

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
# FIG A7/A8 (full-period analogues) — LE & MLO firm characteristics,
# UNCONDITIONAL 2002-2015 mean of firm outcome (suffix _mean_0215)
# =========================================================================
# Use S2's _mean_0215 variants. Output filenames mirror the *Roll* convention
# but with "Full" instead of "Roll".

fig_full_le_specs <- list(
  list(dep = "prod_res_mean_0215",  file = "event_firmfeat_prod_resFull_Log",     ylab = "Productivity"),
  list(dep = "ln_apl_mean_0215",    file = "event_firmfeat_ln_aplFull_Log",       ylab = "Log VA / worker"),
  list(dep = "ln_lshare_mean_0215", file = "event_firmfeat_ln_LshareFull_Log",    ylab = "Log labor share")
)
for (spec in fig_full_le_specs){
  df <- ES_S2_main_full |>
    filter(sample == "le5_panelsize0",
           dep_var == spec$dep,
           interaction_group == "none",
           is.na(description)) |>
    mutate(grp = "none")
  if (nrow(df) == 0){ warning("No rows for fig Full LE / ", spec$dep); next }
  p <- plot_event(df, default_map, "grp") +
    theme(legend.position = "none") +
    labs(y = spec$ylab)
  save_pdf(p, spec$file, h_in = 5, w_in = 6)
}

fig_full_mlo_specs <- list(
  list(dep = "prod_res_mean_0215",  file = "event_firmfeat_prod_resFull_mlo_Log",     ylab = "Productivity"),
  list(dep = "ln_apl_mean_0215",    file = "event_firmfeat_ln_aplFull_mlo_Log",       ylab = "Log VA / worker"),
  list(dep = "ln_lshare_mean_0215", file = "event_firmfeat_ln_LshareFull_mlo_Log",    ylab = "Log labor share")
)
for (spec in fig_full_mlo_specs){
  df <- ES_S2_main_full |>
    filter(sample == samp_mlo,
           dep_var == spec$dep,
           interaction_group == "none",
           is.na(description)) |>
    mutate(grp = "none")
  if (nrow(df) == 0){ warning("No rows for fig Full MLO / ", spec$dep); next }
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
# FIG A8 / A11 — AKM FE distribution: BHMW vs prior literature
# =========================================================================
# BHMW samples: drawn from misc-descriptive_statistics/quantiles_split.csv (S2)
# using the contamination-corrected fe0215_mlo_le quintiles, for samples
# le5_panelsize0 (LE) and panelpanSIZE2 (MLO30).
#
# Literature numbers: hard-coded from histogram_antepremium.do (legacy
# 05_dofiles/), which was the script behind the 2022-02-11 version of this
# figure. Sources reported in the prior literature:
#   LMW  - Lachowska, Mas, Woodbury (AER 2020)
#   SWH  - Schmieder, von Wachter, Heining (AER 2023), boom and recession
#   BABGLS - Bertheau, Acabbi, Barcelo, Gulyas, Lombardi, Saggio (AER 2023)
qs <- read_csv(file.path(path_S2, "misc-descriptive_statistics/quantiles_split.csv"),
               show_col_types = FALSE)

bhmw <- qs |>
  filter(type_akm == "fe0215_mlo_le_q5_full",
         sample %in% c("le5_panelsize0", "panelpanSIZE2")) |>
  group_by(sample) |>
  mutate(share = 100 * n_ind / sum(n_ind, na.rm = TRUE)) |>
  ungroup() |>
  mutate(id = recode(sample,
    le5_panelsize0 = "LE",
    panelpanSIZE2  = "MLO")) |>
  select(id, quantile, share)

literature <- tribble(
  ~id,         ~quantile, ~share,
  "LMW",       1L,  9.9, "LMW",       2L,  9.4, "LMW",       3L, 18.9, "LMW",       4L, 19.8, "LMW",       5L, 42.0,
  "SWH boom",  1L,  2.05,"SWH boom",  2L,  5.97,"SWH boom",  3L, 15.2, "SWH boom",  4L, 36.5, "SWH boom",  5L, 40.3,
  "SWH rec.",  1L,  1.65,"SWH rec.",  2L,  6.1, "SWH rec.",  3L, 16.4, "SWH rec.",  4L, 37.4, "SWH rec.",  5L, 38.5,
  "BABGLS",    1L,  7.14,"BABGLS",    2L,  6.5, "BABGLS",    3L, 18.66,"BABGLS",    4L, 30.38,"BABGLS",    5L, 37.31
)

# Top-to-bottom row order: LE, MLO, LMW, SWH boom, SWH rec., BABGLS
fA8_dat <- bind_rows(bhmw, literature) |>
  mutate(id = factor(id,
                     levels = rev(c("LE", "MLO", "LMW",
                                    "SWH boom", "SWH rec.", "BABGLS"))),
         quantile = factor(quantile, levels = 1:5,
                           labels = c("Q1", "Q2", "Q3", "Q4", "Q5")))

# Palette mirrors Patryk's choclow style (see context/.../src/_utils.R).
pal_akm <- c(Q1 = "#A8A8A8", Q2 = "#D0D0D0", Q3 = "#ABDAF4",
             Q4 = "#80CFB9", Q5 = "#E6BCD3")

p_a8 <- ggplot(fA8_dat, aes(y = id, x = share, fill = quantile)) +
  geom_col(width = 0.75, color = "grey20", linewidth = 0.25,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(share >= 2.5, sprintf("%.1f%%", share), "")),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 4.2, color = "grey15") +
  scale_fill_manual(values = pal_akm, name = "AKM Quantile") +
  scale_x_continuous(breaks = seq(0, 100, by = 20),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0),
                     limits = c(0, 100.5)) +
  labs(x = NULL, y = NULL) +
  theme_choclow() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# Sized for a single-column article figure (~6.5 inches text width). At
# \linewidth in LaTeX the labels remain readable. Written to BOTH the
# legacy graphs path (some letters still look there) and the consolidated
# EJ_R1/figures-2026/ folder used by the paper.
save_pdf(p_a8, "sample_FEdist", h_in = 4.5, w_in = 7.5)
ggsave(file.path("/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/figures-2026",
                 "sample_FEdist.pdf"),
       p_a8, bg = "white", height = 4.5, width = 7.5, units = "in")

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
fD_sec <- plot_estimation_vs_population(desc_S2, "sec_a10",
                                        sec_a10_lab, "panelpanSIZE2")
fD_occ <- plot_balance_distribution(desc_S2, "cs1",     cs1_lab,     "panelpanSIZE2")
save_pdf(fD_sec, "MLOAlldesc_sec",   h_in = 4, w_in = 10)
save_pdf(fD_occ, "MLOAlldesc_occup", h_in = 4, w_in = 9)
ggsave(file.path("/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/figures-2026",
                 "MLOAlldesc_sec.pdf"),
       fD_sec, bg = "white", height = 4, width = 10, units = "in")

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
  # Acco sub-sample (le5_panelsize0-acco_alt) is restricted to LE workers
  # displaced after 2007, so we observe at most d=+4 with non-trivial sample
  # size. Plotting through d=+6 produces a huge CI on Log Earnings at d=+5,6
  # that blows out the y-axis. Restrict the x-axis to d in [-5, 4] for this
  # sample, mirroring choclow_export-main/src/3-ohter_plots.R Fig A12.
  if (samp_str == "le5_panelsize0-acco_alt"){
    fig <- fig |> filter(distance <= 4)
    x_breaks <- -5:4
  } else {
    x_breaks <- -5:6
  }
  p <- plot_event(fig, main_map, "dep_var_label", x_breaks = x_breaks)
  save_pdf(p, file_out, h_in = 5.5, w_in = 8.5)
}
build_loss_premium("le5_panelsize0-acco_alt", "LE_accords_age_roblabor")
build_loss_premium("le5_panelsize0-election", "LE_elections_roblabor")

# =========================================================================
# FIG A14 — Visual of Table A.14 (collective-agreement event studies)
# =========================================================================
# Produces 3 single-panel event-study PDFs corresponding to columns 1, 2,
# and 4 of the revised Table~\ref{tab:nego:LEnego_acc_result} (R2 minor
# comment on pre-trends). Source: event_studies-elec_samples/le5_panelsize0
# -acco_alt (S1, baseline specification, no controls).
#
# extwage_agreement_ind_07_alt (originally column 4 = "Extended wage
# agreement indicator") was dropped from both the table and this figure in
# the R1 revision. On the broader 20-50 / no-FT-filter sample its
# post-period coefficients collapse to ~-0.02 with CIs containing zero,
# while the pre-trend is ~+0.02; the outcome is no longer informative.
# Both legacy (EJ_R1/figures-2026) and consolidated
# (EJ_R1/figures-2026) destinations are written.
fig_a14_specs <- list(
  list(dep = "wage_agreement_ind_07_alt",    file = "fig_a14_wage_indicator",
       ylab = "Wage agreement (indicator)"),
  list(dep = "wage_agreement_num_07_alt",    file = "fig_a14_wage_num",
       ylab = "Wage agreements (count)"),
  list(dep = "hours_agreement_ind_07_alt",   file = "fig_a14_hours_indicator",
       ylab = "Hours agreement (indicator)")
)

for (spec in fig_a14_specs) {
  df <- ES_S1_elec |>
    filter(sample == "le5_panelsize0-acco_alt",
           dep_var == spec$dep,
           interaction_group == "none",
           is.na(description)) |>
    mutate(grp = "none")
  if (nrow(df) == 0) {
    warning("No rows for Fig A14 / ", spec$dep); next
  }
  p <- plot_event(df, default_map, "grp",
                  x_breaks = -5:4) +
    theme(legend.position = "none") +
    labs(y = spec$ylab)
  save_pdf(p, spec$file, h_in = 4.2, w_in = 6)
  ggsave(file.path("/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/figures-2026",
                   paste0(spec$file, ".pdf")),
         p, bg = "white", height = 4.2, width = 6, units = "in")
}

cat("Done. PDFs written to:\n  ", out_dir, "\n", sep = "")
