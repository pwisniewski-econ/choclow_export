# Robustness for referee 3 (Camille's punch list, R3.8 / R3.9 / R3.10 / R3.11 / R3.14).
# Produces figures and a table for the R3 letter appendix.
#
#   R3.10 -- HHI heterogeneity: firm wage premium, productivity, labor share
#            split by HHI quartile (4 lines / panel).
#   R3.11 -- Recession heterogeneity: same outcomes, recession (2008-2009)
#            vs non-recession (2005-2007 and 2010-2012).
#   R3.14 -- No-growth firms: Fig 1 (wage decomposition) and Fig 2
#            (productivity / labor share / firm premium) on le5_panelsize40,
#            which is restricted to no-growth firms (cf. 06-lic_eco-matching.do
#            line 47-51: `keep if no_growth==1`).
#   R3.9  -- Firm age and worker tenure differences between origin and
#            destination firms (descriptive event-study coefficients,
#            written to a small TeX table).
#   R3.8  -- No new figure: refers to the existing event-study of
#            origin-firm productivity that is already in the paper appendix
#            (fig:event_firmfeat_prod_res_LE_Roll).

library(tidyverse)
library(readr)

path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
path_S2 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260423"
out_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/03_Draft/graphs/2026-04"
tab_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/03_Draft/tables/2026-04/r3_robustness"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

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

# Loader: same convention as body.R (insert d=-1 reference row for S1)
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
  ES
}

label_dep <- function(x){
  case_when(
    x == "lnsbrhour"     ~ "Log Hourly Wage",
    x == "lnnbheur"      ~ "Log Hours",
    x == "lnsbr"         ~ "Log Earnings",
    x == "fe0215_mlo_le" ~ "Firm Wage Premium",
    x == "prod_res_ma3"  ~ "Productivity",
    x == "ln_lshare_ma3" ~ "Labor Share of Value-added",
    x == "ln_apl_ma3"    ~ "Value-added per Worker",
    TRUE ~ x)
}

plot_event <- function(DF, scale_map, group_var, legend_nrow = 2,
                       x_breaks = -5:6){
  DF |>
    mutate(ci_lower = beta - ci_scalar * std_error,
           ci_upper = beta + ci_scalar * std_error) |>
    ggplot(aes(x = distance, y = beta, ymin = ci_lower, ymax = ci_upper,
               color = .data[[group_var]],
               linetype = .data[[group_var]],
               shape = .data[[group_var]])) +
    geom_point(size = 3.0) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = event_cutoff, color = "grey30", linewidth = 0.6) +
    geom_hline(yintercept = 0,           color = "grey30", linewidth = 0.6) +
    geom_errorbar(width = 0.2, linewidth = 0.6, linetype = "solid") +
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

ES_S1 <- load_event_studies(path_S1, "event_studies-main_samples")
ES_S2 <- load_event_studies(path_S2, "event_studies-main_samples")

#=================================================================
# R3.14 -- No-growth firms (le5_panelsize40)
#=================================================================
# Fig 1 (wage decomposition): all 4 curves from S2 (per the S2 convention).
# Fig 2 (firm outcomes): from S1, overall effect only (no within/between).
build_fig1_sample <- function(sample_name) {
  d <- ES_S2 |>
    filter(sample == sample_name,
           interaction_group == "none",
           is.na(description),
           dep_var %in% c("lnsbrhour", "lnnbheur", "lnsbr", "fe0215_mlo_le")) |>
    mutate(dep_var_label = label_dep(dep_var)) |>
    select(distance, beta, std_error, dep_var_label) |>
    mutate(dep_var_label = factor(dep_var_label,
      levels = c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings")))
  d
}

# Fig 1 no-growth
df <- build_fig1_sample("le5_panelsize40")
p <- plot_event(df, main_map, "dep_var_label") +
  scale_y_continuous(breaks = seq(-1, .2, .2), limits = c(-1, .2))
save_pdf(p, "fig_1_wage_decomposition_nogrowth")

# Fig 2 no-growth -- overall-effect lines only
default_map <- list(
  color    = c(`Productivity` = "#1e2d53",
               `Labor Share of Value-added` = "#82c0e9",
               `Firm Wage Premium` = "#91353b"),
  shape    = c(`Productivity` = 19,
               `Labor Share of Value-added` = 15,
               `Firm Wage Premium` = 17),
  linetype = c(`Productivity` = "solid",
               `Labor Share of Value-added` = "solid",
               `Firm Wage Premium` = "solid")
)

build_fig2_sample <- function(sample_name) {
  ES_S1 |>
    filter(sample == sample_name,
           interaction_group == "none",
           is.na(description),
           dep_var %in% c("prod_res_ma3", "ln_lshare_ma3", "fe0215_mlo_le")) |>
    mutate(dep_var_label = label_dep(dep_var)) |>
    select(distance, beta, std_error, dep_var_label) |>
    mutate(dep_var_label = factor(dep_var_label,
      levels = c("Productivity", "Labor Share of Value-added", "Firm Wage Premium")))
}

df_fig2 <- build_fig2_sample("le5_panelsize40")
panel_breaks <- if (max(df_fig2$distance, na.rm = TRUE) >= 6) -5:6 else -5:5
p2 <- plot_event(df_fig2, default_map, "dep_var_label", x_breaks = panel_breaks)
save_pdf(p2, "fig_2_combined_nogrowth", h_in = 5, w_in = 6)

#=================================================================
# R3.11 -- Recession heterogeneity (an_cl interaction in LE panelsize0)
#=================================================================
# Aggregate years into Recession (2008, 2009) vs Non-recession (the other
# six years 2005-07, 2010-12) by simple average of betas; SE are the
# Frisch-style sqrt-of-mean-of-squared-SEs (assumes independence across
# years; conservative because cohorts are non-overlapping samples).

agg_year_groups <- function(es, dep_var_pick, sample_name = "le5_panelsize0",
                            recession_years = c(2008, 2009)) {
  is_S2 <- "treatment_path" %in% names(es)
  ig_col <- "interaction_group"
  group_col <- function(year) sprintf("an_cl:%d", year)
  rec_groups <- group_col(recession_years)

  d <- es |>
    filter(sample == sample_name,
           dep_var == dep_var_pick,
           is.na(description),
           grepl("^an_cl:[0-9]+$", interaction_group)) |>
    mutate(year = as.integer(sub("^an_cl:", "", interaction_group)),
           group = if_else(year %in% recession_years,
                           "Recession (2008-2009)",
                           "Non-recession (2005-07, 2010-12)"))

  d |>
    group_by(group, distance) |>
    summarise(beta = mean(beta, na.rm = TRUE),
              std_error = sqrt(mean(std_error^2, na.rm = TRUE) /
                                 sum(!is.na(std_error))),
              .groups = "drop") |>
    filter(is.finite(beta))
}

recession_map <- list(
  color    = c(`Recession (2008-2009)` = "#91353b",
               `Non-recession (2005-07, 2010-12)` = "#1e2d53"),
  shape    = c(`Recession (2008-2009)` = 19,
               `Non-recession (2005-07, 2010-12)` = 17),
  linetype = c(`Recession (2008-2009)` = "solid",
               `Non-recession (2005-07, 2010-12)` = "dashed")
)

# Plot helper that takes the aggregated dataset
plot_recession <- function(es_src, dep_var_pick, file_stub) {
  d <- agg_year_groups(es_src, dep_var_pick)
  if (nrow(d) == 0) return(invisible(NULL))
  p <- d |>
    mutate(group = factor(group,
                          levels = c("Recession (2008-2009)",
                                     "Non-recession (2005-07, 2010-12)"))) |>
    rename(group_lbl = group) |>
    plot_event(recession_map, "group_lbl",
               x_breaks = if (max(d$distance, na.rm = TRUE) >= 6) -5:6 else -5:5)
  save_pdf(p, file_stub, h_in = 5, w_in = 6)
}

# Worker outcomes from S2; firm outcomes from S1 (same source mapping as paper)
plot_recession(ES_S2, "fe0215_mlo_le", "fig_recession_firm_premium")
plot_recession(ES_S2, "lnsbr",         "fig_recession_log_earnings")
plot_recession(ES_S1, "prod_res_ma3",  "fig_recession_productivity")
plot_recession(ES_S1, "ln_lshare_ma3", "fig_recession_labor_share")

#=================================================================
# R3.10 -- HHI heterogeneity (le5_panelsize0, hhi quartiles)
#=================================================================
# S1 has hhi_zecs1_ma3_q4:1..4; S2 has hhi_zecs1_ma3_dlag4_q4:1..4.

hhi_map <- list(
  color    = c(`Q1 (low HHI)` = "#1e2d53",
               `Q2`           = "#5c7ba1",
               `Q3`           = "#c98583",
               `Q4 (high HHI)`= "#91353b"),
  shape    = c(`Q1 (low HHI)` = 19,
               `Q2`           = 17,
               `Q3`           = 15,
               `Q4 (high HHI)`= 18),
  linetype = c(`Q1 (low HHI)` = "solid",
               `Q2`           = "dashed",
               `Q3`           = "dashed",
               `Q4 (high HHI)`= "solid")
)

plot_hhi <- function(es_src, dep_var_pick, file_stub, hhi_pattern) {
  d <- es_src |>
    filter(sample == "le5_panelsize0",
           dep_var == dep_var_pick,
           is.na(description),
           grepl(hhi_pattern, interaction_group)) |>
    mutate(q = sub(".*_q4:", "", interaction_group),
           group = factor(case_when(
             q == "1" ~ "Q1 (low HHI)",
             q == "2" ~ "Q2",
             q == "3" ~ "Q3",
             q == "4" ~ "Q4 (high HHI)",
             TRUE ~ q),
             levels = c("Q1 (low HHI)", "Q2", "Q3", "Q4 (high HHI)"))) |>
    select(distance, beta, std_error, group)
  if (nrow(d) == 0) return(invisible(NULL))
  p <- d |>
    plot_event(hhi_map, "group",
               legend_nrow = 1,
               x_breaks = if (max(d$distance, na.rm = TRUE) >= 6) -5:6 else -5:5)
  save_pdf(p, file_stub, h_in = 5, w_in = 6.5)
}

plot_hhi(ES_S2, "fe0215_mlo_le", "fig_hhi_firm_premium",
         "^hhi_zecs1_ma3_dlag4_q4:[1-4]$")
plot_hhi(ES_S2, "lnsbr",         "fig_hhi_log_earnings",
         "^hhi_zecs1_ma3_dlag4_q4:[1-4]$")
plot_hhi(ES_S1, "prod_res_ma3",  "fig_hhi_productivity",
         "^hhi_zecs1_ma3_q4:[1-4]$")
plot_hhi(ES_S1, "ln_lshare_ma3", "fig_hhi_labor_share",
         "^hhi_zecs1_ma3_q4:[1-4]$")

#=================================================================
# R3.9 -- Firm age and tenure differences (descriptive table)
#=================================================================
# Pull event-study coefficients for age_firm, year_cre, mean_tenure
# at d = -2, 0, 2, 5 (LE panelsize0, none).
tab_rows <- ES_S2 |>
  filter(sample == "le5_panelsize0",
         interaction_group == "none",
         is.na(description),
         dep_var %in% c("age_firm", "year_cre", "mean_tenure_eqtp_ma3"),
         distance %in% c(-2L, 0L, 2L, 5L)) |>
  mutate(label = case_when(
           dep_var == "age_firm"             ~ "Firm age (years)",
           dep_var == "year_cre"             ~ "Firm year of creation",
           dep_var == "mean_tenure_eqtp_ma3" ~ "Mean worker tenure (years)"),
         star = case_when(
           is.na(std_error) ~ "",
           abs(beta / std_error) > 2.576 ~ "***",
           abs(beta / std_error) > 1.960 ~ "**",
           abs(beta / std_error) > 1.645 ~ "*",
           TRUE ~ "")) |>
  select(label, distance, beta, std_error, star)

tex_lines <- c(
  "% Generated by main_plots_2026-04_r3_robustness.R",
  "% R3.9 -- destination vs origin gap in firm age, year of creation, mean tenure",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & $d=-2$ & $d=0$ & $d=2$ & $d=5$ \\\\",
  "\\midrule"
)
for (lab in c("Firm age (years)", "Firm year of creation", "Mean worker tenure (years)")) {
  beta_row <- tab_rows |> filter(label == lab) |> arrange(distance)
  cells <- map2_chr(beta_row$beta, beta_row$star,
                    \(b, s) if (is.na(b)) "--" else sprintf("$%+.3f$%s", b, s))
  tex_lines <- c(tex_lines,
                 sprintf("%s & %s \\\\", lab, paste(cells, collapse = " & ")))
  se_cells <- map_chr(beta_row$std_error,
                      \(s) if (is.na(s)) "" else sprintf("(%.3f)", s))
  tex_lines <- c(tex_lines,
                 sprintf("        & %s \\\\", paste(se_cells, collapse = " & ")))
}
tex_lines <- c(tex_lines, "\\bottomrule", "\\end{tabular}")
writeLines(tex_lines, file.path(tab_dir, "r3_firm_age_tenure.tex"))

cat("Done. Outputs:\n")
cat("  PDFs : ", out_dir, "\n", sep = "")
cat("  Table: ", file.path(tab_dir, "r3_firm_age_tenure.tex"), "\n", sep = "")
