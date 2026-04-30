# Robustness for referee 1, item 3(d): wage decomposition by sex.
# Produces 2 PDFs in 03_Draft/graphs/2026-04/:
#   fig_1_wage_decomposition_men.pdf    (sx:1)
#   fig_1_wage_decomposition_women.pdf  (sx:0)
#
# Sourcing rule: worker outcomes from S2; firm wage premium from S1.
# Sex coding verified by baseline lnsbr means: sx:1 (higher wage) = men, sx:0 = women.

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

label_dep <- function(x){
  case_when(
    x == "lnsbrhour"     ~ "Log Hourly Wage",
    x == "lnnbheur"      ~ "Log Hours",
    x == "lnsbr"         ~ "Log Earnings",
    x == "fe0215_mlo_le" ~ "Firm Wage Premium",
    TRUE ~ x)
}

read_S2 <- function(){
  files <- list.files(file.path(path_S2, "event_studies-main_samples"),
                      full.names = TRUE, pattern = "\\.csv$")
  ES <- map_df(files, read_csv, show_col_types = FALSE)
  if ("treatment_path" %in% names(ES)) ES <- filter(ES, treatment_path)
  ES
}
read_S1 <- function(){
  files <- list.files(file.path(path_S1, "event_studies-main_samples"),
                      full.names = TRUE, pattern = "\\.csv$")
  ES <- map_df(files, read_csv, show_col_types = FALSE)
  # S1 omits the d = -1 reference period (implicit beta = 0). Insert it
  # explicitly so plots show a continuous timeline through the cutoff,
  # mirroring choclow_export-main/src/1-merge_exports.R.
  ES |>
    group_by(across(any_of(c("sample", "dep_var", "interaction_group", "description")))) |>
    group_modify(~ add_row(.x, .before = 0,
                           distance = -1L, beta = 0,
                           std_error = NA_real_)) |>
    ungroup()
}

ES_S1 <- read_S1()
ES_S2 <- read_S2()

build_fig1 <- function(sex_code) {
  indiv <- ES_S2 |>
    filter(sample == "le5_panelsize0",
           interaction_group == paste0("sx:", sex_code),
           dep_var %in% c("lnsbrhour", "lnnbheur", "lnsbr"),
           is.na(description)) |>
    mutate(dep_var_label = label_dep(dep_var)) |>
    select(distance, beta, std_error, dep_var_label)

  firm <- ES_S2 |>
    filter(sample == "le5_panelsize0",
           interaction_group == paste0("sx:", sex_code),
           dep_var == "fe0215_mlo_le",
           is.na(description)) |>
    mutate(dep_var_label = label_dep(dep_var)) |>
    select(distance, beta, std_error, dep_var_label)

  if (nrow(indiv) == 0 || nrow(firm) == 0) {
    stop("Missing rows for sex_code=", sex_code,
         " (n_indiv=", nrow(indiv), ", n_firm=", nrow(firm), ")")
  }

  bind_rows(firm, indiv) |>
    mutate(dep_var_label = factor(dep_var_label,
      levels = c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings")))
}

get_beta <- function(df, label, d) {
  v <- df$beta[df$dep_var_label == label & df$distance == d]
  if (length(v) == 0) NA_real_ else v[1]
}
pct <- function(num, den) sprintf("%.0f", 100 * num / den)

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

# sx:1 = men (higher baseline earnings), sx:0 = women (verified by mean lnsbr at d=-2)
plot_fig1(build_fig1(sex_code = 1), "fig_1_wage_decomposition_men")
plot_fig1(build_fig1(sex_code = 0), "fig_1_wage_decomposition_women")

cat("Done. PDFs written to:\n  ", out_dir, "\n", sep = "")
