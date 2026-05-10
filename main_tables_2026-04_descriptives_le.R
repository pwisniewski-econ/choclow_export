# Regenerate the LE descriptive matching-balance table for the EJ R1
# revision (LE5 sample = le5_panelsize0). Produces:
#
#   EJ_R1/tables-2026/LEAllmatchvar_quantivar_2026-05.tex
#
# Source: misc-descriptive_statistics/matching_balance.csv from CASD export
# Export_20260331; annual gross earnings at d=-2 from
# event_studies-main_samples/le5_panelsize0-res.csv (Export_20260423).
#
# Replaces the legacy LEAllmatchvar_quantivar_202402.tex inputted in
# A_app_tables_figures.tex line 290. The 2024-02 vintage was for the
# OLD LE sample (age 25-60, ref t-1/t-2, fe0215_god) and pre-dates the
# EJ R1 sample redefinition (age 20-50, ref t-2/t-3, fe0215_le).
#
# Layout: combined matchvar+quantivar in a single table with bold
# section dividers, matching the existing paper layout.

library(tidyverse)
library(readr)

path_S1 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
path_S2 <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260423"
out_dir <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/tables-2026"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

bal <- read_csv(file.path(path_S1, "misc-descriptive_statistics/matching_balance.csv"),
                show_col_types = FALSE)

# Distinct origin firms (proxy for the firm-year events count).
# Source: tbl2-origin_destination/means-<sample>-fixed_all.csv from the same
# CASD export. n_distinct(sir) summed over origin=TRUE rows gives the count
# of distinct firms in the AKM-eligible panel that ever served as the origin
# of a displacement in the sample period. Close but not strictly equal to
# the count of distinct (firm, year) layoff events.
get_origin_firms <- function(sample_name, export_root) {
  f <- file.path(export_root,
                 "tbl2-origin_destination",
                 paste0("means-", sample_name, "-fixed_all.csv"))
  d <- read_csv(f, show_col_types = FALSE)
  d |> filter(origin) |> summarise(n = sum(nobs)) |> pull(n)
}

# Helpers ----
fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_sd  <- function(x, digits = 2) sprintf("(%s)", fmt_num(x, digits))
fmt_nd  <- function(x, digits = 3) sprintf("%+.3f", x)

build_row <- function(df, var_root, label, digits = 2) {
  mn <- df |> filter(name == var_root) |> slice(1)
  sd <- df |> filter(name == paste0(var_root, "_sd")) |> slice(1)
  if (nrow(mn) == 0) return(c(
    sprintf("%-50s & %s & %s & %s \\\\", label, "", "", ""),
    sprintf("%-50s & %s & %s & %s \\\\", "", "", "", "")
  ))
  c(
    sprintf("%-50s & %s & %s & %s \\\\",
            label,
            fmt_num(mn$Treated, digits),
            fmt_num(mn$Control, digits),
            fmt_nd(mn$`Norm. Diff.`)),
    sprintf("%-50s & %s & %s & %s \\\\",
            "",
            fmt_sd(sd$Treated, digits),
            fmt_sd(sd$Control, digits),
            "")
  )
}

# Override row builder when means/sd come from outside matching_balance.csv
build_row_ext <- function(label, treated_mean, control_mean, nd, treated_sd, control_sd, digits = 2) {
  fmt_or_dash <- function(x, fn) if (is.na(x)) "--" else fn(x, digits)
  c(
    sprintf("%-50s & %s & %s & %s \\\\",
            label,
            fmt_or_dash(treated_mean, fmt_num),
            fmt_or_dash(control_mean, fmt_num),
            if (is.na(nd)) "--" else fmt_nd(nd)),
    sprintf("%-50s & %s & %s & %s \\\\",
            "",
            fmt_or_dash(treated_sd, fmt_sd),
            fmt_or_dash(control_sd, fmt_sd),
            "")
  )
}

# LE5 = le5_panelsize0 ----
le <- bal |> filter(sample == "le5_panelsize0")
n_treated <- le$n_treated[1]
n_control <- le$n_control[1]
n_origin_firms <- get_origin_firms("le5_panelsize0", path_S1)

# Pull annual gross earnings (sbr) at d=-2 from S2 event-studies
es <- read_csv(file.path(path_S2, "event_studies-main_samples/le5_panelsize0-res.csv"),
               show_col_types = FALSE)
sbr_dm2 <- es |>
  filter(treatment_path,
         interaction_group == "none",
         is.na(description),
         dep_var == "sbr",
         distance == -2) |>
  slice(1)

rows_match <- c(
  build_row(le, "age_cl",    "Age",                              digits = 2),
  build_row(le, "sbrhour2",  "Hourly wage at $t^D-2$",            digits = 2),
  build_row(le, "sbrhour3",  "Hourly wage at $t^D-3$",            digits = 2),
  build_row(le, "nbheur2",   "Hours worked at $t^D-2$",           digits = 2),
  build_row(le, "nbheur3",   "Hours worked at $t^D-3$",           digits = 2),
  build_row(le, "nbsa_ent1", "\\# employees at firm at $t^D-1$",  digits = 1)
)

rows_quanti <- c(
  build_row(le, "sx",                       "Gender: Male",                                      digits = 2),
  build_row_ext("Annual gross earnings at $t^D-2$",
                treated_mean = sbr_dm2$mean_treated,
                control_mean = sbr_dm2$mean_control,
                nd = NA_real_, treated_sd = NA_real_, control_sd = NA_real_,
                digits = 2),
  build_row(le, "fe0215_le",                "Firm wage premium $\\widehat{\\psi}_{J(i,t)}$",     digits = 2),
  build_row(le, "hhi_comp_sirze_eqtp_ma3",  "Local labor market HHI",                            digits = 4),
  build_row(le, "year_cre",                 "Firm year of creation",                             digits = 1)
)

tex <- c(
  "% Generated by main_tables_2026-04_descriptives_le.R",
  "% Source: matching_balance.csv from CASD export Export_20260331",
  "% (sample = le5_panelsize0); event_studies-main_samples/le5_panelsize0-res.csv",
  "% from Export_20260423 (sbr at d=-2).",
  "% Replaces the stale LEAllmatchvar_quantivar_202402.tex.",
  "%",
  "% Note: SD and Imbens-Rubin normalized difference for annual gross",
  "% earnings are not exported in either CSV; reported as '--'.",
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{l*{3}{c}}",
  "\\hline\\hline",
  "            & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} \\\\",
  "            & \\multicolumn{1}{c}{LE} & \\multicolumn{1}{c}{Control} & \\multicolumn{1}{c}{Norm.\\ Diff.} \\\\",
  "\\hline",
  "\\multicolumn{4}{l}{\\textbf{Matching variables}} \\\\",
  rows_match,
  "                                                  &        &        &        \\\\",
  "\\multicolumn{4}{l}{\\textbf{Variables not included in matching algorithm}} \\\\",
  rows_quanti,
  "\\hline",
  sprintf("Obs (workers)              & %s & %s & %s \\\\",
          formatC(n_treated, format = "d", big.mark = ","),
          formatC(n_control, format = "d", big.mark = ","),
          formatC(n_treated + n_control, format = "d", big.mark = ",")),
  sprintf("Distinct origin firms      & %s &        &        \\\\",
          formatC(n_origin_firms, format = "d", big.mark = ",")),
  "\\hline\\hline",
  "\\end{tabular}",
  "}"
)

out <- file.path(out_dir, "LEAllmatchvar_quantivar_2026-05.tex")
writeLines(tex, out)
cat("Wrote:", out, "\n")
