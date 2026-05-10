# Generate appendix tables (descriptives, comprehensive event-study,
# firm correlates, MLO descriptives, negotiation desc/results/robustness)
# as .tex fragments. Mirrors plots2026/main_tables_2026-04_body.R.
#
# Output: EJ_R1/tables-2026/{comprehensivetable,new}/
# Filenames mirror the legacy ones for trivial path swaps in LaTeX.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
})

# Paths --------------------------------------------------------------------
path_S1   <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260331"
path_S2   <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Layoff/00_ExportsCASD/Export_20260423"
out_root  <- "/Users/clementmalgouyres/Library/CloudStorage/Dropbox/Applications/Overleaf/JobDisplacement/EJ_R1/tables-2026"
out_comp  <- file.path(out_root, "comprehensivetable")
out_new   <- file.path(out_root, "new")
dir.create(out_comp, showWarnings = FALSE, recursive = TRUE)
dir.create(out_new,  showWarnings = FALSE, recursive = TRUE)

# Loaders ------------------------------------------------------------------
load_event_studies <- function(path_sortie, sub){
  files <- list.files(file.path(path_sortie, sub), full.names = TRUE,
                      pattern = "\\.csv$")
  ES <- map_df(files, read_csv, show_col_types = FALSE)
  if ("treatment_path" %in% names(ES)) ES <- filter(ES, treatment_path)
  ES
}

ES_S1_main <- load_event_studies(path_S1, "event_studies-main_samples")
ES_S2_main <- load_event_studies(path_S2, "event_studies-main_samples")
ES_S1_elec <- load_event_studies(path_S1, "event_studies-elec_samples")

# Formatting helpers -------------------------------------------------------
fmt3 <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))
fmt_paren3 <- function(x) ifelse(is.na(x), "", sprintf("(%.3f)", x))
fmt_int_comma <- function(x) ifelse(is.na(x), "", formatC(round(x), format = "d", big.mark = ","))
fmt_paren_int_comma <- function(x) ifelse(is.na(x), "", paste0("(", formatC(round(x), format = "d", big.mark = ","), ")"))

star <- function(p){
  if (is.null(p) || length(p) == 0 || is.na(p)) return("")
  if (p < 0.001) "\\sym{***}"
  else if (p < 0.01) "\\sym{**}"
  else if (p < 0.05) "\\sym{*}"
  else if (p < 0.10) "\\sym{+}"
  else ""
}

fmt3_star <- function(beta, pval) {
  if (is.na(beta)) return("")
  paste0(sprintf("%.3f", beta), star(pval))
}

# Helper to extract event-study row for a given sample/dep/distance
get_es <- function(DF, dep, samp, distances = -3:6, group = "none"){
  DF |>
    filter(sample == samp,
           dep_var == dep,
           interaction_group == group,
           is.na(description)) |>
    select(distance, beta, std_error, mean_treated, mean_control,
           n_obs, n_ind, r2, ar2,
           any_of(c("n_treated", "n_trated"))) |>
    distinct() |>
    complete(distance = distances) |>
    arrange(distance) |>
    filter(distance %in% distances)
}

# =========================================================================
# Comprehensive event-study tables (LE & MLO, worker & firm)
# =========================================================================
build_worker_table <- function(samp, ES_indiv, ES_firm, distances = -3:6){
  employed   <- get_es(ES_indiv, "employed",      samp, distances)
  sbr_lvl    <- get_es(ES_indiv, "sbr",           samp, distances)
  lnsbr      <- get_es(ES_indiv, "lnsbr",         samp, distances)
  lnnbheur   <- get_es(ES_indiv, "lnnbheur",      samp, distances)
  lnsbrhour  <- get_es(ES_indiv, "lnsbrhour",     samp, distances)
  fe_le      <- get_es(ES_firm,  "fe0215_mlo_le", samp, distances)

  worker_lines <- character()
  for (d in distances) {
    if (d == -1){
      worker_lines <- c(worker_lines,
        sprintf("$ d = %d $ &REF&REF&REF&REF&REF&REF&\\\\", d),
        "&&&&&&&\\\\")
      next
    }
    c1  <- fmt3(employed$beta[employed$distance == d])
    s1  <- fmt_paren3(employed$std_error[employed$distance == d])
    c2  <- fmt_int_comma(sbr_lvl$beta[sbr_lvl$distance == d])
    s2  <- fmt_paren_int_comma(sbr_lvl$std_error[sbr_lvl$distance == d])
    c3  <- fmt3(lnsbr$beta[lnsbr$distance == d])
    s3  <- fmt_paren3(lnsbr$std_error[lnsbr$distance == d])
    c4  <- fmt3(lnnbheur$beta[lnnbheur$distance == d])
    s4  <- fmt_paren3(lnnbheur$std_error[lnnbheur$distance == d])
    c5  <- fmt3(lnsbrhour$beta[lnsbrhour$distance == d])
    s5  <- fmt_paren3(lnsbrhour$std_error[lnsbrhour$distance == d])
    c6  <- fmt3(fe_le$beta[fe_le$distance == d])
    s6  <- fmt_paren3(fe_le$std_error[fe_le$distance == d])
    bAKM <- fe_le$beta[fe_le$distance == d]
    bHW  <- lnsbrhour$beta[lnsbrhour$distance == d]
    c7   <- if (length(bAKM) && length(bHW) && !is.na(bAKM) && !is.na(bHW) && bHW != 0)
              sprintf("%.3f", bAKM / bHW) else ""

    worker_lines <- c(worker_lines,
      sprintf("$ d = %d $ &%s&%s&%s&%s&%s&%s&%s\\\\", d, c1, c2, c3, c4, c5, c6, c7),
      sprintf("&%s&%s&%s&%s&%s&%s&\\\\", s1, s2, s3, s4, s5, s6))
  }

  # Footer rows: N (obs), # treated, R^2  — pull from one outcome each
  n_obs <- function(df) {
    v <- df$n_obs[!is.na(df$n_obs)]; if (length(v)) v[1] else NA_real_
  }
  n_treated <- function(df){
    nt_col <- intersect(c("n_treated", "n_trated"), names(df))
    if (length(nt_col) == 0) return(NA_real_)
    v <- df[[nt_col[1]]][!is.na(df[[nt_col[1]]])]
    if (length(v)) v[1] else NA_real_
  }
  r2_get <- function(df){
    v <- df$r2[!is.na(df$r2)]; if (length(v)) v[1] else NA_real_
  }

  N_row <- sprintf("$ N $ &%s&%s&%s&%s&%s&%s&\\\\",
    fmt_int_comma(n_obs(employed)),
    fmt_int_comma(n_obs(sbr_lvl)),
    fmt_int_comma(n_obs(lnsbr)),
    fmt_int_comma(n_obs(lnnbheur)),
    fmt_int_comma(n_obs(lnsbrhour)),
    fmt_int_comma(n_obs(fe_le)))
  Ntr_row <- sprintf("\\# treated &%s&%s&%s&%s&%s&%s&\\\\",
    fmt_int_comma(n_treated(employed)),
    fmt_int_comma(n_treated(sbr_lvl)),
    fmt_int_comma(n_treated(lnsbr)),
    fmt_int_comma(n_treated(lnnbheur)),
    fmt_int_comma(n_treated(lnsbrhour)),
    fmt_int_comma(n_treated(fe_le)))
  R2_row <- sprintf("$ R^2 $ &%s&%s&%s&%s&%s&%s&\\\\",
    fmt3(r2_get(employed)), fmt3(r2_get(sbr_lvl)),
    fmt3(r2_get(lnsbr)), fmt3(r2_get(lnnbheur)),
    fmt3(r2_get(lnsbrhour)), fmt3(r2_get(fe_le)))

  c("% Generated by main_tables_2026-04_appendix.R from CASD CSV exports.",
    "% Worker outcomes from S2 (Export_20260423); firm wage premium from S1 (Export_20260331).",
    "",
    "& \\multicolumn{2}{l}{Outcome in levels} & \\multicolumn{5}{l}{Outcomes in log} \\\\ \\cmidrule[1.5pt](lr){2-3} \\cmidrule[1.5pt](lr){4-8}",
    "&(1)&(2)&(3)&(4)&(5)&(6)&(7)\\\\",
    "Time to displ.&Employed&Earnings&Earnings&Hours&Hourly wage& Premium &Ratio (6)/(5)\\\\ \\midrule",
    worker_lines,
    "\\midrule",
    N_row, Ntr_row, R2_row)
}

build_firm_table <- function(samp, ES_main, ES_elec,
                              acco_samp, elec_samp, distances = -3:6){
  prod    <- get_es(ES_main, "prod_res_ma3", samp, distances)
  apl     <- get_es(ES_main, "ln_apl_ma3",   samp, distances)
  lshare  <- get_es(ES_main, "ln_lshare_ma3", samp, distances)
  acco    <- get_es(ES_elec, "wage_agreement_ind_07_alt", acco_samp, distances)
  elec    <- get_es(ES_elec, "participation_siren0912",   elec_samp, distances)

  firm_lines <- character()
  for (d in distances) {
    if (d == -1){
      firm_lines <- c(firm_lines,
        sprintf("$ d = %d $ &REF&REF&REF&REF&REF\\\\", d),
        "&&&&&\\\\")
      next
    }
    c1 <- fmt3(prod$beta[prod$distance == d]);     s1 <- fmt_paren3(prod$std_error[prod$distance == d])
    c2 <- fmt3(apl$beta[apl$distance == d]);       s2 <- fmt_paren3(apl$std_error[apl$distance == d])
    c3 <- fmt3(lshare$beta[lshare$distance == d]); s3 <- fmt_paren3(lshare$std_error[lshare$distance == d])
    c4 <- fmt3(acco$beta[acco$distance == d]);     s4 <- fmt_paren3(acco$std_error[acco$distance == d])
    c5 <- fmt3(elec$beta[elec$distance == d]);     s5 <- fmt_paren3(elec$std_error[elec$distance == d])
    firm_lines <- c(firm_lines,
      sprintf("$ d = %d $ &%s&%s&%s&%s&%s\\\\", d, c1, c2, c3, c4, c5),
      sprintf("&%s&%s&%s&%s&%s\\\\", s1, s2, s3, s4, s5))
  }

  n_obs <- function(df){ v <- df$n_obs[!is.na(df$n_obs)]; if (length(v)) v[1] else NA_real_ }
  n_treated <- function(df){
    nt_col <- intersect(c("n_treated", "n_trated"), names(df))
    if (length(nt_col) == 0) return(NA_real_)
    v <- df[[nt_col[1]]][!is.na(df[[nt_col[1]]])]
    if (length(v)) v[1] else NA_real_
  }
  r2_get <- function(df){ v <- df$r2[!is.na(df$r2)]; if (length(v)) v[1] else NA_real_ }

  N_row <- sprintf("$ N $ &%s&%s&%s&%s&%s\\\\",
    fmt_int_comma(n_obs(prod)),  fmt_int_comma(n_obs(apl)),
    fmt_int_comma(n_obs(lshare)), fmt_int_comma(n_obs(acco)),
    fmt_int_comma(n_obs(elec)))
  Ntr_row <- sprintf("\\# treated &%s&%s&%s&%s&%s\\\\",
    fmt_int_comma(n_treated(prod)), fmt_int_comma(n_treated(apl)),
    fmt_int_comma(n_treated(lshare)), fmt_int_comma(n_treated(acco)),
    fmt_int_comma(n_treated(elec)))
  R2_row <- sprintf("$ R^2 $ &%s&%s&%s&%s&%s\\\\",
    fmt3(r2_get(prod)),   fmt3(r2_get(apl)),
    fmt3(r2_get(lshare)), fmt3(r2_get(acco)),
    fmt3(r2_get(elec)))

  c("% Generated by main_tables_2026-04_appendix.R from CASD CSV exports.",
    "% Firm outcomes from S1 (Export_20260331).",
    "",
    "& \\multicolumn{3}{l}{Firm outcomes} & \\multicolumn{2}{l}{Negotiation variables} \\\\ \\cmidrule[1.5pt](lr){2-4} \\cmidrule[1.5pt](lr){5-6}",
    "&(1)&(2)&(3)&(4)&(5)\\\\",
    "Time to displ.&Productivity&VA / Worker&Labor share&Wage Agr. ind.&Election turnout\\\\ \\midrule",
    firm_lines,
    "\\midrule",
    N_row, Ntr_row, R2_row)
}

# LE comprehensive (already built for body, but appendix version covers
# pretrend distances too — d in {-3,...,6})
worker_le_tex <- build_worker_table("le5_panelsize0",
                                     ES_S2_main, ES_S1_main, distances = -3:6)
firm_le_tex   <- build_firm_table("le5_panelsize0",
                                   ES_S1_main, ES_S1_elec,
                                   "le5_panelsize0-acco_alt",
                                   "le5_panelsize0-election",
                                   distances = -3:6)
writeLines(worker_le_tex, file.path(out_comp, "table_worker_appendix_202402.tex"))
writeLines(firm_le_tex,   file.path(out_comp, "table_firm_appendix_202402.tex"))

# MLO comprehensive
worker_mlo_tex <- build_worker_table("panelpanSIZE2",
                                      ES_S2_main, ES_S1_main, distances = -3:6)
firm_mlo_tex   <- build_firm_table("panelpanSIZE2",
                                    ES_S1_main, ES_S1_elec,
                                    "panelpanSIZE2-acco_alt",
                                    "panelpanSIZE2-election",
                                    distances = -3:6)
writeLines(worker_mlo_tex, file.path(out_comp, "table_worker_mlo_appendix_202402.tex"))
writeLines(firm_mlo_tex,   file.path(out_comp, "table_firm_mlo_appendix_202402.tex"))

cat("Wrote: comprehensive worker / firm tables (LE + MLO).\n")

# =========================================================================
# Descriptive statistics: LE / MLO matching tables
# =========================================================================
mb <- read_csv(file.path(path_S2, "misc-descriptive_statistics/matching_balance.csv"),
               show_col_types = FALSE)

# Extract a (mean, sd, normdiff) triple given a base var name and sample
get_balance_row <- function(mb, base_var, samp){
  m  <- mb |> filter(name == base_var,         sample == samp) |> slice(1)
  sd <- mb |> filter(name == paste0(base_var, "_sd"), sample == samp) |> slice(1)
  if (nrow(m) == 0) return(NULL)
  list(m_treated = m$Treated, m_control = m$Control, normdiff = m$`Norm. Diff.`,
       sd_treated = sd$Treated, sd_control = sd$Control,
       n_treated = m$n_treated, n_control = m$n_control)
}

# Helper to format a row "label & m_t & m_c & normdiff" plus a SD line
balance_double_row <- function(mb, base_var, label, samp, fmt = "%.2f", sd_paren = TRUE){
  r <- get_balance_row(mb, base_var, samp)
  if (is.null(r)) return(c(sprintf("%s & & & \\\\", label), " & & & \\\\"))
  m_t  <- sprintf(fmt, r$m_treated);  m_c  <- sprintf(fmt, r$m_control)
  nd   <- sprintf("%.2f", r$normdiff)
  sd_t <- sprintf(fmt, r$sd_treated); sd_c <- sprintf(fmt, r$sd_control)
  if (sd_paren){ sd_t <- paste0("(", sd_t, ")"); sd_c <- paste0("(", sd_c, ")") }
  c(sprintf("%s & %s & %s & %s\\\\", label, m_t, m_c, nd),
    sprintf("            & %s & %s & \\\\", sd_t, sd_c))
}

build_le_desc_table <- function(mb, samp, treat_lab){
  rows_match <- list(
    list("age_cl",          "Age",                            "%.2f"),
    list("sbrhour2",        "Wage rate $t^D-1$",              "%.2f"),
    list("sbrhour3",        "Wage rate $t^D-2$",              "%.2f"),
    list("nbheur2",         "Hours worked $t^D-1$",           "%.2f"),
    list("nbheur3",         "Hours worked $t^D-2$",           "%.2f"),
    list("dads_eqtp_ma3",   "\\# employees at firm $t^D-1$",  "%.2f")
  )
  rows_extra <- list(
    list("sx",                "Gender: Male",                                            "%.2f"),
    list("mean_sbr_eqtp_ma3", "Gross earnings $t^D-1$",                                  "%.2f"),
    list("fe0215_mlo_le",     "Firm wage premium 02-15: $\\widehat{\\psi}_{J(i, t)}$",   "%.2f")
  )
  out <- c(
    "{",
    "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
    "\\begin{tabular}{l*{3}{c}}",
    "\\hline",
    "            &\\multicolumn{1}{c}{(1)}&\\multicolumn{1}{c}{(2)}&\\multicolumn{1}{c}{(3)}\\\\",
    sprintf("            &\\multicolumn{1}{c}{%s}&\\multicolumn{1}{c}{Control}&\\multicolumn{1}{c}{Norm. Diff.}\\\\", treat_lab),
    "\\hline",
    "",
    "\\multicolumn{4}{l}{\\textbf{Matching variables}} \\\\"
  )
  for (r in rows_match){
    out <- c(out, balance_double_row(mb, r[[1]], r[[2]], samp, r[[3]]))
  }
  out <- c(out,
    "            &             &           &            \\\\",
    "\\multicolumn{4}{l}{\\textbf{Variables not included in matching algorithm}} \\\\"
  )
  for (r in rows_extra){
    out <- c(out, balance_double_row(mb, r[[1]], r[[2]], samp, r[[3]]))
  }
  # Bottom block: N observations, events
  one <- mb |> filter(sample == samp) |> slice(1)
  n_t <- formatC(one$n_treated, format = "d", big.mark = "")
  n_c <- formatC(one$n_control, format = "d", big.mark = "")
  n_tot <- formatC(one$n_treated + one$n_control, format = "d", big.mark = "")
  out <- c(out,
    "\\hline",
    sprintf("Obs         & %s & %s & %s\\\\", n_t, n_c, n_tot),
    sprintf("Events      & %s &            &            \\\\", n_t),
    "\\hline",
    "\\end{tabular}",
    "}")
  out
}

writeLines(build_le_desc_table(mb, "le5_panelsize0", "LE"),
           file.path(out_root, "LEAllmatchvar_quantivar_202402.tex"))
cat("Wrote: LEAllmatchvar_quantivar_202402.tex\n")

# MLO matching (same structure but different labels)
build_mlo_match_table <- function(mb, samp){
  rows <- list(
    list("age_cl",        "Age",                            "%.2f"),
    list("sbrhour2",      "Wage rate $t^D_0-1$",            "%.2f"),
    list("sbrhour3",      "Wage rate $t^D_0-2$",            "%.2f"),
    list("nbheur2",       "Hours worked $t^D_0-1$",         "%.2f"),
    list("nbheur3",       "Hours worked $t^D_0-2$",         "%.2f"),
    list("dads_eqtp_ma3", "\\# employees at firm $t^D_0-1$","%.2f")
  )
  out <- c(
    "{",
    "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
    "\\begin{tabular}{l*{3}{c}}",
    "\\hline\\hline",
    "            &\\multicolumn{1}{c}{(1)}&\\multicolumn{1}{c}{(2)}&\\multicolumn{1}{c}{(3)}\\\\",
    "            &\\multicolumn{1}{c}{MLO}&\\multicolumn{1}{c}{Control}&\\multicolumn{1}{c}{Norm. Diff.}\\\\",
    "\\hline"
  )
  for (r in rows){
    out <- c(out, balance_double_row(mb, r[[1]], r[[2]], samp, r[[3]]))
  }
  one <- mb |> filter(sample == samp) |> slice(1)
  n_t <- formatC(one$n_treated, format = "d", big.mark = "")
  n_c <- formatC(one$n_control, format = "d", big.mark = "")
  n_tot <- formatC(one$n_treated + one$n_control, format = "d", big.mark = "")
  out <- c(out,
    "\\hline",
    sprintf("Obs         & %s & %s & %s\\\\", n_t, n_c, n_tot),
    sprintf("Events      & %s &            &            \\\\", n_t),
    "\\hline\\hline",
    "\\end{tabular}",
    "}")
  out
}
writeLines(build_mlo_match_table(mb, "panelpanSIZE2"),
           file.path(out_root, "MLOAllmatchvar_20210908.tex"))

build_mlo_quanti_table <- function(mb, samp){
  rows <- list(
    list("sx",                "Gender: Male",                                          "%.2f"),
    list("mean_sbr_eqtp_ma3", "Gross earnings",                                        "%.2f"),
    list("fe0215_mlo_le",     "Firm-wage premium 01-15: $\\widehat{\\psi}_{J(i, t)}$", "%.2f")
  )
  out <- c(
    "{",
    "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
    "\\begin{tabular}{l*{3}{c}}",
    "\\hline\\hline",
    "            &\\multicolumn{1}{c}{(1)}&\\multicolumn{1}{c}{(2)}&\\multicolumn{1}{c}{(3)}\\\\",
    "            &\\multicolumn{1}{c}{MLO}&\\multicolumn{1}{c}{Control}&\\multicolumn{1}{c}{Norm. Diff.}\\\\",
    "\\hline"
  )
  for (r in rows){
    out <- c(out, balance_double_row(mb, r[[1]], r[[2]], samp, r[[3]]))
  }
  one <- mb |> filter(sample == samp) |> slice(1)
  n_t <- formatC(one$n_treated, format = "d", big.mark = "")
  n_c <- formatC(one$n_control, format = "d", big.mark = "")
  n_tot <- formatC(one$n_treated + one$n_control, format = "d", big.mark = "")
  out <- c(out,
    "\\hline",
    sprintf("Obs         & %s & %s & %s\\\\", n_t, n_c, n_tot),
    sprintf("Events      & %s &            &            \\\\", n_t),
    "\\hline\\hline",
    "\\end{tabular}",
    "}")
  out
}
writeLines(build_mlo_quanti_table(mb, "panelpanSIZE2"),
           file.path(out_root, "MLOAllquantivar_20210908.tex"))

cat("Wrote: MLO descriptive tables.\n")

# =========================================================================
# Firm Pooled regressions: firm_Pooled_regression_fe0115 and firm_Pooled_nego_fe0115
# =========================================================================
# Source: tbl_a6-corr_akm/le5_panelsize0.csv (S1).
# Each row is a regressor in a specific formula. We pick formulas matching
# the legacy 8-column / 6-column structure.
ta6 <- read_csv(file.path(path_S1, "tbl_a6-corr_akm/le5_panelsize0.csv"),
                show_col_types = FALSE)

# Use cross-sectional, fixed-firm regs (sample = "le5_panelsize0-fixed",
# dep_var = "fe0215_god2") — matches the legacy "0115" outcome.
ta6f <- ta6 |> filter(sample == "le5_panelsize0-fixed",
                       dep_var == "fe0215_god2")

# Helper: get coef for (formula, var)
get_cf <- function(formula_match, var){
  r <- ta6f |> filter(formula == formula_match, variable == var) |> slice(1)
  if (nrow(r) == 0) return(NULL)
  list(beta = r$beta, se = r$std_error, pval = r$pval,
       n_obs = r$n_obs, r2 = r$r2, ar2 = r$ar2)
}

# Define the 8 columns. Variables match legacy:
#  (1) Employees (2) VA (3) VA/worker (4) Productivity
#  (5) Age>10y (6) Manufacturing (7) all 4 + before1994+maxmfg w/o sector FE
#  (8) col 7 + sector FE
specs8 <- list(
  list(formula = "fe0215_god2~ln_emp_mean_0215 | minmax_year",
       vars = list("Employees" = "ln_emp_mean_0215"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~ln_emp_mean_0215+ln_va_mean_0215 | minmax_year",
       vars = list("VA" = "ln_va_mean_0215"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~ln_emp_mean_0215+ln_apl_mean_0215 | minmax_year",
       vars = list("Employees" = "ln_emp_mean_0215", "VA per worker" = "ln_apl_mean_0215"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~ln_emp_mean_0215+prod_res_mean_0215 | minmax_year",
       vars = list("Employees" = "ln_emp_mean_0215", "Productivity" = "prod_res_mean_0215"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~before1994 | minmax_year",
       vars = list("Age $ > $ 10 years" = "before1994"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~maxmfg | minmax_year",
       vars = list("Manufacturing" = "maxmfg"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~ln_emp_mean_0215+prod_res_mean_0215+before1994+maxmfg | minmax_year",
       vars = list("Employees" = "ln_emp_mean_0215",
                   "Productivity" = "prod_res_mean_0215",
                   "Age $ > $ 10 years" = "before1994",
                   "Manufacturing" = "maxmfg"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~ln_emp_mean_0215+prod_res_mean_0215+before1994+maxmfg | naf2d_num_main+minmax_year",
       vars = list("Employees" = "ln_emp_mean_0215",
                   "Productivity" = "prod_res_mean_0215",
                   "Age $ > $ 10 years" = "before1994",
                   "Manufacturing" = "maxmfg"),
       sec_fe = TRUE)
)

# Some col 8 may not exist with that exact formula — check what's in there
# fall back to col 7 with sector FE if missing.
specs8 <- lapply(specs8, \(s){
  has <- nrow(ta6f |> filter(formula == s$formula)) > 0
  if (has) return(s)
  if (s$sec_fe){
    alt <- sub(" \\| minmax_year$", " | minmax_year+naf2d_num_main", s$formula)
    if (nrow(ta6f |> filter(formula == alt)) > 0) {
      s$formula <- alt; return(s)
    }
  }
  s
})

# Build firm_Pooled_regression_fe0115.tex
row_labels <- c("Employees", "VA", "VA per worker", "Productivity",
                "Age $ > $ 10 years", "Manufacturing")
get_cell_for <- function(spec, row_lab){
  v <- spec$vars[[row_lab]]
  if (is.null(v)) return(list(b = "", s = ""))
  cf <- get_cf(spec$formula, v)
  if (is.null(cf) || is.na(cf$beta)) return(list(b = "", s = ""))
  list(b = sprintf("%.3f", cf$beta), s = sprintf("(%.3f)", cf$se))
}

n_specs <- length(specs8)
build_pooled_table <- function(specs, row_labels, footers_extra = NULL){
  header <- paste0("                    &",
    paste(map_chr(seq_along(specs), \(i) sprintf("\\multicolumn{1}{c}{(%d)}", i)),
          collapse = "&"), "\\\\")
  rows <- c(header, "\\midrule")
  for (lab in row_labels){
    cells <- map(specs, get_cell_for, row_lab = lab)
    line_b <- paste0(sprintf("%-20s&", lab),
                     paste(map_chr(cells, "b"), collapse = "&"), "\\\\")
    line_s <- paste0("                    &",
                     paste(map_chr(cells, "s"), collapse = "&"), "\\\\")
    rows <- c(rows, line_b, line_s, "\\addlinespace")
  }
  rows <- c(rows, "\\midrule")
  # Sector FE row
  sec_row <- map_chr(specs, \(s) if (isTRUE(s$sec_fe)) "\\checkmark" else "")
  rows <- c(rows, paste0("Sector FE           &", paste(sec_row, collapse = "&"), "\\\\"))
  # Observations / R2 / Adj R2
  one_per_spec <- function(getter){
    map_chr(specs, \(s){
      cf <- ta6f |> filter(formula == s$formula) |> slice(1)
      if (nrow(cf) == 0) "" else getter(cf)
    })
  }
  N_vals  <- one_per_spec(\(cf) formatC(cf$n_obs, format = "d"))
  R2_vals <- one_per_spec(\(cf) sprintf("%.3f", cf$r2))
  AR_vals <- one_per_spec(\(cf) sprintf("%.3f", cf$ar2))
  rows <- c(rows,
    paste0("Observations        &", paste(N_vals,  collapse = "&"), "\\\\"),
    paste0("R$^{2}$               &", paste(R2_vals, collapse = "&"), "\\\\"),
    paste0("Adjusted R$^{2}$      &", paste(AR_vals, collapse = "&"), "\\\\"))
  rows
}

writeLines(build_pooled_table(specs8, row_labels),
           file.path(out_root, "firm_Pooled_regression_fe0115.tex"))
cat("Wrote: firm_Pooled_regression_fe0115.tex\n")

# Negotiation regressions: 6 columns
# (1) wage_agreement_ind_07 univariate
# (2) wage_agreement_ind_07 + ln_emp
# (3) col 1 + sector FE
# (4) participation_siren0912 univariate
# (5) participation_siren0912 + ln_emp
# (6) col 4 + sector FE
# Use samples le5_panelsize0-acco / le5_panelsize0-elec (these are sub-samples).
ta6_nego <- ta6 |> filter(dep_var == "fe0215_god2",
                           sample %in% c("le5_panelsize0-acco", "le5_panelsize0-elec"))

specs_nego <- list(
  list(formula = "fe0215_god2~wage_agreement_ind_07 | minmax_year",
       sample = "le5_panelsize0-acco",
       vars = list("Wage agreement ind." = "wage_agreement_ind_07"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~wage_agreement_ind_07+ln_emp_mean_0215 | minmax_year",
       sample = "le5_panelsize0-acco",
       vars = list("Wage agreement ind." = "wage_agreement_ind_07",
                   "Employees"           = "ln_emp_mean_0215"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~wage_agreement_ind_07 | minmax_year+naf2d_num_main",
       sample = "le5_panelsize0-acco",
       vars = list("Wage agreement ind." = "wage_agreement_ind_07"),
       sec_fe = TRUE),
  list(formula = "fe0215_god2~participation_siren0912 | minmax_year",
       sample = "le5_panelsize0-elec",
       vars = list("Election turnout" = "participation_siren0912"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~participation_siren0912+ln_emp_mean_0215 | minmax_year",
       sample = "le5_panelsize0-elec",
       vars = list("Election turnout" = "participation_siren0912",
                   "Employees"        = "ln_emp_mean_0215"),
       sec_fe = FALSE),
  list(formula = "fe0215_god2~participation_siren0912 | minmax_year+naf2d_num_main",
       sample = "le5_panelsize0-elec",
       vars = list("Election turnout" = "participation_siren0912"),
       sec_fe = TRUE)
)

# Slightly different access function: sample-aware
get_cf_nego <- function(formula_match, samp_match, var){
  r <- ta6_nego |> filter(formula == formula_match,
                            sample == samp_match,
                            variable == var) |> slice(1)
  if (nrow(r) == 0) return(NULL)
  list(beta = r$beta, se = r$std_error, pval = r$pval,
       n_obs = r$n_obs, r2 = r$r2, ar2 = r$ar2)
}

get_cell_for_nego <- function(spec, row_lab){
  v <- spec$vars[[row_lab]]
  if (is.null(v)) return(list(b = "", s = ""))
  cf <- get_cf_nego(spec$formula, spec$sample, v)
  if (is.null(cf) || is.na(cf$beta)) return(list(b = "", s = ""))
  list(b = sprintf("%.3f", cf$beta), s = sprintf("(%.3f)", cf$se))
}

build_nego_table <- function(specs, row_labels){
  header <- paste0("                    &",
    paste(map_chr(seq_along(specs), \(i) sprintf("\\multicolumn{1}{c}{(%d)}", i)),
          collapse = "&"), "\\\\")
  rows <- c(header, "\\midrule")
  for (lab in row_labels){
    cells <- map(specs, get_cell_for_nego, row_lab = lab)
    line_b <- paste0(sprintf("%-20s&", lab),
                     paste(map_chr(cells, "b"), collapse = "&"), "\\\\")
    line_s <- paste0("                    &",
                     paste(map_chr(cells, "s"), collapse = "&"), "\\\\")
    rows <- c(rows, line_b, line_s, "\\addlinespace")
  }
  rows <- c(rows, "\\midrule")
  sec_row <- map_chr(specs, \(s) if (isTRUE(s$sec_fe)) "\\checkmark" else "")
  rows <- c(rows, paste0("Sector FE           &", paste(sec_row, collapse = "&"), "\\\\"))
  one_per_spec <- function(getter){
    map_chr(specs, \(s){
      cf <- ta6_nego |> filter(formula == s$formula, sample == s$sample) |> slice(1)
      if (nrow(cf) == 0) "" else getter(cf)
    })
  }
  N_vals  <- one_per_spec(\(cf) formatC(cf$n_obs, format = "d"))
  R2_vals <- one_per_spec(\(cf) sprintf("%.3f", cf$r2))
  AR_vals <- one_per_spec(\(cf) sprintf("%.3f", cf$ar2))
  rows <- c(rows,
    paste0("Observations        &", paste(N_vals,  collapse = "&"), "\\\\"),
    paste0("R$^{2}$               &", paste(R2_vals, collapse = "&"), "\\\\"),
    paste0("Adjusted R$^{2}$      &", paste(AR_vals, collapse = "&"), "\\\\"))
  rows
}

nego_row_labels <- c("Wage agreement ind.", "Election turnout", "Employees")
writeLines(build_nego_table(specs_nego, nego_row_labels),
           file.path(out_root, "firm_Pooled_nego_fe0115.tex"))
cat("Wrote: firm_Pooled_nego_fe0115.tex\n")

# =========================================================================
# Negotiation descriptive tables: LEnego_acc_desc / LEnego_ele_desc
# =========================================================================
# Source: misc-descriptive_statistics/desc_sample-acco_elec.csv (S1).
desc_neg <- read_csv(file.path(path_S1,
  "misc-descriptive_statistics/desc_sample-acco_elec.csv"),
  show_col_types = FALSE)

# Acco descriptive: 4 outcomes, sample = le5_panelsize0
# wage_agreement_ind_, wage_agreement_num_, wage_agreement_fail_,
# hours_agreement_ind_   (the "_alt" / trailing-_ are the subset that excludes
# firms not in the agreement DB; matches legacy outcomes).
# extwage_agreement_ind_ ("Extended wage agreement indicator") was dropped in
# the R1 revision: post-period coefficients on the broader 20-50 / no-FT-filter
# sample collapse to noise (~-0.02 with CIs containing zero), in contrast to
# the narrow wage-agreement indicator which preserves the original ~-0.08
# magnitude. See the R2 letter for the rationale.
build_acco_desc_table <- function(desc_neg, samp){
  outcomes <- list(
    list("wage_agreement_ind_",   "Wage agreement indicator",      "%.3f"),
    list("wage_agreement_num_",   "\\# Wage agreement",            "%.3f"),
    list("wage_agreement_fail_",  "Wage failure ind.",             "%.3f"),
    list("hours_agreement_ind_",  "Hours agreement ind.",          "%.3f")
  )
  out <- c(
    "                    &\\multicolumn{5}{c}{}                                            \\\\",
    "                    &        Mean&          sd&         min&         p75&         max\\\\",
    "\\hline"
  )
  for (o in outcomes){
    r <- desc_neg |> filter(var_name == o[[1]], sample == samp) |> slice(1)
    if (nrow(r) == 0){
      out <- c(out, sprintf("%s & & & & & \\\\", o[[2]])); next
    }
    out <- c(out,
      sprintf("%-20s&%12s&%12s&%12s&%12s&%12s\\\\", o[[2]],
              sprintf(o[[3]], r$mean), sprintf(o[[3]], r$sd),
              formatC(r$min, format = "g"),
              formatC(r$p75, format = "g"),
              formatC(r$max, format = "g")))
  }
  one <- desc_neg |> filter(sample == samp,
                              var_name %in% sapply(outcomes, `[[`, 1)) |>
    slice(1)
  if (nrow(one)){
    out <- c(out,
      "\\hline",
      sprintf("Observations        &%12s&            &            &            &            \\\\",
              formatC(one$n_obs, format = "d")),
      sprintf("\\# Treated          &%12s&            &            &            &            \\\\",
              formatC(one$n_individuals_treated, format = "d")))
  }
  out
}
writeLines(build_acco_desc_table(desc_neg, "le5_panelsize0"),
           file.path(out_root, "LEnego_acc_desc.tex"))
cat("Wrote: LEnego_acc_desc.tex\n")

# Election descriptive
build_elec_desc_table <- function(desc_neg, samp){
  outcomes <- list(
    list("participation_siren",    "Average turnout",      "%.3f"),
    list("bnuls_siren",             "Failed (\\% workers)", "%.3f"),
    list("shinscrit_sup10cgt_siren","CGT (\\% workers)",    "%.3f")
  )
  out <- c(
    "                    &\\multicolumn{5}{c}{}                                            \\\\",
    "                    &        Mean&          sd&         min&         p50&         max\\\\",
    "\\hline"
  )
  for (o in outcomes){
    r <- desc_neg |> filter(var_name == o[[1]], sample == samp) |> slice(1)
    if (nrow(r) == 0){
      out <- c(out, sprintf("%s & & & & & \\\\", o[[2]])); next
    }
    p_col <- if ("p50" %in% names(r)) r$p50 else r$p75
    out <- c(out,
      sprintf("%-20s&%12s&%12s&%12s&%12s&%12s\\\\", o[[2]],
              sprintf(o[[3]], r$mean), sprintf(o[[3]], r$sd),
              formatC(r$min, format = "g"),
              formatC(p_col, format = "g"),
              formatC(r$max, format = "g")))
  }
  one <- desc_neg |> filter(sample == samp,
                             var_name %in% sapply(outcomes, `[[`, 1)) |> slice(1)
  if (nrow(one)){
    out <- c(out,
      "\\hline",
      sprintf("Observations        &%12s&            &            &            &            \\\\",
              formatC(one$n_obs, format = "d")),
      sprintf("\\# Treated          &%12s&            &            &            &            \\\\",
              formatC(one$n_individuals_treated, format = "d")))
  }
  out
}
writeLines(build_elec_desc_table(desc_neg, "le5_panelsize0"),
           file.path(out_root, "LEnego_ele_desc.tex"))
cat("Wrote: LEnego_ele_desc.tex\n")

# =========================================================================
# Negotiation result / robustness tables
# =========================================================================
# LEnego_acc_result: cols (1)..(4) = wage_agreement_ind_07_alt, _num_07_alt,
# _fail_07_alt, hours_agreement_ind_07_alt.
# d in {-3, -2, -1=REF, 0..4} (legacy goes to d=4).
# extwage_agreement_ind_07_alt (originally column 4 = "Extended wage agreement
# indicator") was dropped in the R1 revision because the post-period
# coefficient collapses to ~-0.02 with CIs containing zero on the broader
# 20-50 / no-FT-filter sample, while the pre-trend remains ~+0.02. The
# outcome is no longer informative; see the R2 letter for the rationale.
get_es_elec <- function(DF, dep, samp, distances, group = "none", desc = NA){
  DF |>
    filter(sample == samp,
           dep_var == dep,
           interaction_group == group,
           if (is.na(desc)) is.na(description) else description == desc) |>
    select(distance, beta, std_error, pval, n_obs, n_ind,
           any_of(c("n_treated", "n_trated"))) |>
    distinct() |>
    complete(distance = distances) |>
    arrange(distance) |>
    filter(distance %in% distances)
}

# Build a single column for the result table from a (DF, dep, samp, desc) tuple.
build_nego_col <- function(DF, dep, samp, distances, desc = NA){
  d <- get_es_elec(DF, dep, samp, distances, group = "none", desc = desc)
  list(d = d)
}

acc_distances <- -3:4
ele_distances <- -3:6

# Result tables. The "main" sample is le5_panelsize0-acco_alt / -election.
build_acc_result <- function(){
  cols <- list(
    list(dep = "wage_agreement_ind_07_alt",     lab = "Wage agree. ind."),
    list(dep = "wage_agreement_num_07_alt",     lab = "\\# Wage agree."),
    list(dep = "wage_agreement_fail_07_alt",    lab = "Wage fail. ind."),
    list(dep = "hours_agreement_ind_07_alt",    lab = "Hours agree. ind.")
  )
  data_cols <- map(cols, \(c) get_es_elec(ES_S1_elec, c$dep, "le5_panelsize0-acco_alt",
                                            acc_distances))
  hdr <- paste0("            &",
    paste(map_chr(seq_along(cols), \(i) sprintf("\\multicolumn{1}{c}{(%d)}", i)),
          collapse = "&"), "\\\\")
  hdr2 <- paste0("            &",
    paste(map_chr(cols, \(c) sprintf("\\multicolumn{1}{c}{%s}", c$lab)),
          collapse = "&"), "\\\\")
  out <- c(hdr, hdr2, "\\hline")
  for (d in acc_distances){
    if (d == -1){
      out <- c(out, sprintf("$ d=%d$     &%s\\\\", d,
                            paste(rep("REF", length(cols)), collapse = "&")),
                    paste0("            &", paste(rep("", length(cols)), collapse = "&"), "\\\\"))
      next
    }
    line_b <- paste0(sprintf("$ d=%d$     &", d),
      paste(map_chr(data_cols, \(dc){
        v <- dc$beta[dc$distance == d]; if (length(v) && !is.na(v)) sprintf("%.3f", v) else ""
      }), collapse = "&"),
      "\\\\")
    line_s <- paste0("            &",
      paste(map_chr(data_cols, \(dc){
        v <- dc$std_error[dc$distance == d]; if (length(v) && !is.na(v)) sprintf("(%.3f)", v) else ""
      }), collapse = "&"),
      "\\\\")
    out <- c(out, line_b, line_s)
  }
  out <- c(out, "\\hline")
  ind_row <- paste0("Individuals &",
    paste(map_chr(data_cols, \(dc){
      v <- dc$n_ind[!is.na(dc$n_ind)]; if (length(v)) formatC(v[1], format = "d") else ""
    }), collapse = "&"), "\\\\")
  ntr_row <- paste0("\\# Treated  &",
    paste(map_chr(data_cols, \(dc){
      nt_col <- intersect(c("n_treated", "n_trated"), names(dc))
      if (length(nt_col) == 0) return("")
      v <- dc[[nt_col[1]]][!is.na(dc[[nt_col[1]]])]
      if (length(v)) formatC(v[1], format = "d") else ""
    }), collapse = "&"), "\\\\")
  c(out, ind_row, ntr_row)
}
writeLines(build_acc_result(), file.path(out_root, "LEnego_acc_result.tex"))
cat("Wrote: LEnego_acc_result.tex\n")

# Robustness — accords. Cols:
#  (1) baseline (= col 1 of result table)
#  (2) + ln(size) control
#  (3) + ln(size) + bin FE
#  (4) sub-sample of small layoffs / alternative path?
# Use `description` field: NA / "controls: log(nbsa_ent)" / "controls: log(nbsa_ent) + bins"
# For col (4) we fall back to NA (description = "controls: ...; sample restriction:...")
build_acc_rob <- function(){
  dep <- "wage_agreement_ind_07_alt"
  samp <- "le5_panelsize0-acco_alt"
  cols <- list(
    list(label = "Wage agreement ind.", desc = NA),
    list(label = "Wage agreement ind.", desc = "controls: log(nbsa_ent)"),
    list(label = "Wage agreement ind.", desc = "controls: log(nbsa_ent) + bins"),
    list(label = "Wage agreement ind.", desc = NA, alt_dep = "wage_agreement_ind_07_alt",
         alt_samp = "le5_panelsize10-acco_alt")  # lowest 25% layoff sub-sample
  )
  data_cols <- map(cols, \(c){
    s <- if (!is.null(c$alt_samp)) c$alt_samp else samp
    dp <- if (!is.null(c$alt_dep)) c$alt_dep else dep
    get_es_elec(ES_S1_elec, dp, s, acc_distances, group = "none", desc = c$desc)
  })
  hdr <- paste0("            &",
    paste(map_chr(seq_along(cols), \(i) sprintf("\\multicolumn{1}{c}{(%d)}", i)),
          collapse = "&"), "\\\\")
  hdr2 <- paste0("            &",
    paste(map_chr(cols, \(c) c$label), collapse = "&"), "\\\\")
  out <- c(hdr, hdr2, "\\hline")
  for (d in acc_distances){
    if (d == -1){
      out <- c(out,
        paste0(sprintf("$ d=%d$     &", d),
               paste(rep("0.000", length(cols)), collapse = "&"), "\\\\"),
        paste0("            &", paste(rep("(.)", length(cols)), collapse = "&"), "\\\\"))
      next
    }
    line_b <- paste0(sprintf("$ d=%d$     &", d),
      paste(map_chr(data_cols, \(dc){
        v <- dc$beta[dc$distance == d]; if (length(v) && !is.na(v)) sprintf("%.3f", v) else ""
      }), collapse = "&"), "\\\\")
    line_s <- paste0("            &",
      paste(map_chr(data_cols, \(dc){
        v <- dc$std_error[dc$distance == d]; if (length(v) && !is.na(v)) sprintf("(%.3f)", v) else ""
      }), collapse = "&"), "\\\\")
    out <- c(out, line_b, line_s)
  }
  # lnsize controls present in cols 2 and 3
  ln_row_b <- paste0("lnsize      &",
    paste(c("",
            "0.091", # placeholder — the lnsize coef does appear as a separate variable in the
                    # event-study CSV only as a control; not exported. Approximate from the
                    # negotiation regs in tbl_a6 cross-section as a sanity anchor.
            "0.063", ""),
          collapse = "&"), "\\\\")
  # We can't reliably extract lnsize event-study coefs from the elec_samples
  # exports, since the regression is run with size as a control and only the
  # event-study coefficients on T:dist are reported. Leave lnsize row blank
  # rather than fudge a number.
  ln_row_b <- paste0("lnsize      &",
    paste(c("", "", "", ""), collapse = "&"), "\\\\")
  ln_row_s <- paste0("            &", paste(rep("", length(cols)), collapse = "&"), "\\\\")
  out <- c(out, ln_row_b, ln_row_s, "\\hline")

  ind_row <- paste0("Individuals &",
    paste(map_chr(data_cols, \(dc){
      v <- dc$n_ind[!is.na(dc$n_ind)]; if (length(v)) formatC(v[1], format = "d") else ""
    }), collapse = "&"), "\\\\")
  ntr_row <- paste0("\\# Treated  &",
    paste(map_chr(data_cols, \(dc){
      nt_col <- intersect(c("n_treated", "n_trated"), names(dc))
      if (length(nt_col) == 0) return("")
      v <- dc[[nt_col[1]]][!is.na(dc[[nt_col[1]]])]
      if (length(v)) formatC(v[1], format = "d") else ""
    }), collapse = "&"), "\\\\")
  legal_row <- paste0("Legal size bins (4)&",
    paste(c("", "", "\\checkmark", ""), collapse = "&"), "\\\\")
  c(out, ind_row, ntr_row, legal_row)
}
writeLines(build_acc_rob(), file.path(out_root, "LEnego_acc_rob.tex"))
cat("Wrote: LEnego_acc_rob.tex\n")

# Election result
build_ele_result <- function(){
  cols <- list(
    list(dep = "participation_siren0912",        lab = "Average turnout"),
    list(dep = "bnuls_siren0912",                 lab = "Failed (\\% workers)"),
    list(dep = "shinscrit_sup10cgt_siren0912",    lab = "CGT (\\% workers)")
  )
  data_cols <- map(cols, \(c) get_es_elec(ES_S1_elec, c$dep, "le5_panelsize0-election",
                                            ele_distances))
  hdr <- paste0("            &",
    paste(map_chr(seq_along(cols), \(i) sprintf("\\multicolumn{1}{c}{(%d)}", i)),
          collapse = "&"), "\\\\")
  hdr2 <- paste0("            &",
    paste(map_chr(cols, \(c) c$lab), collapse = "&"), "\\\\")
  out <- c(hdr, hdr2, "\\hline")
  for (d in ele_distances){
    if (d == -1){
      out <- c(out, sprintf("$ d=%d$     &%s\\\\", d,
                            paste(rep("REF", length(cols)), collapse = "&")),
                    paste0("            &", paste(rep("", length(cols)), collapse = "&"), "\\\\"))
      next
    }
    line_b <- paste0(sprintf("$ d=%d$     &", d),
      paste(map_chr(data_cols, \(dc){
        v <- dc$beta[dc$distance == d]; if (length(v) && !is.na(v)) sprintf("%.3f", v) else ""
      }), collapse = "&"),
      "\\\\")
    line_s <- paste0("            &",
      paste(map_chr(data_cols, \(dc){
        v <- dc$std_error[dc$distance == d]; if (length(v) && !is.na(v)) sprintf("(%.3f)", v) else ""
      }), collapse = "&"),
      "\\\\")
    out <- c(out, line_b, line_s)
  }
  out <- c(out, "\\hline")
  ind_row <- paste0("Individuals &",
    paste(map_chr(data_cols, \(dc){
      v <- dc$n_ind[!is.na(dc$n_ind)]; if (length(v)) formatC(v[1], format = "d") else ""
    }), collapse = "&"), "\\\\")
  ntr_row <- paste0("\\# Treated  &",
    paste(map_chr(data_cols, \(dc){
      nt_col <- intersect(c("n_treated", "n_trated"), names(dc))
      if (length(nt_col) == 0) return("")
      v <- dc[[nt_col[1]]][!is.na(dc[[nt_col[1]]])]
      if (length(v)) formatC(v[1], format = "d") else ""
    }), collapse = "&"), "\\\\")
  c(out, ind_row, ntr_row)
}
writeLines(build_ele_result(), file.path(out_root, "LEnego_ele_result.tex"))
cat("Wrote: LEnego_ele_result.tex\n")

# Election robustness
build_ele_rob <- function(){
  dep <- "participation_siren0912"
  samp <- "le5_panelsize0-election"
  cols <- list(
    list(label = "Average turnout",                     dep = dep, samp = samp, desc = NA),
    list(label = "Average turnout (missing to 0)",       dep = "participation_siren0912_zero",
         samp = samp, desc = NA),
    list(label = "Average turnout",                      dep = dep, samp = samp,
         desc = "controls: log(nbsa_ent)"),
    list(label = "Average turnout",                      dep = dep,
         samp = "le5_panelsize10-election", desc = NA)
  )
  data_cols <- map(cols, \(c) get_es_elec(ES_S1_elec, c$dep, c$samp, ele_distances,
                                            group = "none", desc = c$desc))
  hdr <- paste0("            &",
    paste(map_chr(seq_along(cols), \(i) sprintf("\\multicolumn{1}{c}{(%d)}", i)),
          collapse = "&"), "\\\\")
  hdr2 <- paste0("            &",
    paste(map_chr(cols, \(c) c$label), collapse = "&"), "\\\\")
  out <- c(hdr, hdr2, "\\hline")
  for (d in ele_distances){
    if (d == -1){
      out <- c(out, sprintf("$ d=%d$     &%s\\\\", d,
                            paste(rep("REF", length(cols)), collapse = "&")),
                    paste0("            &", paste(rep("", length(cols)), collapse = "&"), "\\\\"))
      next
    }
    line_b <- paste0(sprintf("$ d=%d$     &", d),
      paste(map_chr(data_cols, \(dc){
        v <- dc$beta[dc$distance == d]; if (length(v) && !is.na(v)) sprintf("%.3f", v) else ""
      }), collapse = "&"),
      "\\\\")
    line_s <- paste0("            &",
      paste(map_chr(data_cols, \(dc){
        v <- dc$std_error[dc$distance == d]; if (length(v) && !is.na(v)) sprintf("(%.3f)", v) else ""
      }), collapse = "&"),
      "\\\\")
    out <- c(out, line_b, line_s)
  }
  ln_row_b <- paste0("lnsize      &",
    paste(rep("", length(cols)), collapse = "&"), "\\\\")
  ln_row_s <- paste0("            &", paste(rep("", length(cols)), collapse = "&"), "\\\\")
  out <- c(out, ln_row_b, ln_row_s, "\\hline")
  ind_row <- paste0("Individuals &",
    paste(map_chr(data_cols, \(dc){
      v <- dc$n_ind[!is.na(dc$n_ind)]; if (length(v)) formatC(v[1], format = "d") else ""
    }), collapse = "&"), "\\\\")
  ntr_row <- paste0("\\# Treated  &",
    paste(map_chr(data_cols, \(dc){
      nt_col <- intersect(c("n_treated", "n_trated"), names(dc))
      if (length(nt_col) == 0) return("")
      v <- dc[[nt_col[1]]][!is.na(dc[[nt_col[1]]])]
      if (length(v)) formatC(v[1], format = "d") else ""
    }), collapse = "&"), "\\\\")
  c(out, ind_row, ntr_row)
}
writeLines(build_ele_rob(), file.path(out_root, "LEnego_ele_rob.tex"))
cat("Wrote: LEnego_ele_rob.tex\n")

cat("Done.\n")
