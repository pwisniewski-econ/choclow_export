# IMPORT RESULTS AND CREATE DATASET FOR EVENT STUDY PLOTTING

library(tidyverse)
library(arrow)

casd_export1 <- list.files(paste0("data/external/casd_export1/", c("event_studies-elec_samples", "event_studies-main_samples")), full.names = TRUE) 
CASD_DATA1 <- lapply(casd_export1, readr::read_csv, show_col_types = FALSE) |>
  dplyr::bind_rows() |>
  filter(str_detect(dep_var, "_ma3|_alt|_07")|str_detect(interaction_group, "_ma3")) |>
  group_by(sample, dep_var, interaction_group, description) |>
  group_modify(~ add_row(.x, .before=0, distance = -1, beta = 0)) |>
  ungroup() |>
  mutate(treatment_path = TRUE) |>
  rename(n_treated = n_trated)

casd_export2 <- lapply(paste0("data/external/casd_export2/", c("event_studies-elec_samples", "event_studies-main_samples")), 
  list.files, full.names = TRUE
) |> unlist()

CASD_DATA2 <- lapply(casd_export2, readr::read_csv, show_col_types = FALSE) |>
  dplyr::bind_rows() |>
  filter(!str_detect(dep_var, "_ma3|_alt|_07")&!str_detect(interaction_group, "_ma3"))

FULL_DATA <- bind_rows(CASD_DATA2, CASD_DATA1) 
write_parquet(FULL_DATA, "data/interim/merged_data.parquet")

FULL_DATA |> 
  filter(treatment_path==TRUE, is.na(description), !str_detect(interaction_group, "2005|2006")) |>
  mutate(dep_var = case_when(
    dep_var == "lnsbrhour" ~ "Log Hourly Wage",
    dep_var == "lnnbheur" ~ "Log Hours",
    dep_var == "fe0215_mlo_le" ~ "Firm Wage Premium", 
    dep_var == "lnsbr" ~ "Log Earnings", 
    dep_var == "wage_agreement_ind_07_alt" ~ "Indicator of Wage Agreement", 
    dep_var == "participation_siren0912" ~ "Election Turnout",
    dep_var == "sbr" ~ "Annual Earnings (levels)",
    dep_var == "employed" ~ "Employment Probability",
    dep_var == "nbheur" ~ "Hours Worked",
    .default = dep_var
  ), 
  dep_var = str_replace(dep_var, "prod_res_ma3", "Productivity"),
  dep_var = str_replace(dep_var, "ln_lshare_ma3", "Labor Share of Value-added"),
  dep_var = str_replace(dep_var, "fe0215_mlo_le", "Firm Wage Premium"),
  dep_var = str_replace(dep_var, "ln_apl_ma3", "Value-added per Worker"),
  effect_type = str_extract(dep_var, "_d2.*$"), 
  effect_type = if_else(is.na(effect_type), "_overall", effect_type), 
  varname = str_remove(dep_var, effect_type),
  effect_type = case_when(
    effect_type == "_overall" ~ "Overall effect", 
    effect_type == "_d2res" ~ "Within sector",
    effect_type == "_d2FE" ~ "Between sector"
  )) |> 
  write_parquet("data/interim/plotting_data.parquet")