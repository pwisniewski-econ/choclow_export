# CREATE ALL OTHER "SECONDARY" PLOTS

library(tidyverse)
source("src/_utils.R")

external_path <- "data/external/"
interim_path <- "data/interim/"
outdir_elec <- "results/plots/elec-le5_panelsize0"
outdir_main <- "results/plots"

# ELECTION EVENT STUDIES ------

EVENT_STUDIES <- arrow::read_parquet(paste0(interim_path, "plotting_data.parquet"))

## Figure 3 ----
fig3_vars <- c("Indicator of Wage Agreement", "Election Turnout")

lapply(fig3_vars, \(x){
(EVENT_STUDIES |>
  filter(
    sample%in%c("le5_panelsize0-acco_alt", "le5_panelsize0-election"), dep_var == x, 
    interaction_group=="none", is.na(description), !(sample=="le5_panelsize0-acco_alt"&dep_var=="Election Turnout")) |>
  plot_event(default_map, "interaction_group")+
  theme(legend.position = "none"))|>
  choclow_ggsave(paste0("3-", x), output_dir = outdir_elec)
})

# FIGURE A12 - LOSS OF PREMIUM
fig_a12_samples <- c("le5_panelsize0-acco_alt", "le5_panelsize0-election")

lapply(fig_a12_samples, \(x){
  (EVENT_STUDIES |>
  filter(
    sample==x, 
    dep_var %in% c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings"), 
    interaction_group=="none", !(sample=="le5_panelsize0-acco_alt"&distance>4)) |> 
  plot_event(main_map, group_var = "dep_var")) |>
  choclow_ggsave(paste0("a12-", x), output_dir = outdir_elec)
})


# OCCUPATIONAL COMPARISONS ------
source("src/_all_labels.R")

OCCUP_COMPARISONS <- read_csv(paste0(external_path, "casd_export2/misc-descriptive_statistics/cs1_sec10_samples.csv")) |> 
  filter(!is.na(value)) |>
  group_by(treated = as.factor(treated), variable, sample) |> 
  mutate(share = n_individuals/sum(n_individuals), treated = case_when(
    treated == 0 ~ "0 - Control", 
    treated == 1 ~ "1 - Treated", 
    treated == 2 ~ "2 - Panel DADS"
  ), variable = case_when(
    variable == "cs1" ~ "Occupation", 
    variable == "dip_tot" ~ "Educational Attainment",
    variable == "sec_a10" ~ "Sector, 2-digit" 
  )) 

## Figure A1/A11 ----
fig_a1_a11_vars <- list(
  sample_le = list(
    c("Occupation", "Educational Attainment", "Sector, 2-digit"), 
    c("le5_panelsize0", "dads-2005_2012"),
    "a1"
  ), 
  sample_mlo = list(
    c("Occupation", "Sector, 2-digit"), 
    c("panelpanSIZE2", "dads-2005_2015"),
    "a11"
  )
)

for(sample_info in fig_a1_a11_vars){
  lapply(sample_info[[1]], \(x){
    (OCCUP_COMPARISONS |> 
    filter(variable == x, sample %in% sample_info[[2]]) |>
    mutate(
      name = factor(value, levels = all_labels[[x]]$value, labels = all_labels[[x]]$name, ordered = TRUE)
    ) |>
    ggplot(aes(x=name, y=share, fill = treated, group = treated))+
    geom_bar(stat="identity", position = position_dodge2(width = .75))+
    scale_fill_manual(values = pal_treated)+
    theme_choclow()+theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 45, vjust = .85, hjust = 1, size = 15))+
    geom_text(aes(label = paste0(100*round(share, 3)), y=share+.0075), position = position_dodge(width = .9))+
    labs(fill = "", y = "Share (%)", x = "")) |>
    choclow_ggsave(paste0(sample_info[[3]],"-", x), output_dir = outdir_main)
  }) 
}

# BINNED PLOTS -----

BINNED_DATA_FE <- read_csv(paste0(external_path, "casd_export1/fig_a7-binned_scatter/productivity_akm.csv")) |>
  mutate(xname = case_when(
    xname == "ln_apl_mean_0215" ~ "Value-added per Worker",
    xname == "prod_res_mean_0215" ~ "Productivity"
  ))

## Figure A7 ----
fig_a7_vars <- c("Value-added per Worker", "Productivity")

lapply(fig_a7_vars, \(x){
  (BINNED_DATA_FE |> 
    filter(xname == x) |>
    ggplot(aes(x=mean_xval, y = fe0215_mlo_le))+
    geom_point(size=4, color = '#800020')+
    theme_choclow()+labs(y="Firm Premium", x="")) |>
    choclow_ggsave(paste0("fig_a7-", x), output_dir = outdir_main)
})


# AKM PLOT -----

AKM_SUMMS <- read_csv(paste0(external_path, "casd_export2/misc-descriptive_statistics/quantiles_split.csv")) |>
  filter(str_starts(type_akm, "fe0215_mlo_le")) |>
  mutate(sample2 = case_when(
    str_detect(sample, "panelpanSIZE2") ~ "MLO30",
    str_detect(sample, "panelpanSIZE3") ~ "MLO90",
    str_detect(sample, "le5_panelsize0") ~ "LE0",
    str_detect(sample, "le5_panelsize10") ~ "LE10",
  )) |>
  filter(!is.na(sample2)) |>
  group_by(sample = sample2) |>
  mutate(quantile = factor(paste0("Q",quantile), levels = paste0("Q", 5:1)), 
    prop = n_ind/sum(n_ind), cumprop = cumsum(prop), 
    text_pos = cumprop - (cumprop-lag(cumprop))/2, 
    text_pos = if_else(is.na(text_pos), prop/2, text_pos))

(AKM_SUMMS  |>
  ggplot(aes(y=sample, x= prop, fill = quantile, color = quantile))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = pal_akm_fill)+
  scale_color_manual(values = pal_akm_color)+
  scale_x_continuous(breaks = seq(0,1,.1), labels = \(x){x*100})+
  geom_text(aes(label = paste0(round(prop,3)*100,"%"), x= text_pos), size=4, color = "black", show.legend = FALSE)+
  theme_choclow()+labs(y="", x= "", fill = "AKM Quantile", color = "AKM Quantile")+ 
  guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))) |>
  choclow_ggsave("fig_a8-AKM Quantiles", output_dir = outdir_main)
