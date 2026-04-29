#UTILS/SETUP USED FOR PLOTTING

library(showtext)
font_add(family = "Times", regular = "Times-New-Roman.otf")
showtext_auto()
showtext_opts(dpi = 150)

theme_choclow <- function(){
  theme_minimal(16)+
    theme(
      axis.title.x = element_text(vjust = -1), 
      axis.title.y = element_text(vjust = 3), 
      legend.position = "bottom", 
      text= element_text(family = "Times"), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "grey20", linewidth = 0.6), 
    )
}

choclow_ggsave <- function(p, filename, h = 1200, w = 1600, output_dir){
  if(!dir.exists(output_dir)){dir.create(output_dir)}
  filename <- str_remove_all(tolower(str_replace_all(filename, " ", "_")), ",")
  ggsave(paste0(output_dir, "/", filename, ".png"), p, bg = "white", height = h, width = w, units = "px", dpi = 150)
}


plot_event <- function(DF, scale_map, group_var = "dep_var", ci_scalar = 2.576, event_cutoff = -0.5){
  p <- DF |> 
    mutate(ci_lower = beta - ci_scalar * std_error, ci_upper = beta + ci_scalar * std_error) |>
    ggplot(aes(x=distance, y=beta, ymin = ci_lower, ymax = ci_upper, color = .data[[group_var]], linetype = .data[[group_var]], shape = .data[[group_var]]))+
    geom_point(size = 3.5)+
    geom_line(linewidth = .8)+ 
    geom_vline(xintercept = event_cutoff, color = "grey30", linewidth = .6)+
    geom_hline(yintercept = 0, color = "grey30", linewidth = .6)+
    geom_errorbar(width = .25, linewidth = .75, linetype = "solid")+
    scale_x_continuous(breaks = min(DF$distance):max(DF$distance))+
    theme_choclow()+
    labs(
      x="", 
      y="", color = "", linetype = "", shape = ""
    )+
    scale_color_manual(values = scale_map$color)+
    scale_shape_manual(values = scale_map$shape)+
    scale_linetype_manual(values = scale_map$linetype)

  if(length(unique(DF$interaction_group))>1L){
    p+facet_wrap(~interaction_group)
  }else{
    p
  }
}

## color maps -----
main_map <- list(
  color = c(`Firm Wage Premium` = "#91353b", `Log Hourly Wage` = "#008bbc", `Log Hours` = "#82c0e9", `Log Earnings` = "#1e2d53"), 
  shape = c(`Firm Wage Premium` = 19, `Log Hourly Wage` = 17, `Log Hours` = 18, `Log Earnings` = 15), 
  linetype = c(`Firm Wage Premium` = "solid", `Log Hourly Wage` = "dashed", `Log Hours` = "dashed", `Log Earnings` = "solid")
) 

decomp_map <- list(
  color = c(`Overall effect` = "#1e2d53", `Within sector` = "#a0a0a0", `Between sector` = "#82c0e9"), 
  shape = c(`Overall effect` = 19, `Within sector` = 18, `Between sector` = 15), 
  linetype = rep("solid", 3)
) 

default_map <- list(color = "black", shape = 19, linetype = "solid")

pal_raw_means <- c(Displaced = "#1e2d53", Control = "#82c0e9")
pal_treated <- c(`1 - Treated` = "#1f78b4", `2 - Panel DADS` = "grey90", `0 - Control` = "#a6cee3")
pal_akm_fill <- c(Q1 = "#A8A8A8", Q2 = "#D0D0D0", Q3 = "#ABDAF4", Q4 = "#80CFB9", Q5 = "#E6BCD3")
pal_akm_color <- c(Q1 = "#5A5A5A", Q2 = "#A0A0A0", Q3 = "#56B4E9", Q4 = "#009E73", Q5 = "#CC79A7")

