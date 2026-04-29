#(RE)CREATE FOR ALL SAMPLES AND HET. VARIABLES THE MAIN FIGURES FROM THE PAPER

par_helper <- function(par_sample){

  ## Packages ----
  library(tidyverse)
  source("src/_utils.R")

  ## Load and rename data ----
  EVENT_STUDIES <- arrow::read_parquet("data/interim/plotting_data.parquet")

  main_pipeline <- function(het_var, sample_name){

    sample_outdir <- paste0("results/plots/main-", sample_name)
    if(het_var != "none"){
      sample_outdir <- paste0(sample_outdir, "/", het_var)
    }

    ## Figure 1 ----
    (EVENT_STUDIES |>
      filter(sample==sample_name, dep_var %in% c("Firm Wage Premium", "Log Hourly Wage", "Log Hours", "Log Earnings"), str_detect(interaction_group, het_var)) |>
      plot_event(main_map) +
      scale_y_continuous(breaks = seq(-1,2.2,.2))) |>
      choclow_ggsave("1-Wage Decomposition", output_dir = sample_outdir)

    ## Figure 2 ----
    if(sample_name!="panelpanSIZE3"){
      fig2_vars <- c("Productivity", "Labor Share of Value-added", "Firm Wage Premium", "Value-added per Worker")

      lapply(fig2_vars, \(x){
        EVENT_STUDIES |>
        filter(sample==sample_name, varname == x, str_detect(interaction_group, het_var)) |> 
        plot_event(decomp_map,  "effect_type")|>
        choclow_ggsave(paste0("2-", x), output_dir = sample_outdir)
      })
    }

    ## Figure A2 ----
    a2_vars <- c("Annual Earnings (levels)","Employment Probability","Hours Worked")

    lapply(a2_vars, \(x){
      (EVENT_STUDIES |>
      filter(sample==sample_name, dep_var == x, str_detect(interaction_group, het_var)) |>
      plot_event(default_map)+theme(legend.position = "none")) |>
      choclow_ggsave(paste0("a2-", x), output_dir = sample_outdir)
    })

    # FIGURE A9/A10 - RAW MEANS
    a9_a10_vars <- c("Annual Earnings (levels)", "Hours Worked")

    lapply(a9_a10_vars, \(x){
      p <- EVENT_STUDIES |>
      filter(sample==sample_name, dep_var == x, str_detect(interaction_group, het_var), !is.na(mean_treated)) |>
      rename("Displaced"="mean_treated", "Control" = "mean_control") |>
      pivot_longer(c("Displaced", "Control")) |>
      mutate(pre = if_else(distance < 0, TRUE, FALSE)) |>
      ggplot(aes(x=distance, y=value, color = name))+
      geom_point(size = 2.5)+
      geom_line(linetype = "solid", linewidth = .8)+ 
      geom_vline(xintercept = -.5, color = "grey30", linewidth = .6)+
      scale_x_continuous(breaks = -5:6)+
      theme_choclow()+labs(x= "Time to Displacement", y="", color="")+
      scale_color_manual(values = pal_raw_means)
      if(het_var!="none"){p <- p+facet_wrap(~interaction_group)}
      choclow_ggsave(p, paste0("a9_a10-", x), output_dir = sample_outdir)
    })

    return(0)
  }

  
  if(par_sample!="panelpanSIZE3"){
    hetvar_list <- c("none", "sx", "an_cl", "hhi")
  } else {
    hetvar_list <- c("none", "sx", "an_cl")
  }

  lapply(hetvar_list, main_pipeline, par_sample)

  return(0)

}

library(parallel)
cl <- makeCluster(6)

main_samples <- c("le5_panelsize0", "le5_panelsize10", "le5_panelsize20", "le5_panelsize40", "panelpanSIZE1", "panelpanSIZE2", "panelpanSIZE3")
parLapply(cl, main_samples, par_helper)

stopCluster(cl)