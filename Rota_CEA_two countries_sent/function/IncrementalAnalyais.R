#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Conduct incremental analysis
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 

# estimate the incremental cost table compare to the baseline vaccination strategy: Malawi: Rotarix, Ghana: Rotavac
get_incremental_analysis = function(){
  
  sim_output_file = readRDS(paste0(sim_output_filename,"/sim_output_",num_sim,".rds") )
  
  
  n_schedule = unique(sim_output_file$schedule)
  n_scenario = unique(sim_output_file$scenario)
  n_schedule_scenario = unique(paste0(sim_output_file $schedule,"_",  sim_output_file$scenario))
    
  ### base case Rotarix should not be changed
  
  
  ### check cost data
  sim_output_cost = sim_output_file%>%
                    select(starts_with(c("num_sim", "country","scenario","schedule", "cost_")))
  
  sim_output_cost_mean = aggregate(.~schedule, sim_output_cost%>%
                                  select(starts_with(c("schedule", "cost_"))),mean)
  
  ### Setup data for baseline
  

  c_select <- c("scenario", "num_sim") 
  # 1) build a baseline table that keeps ALL simulations (one row per simulation)
  baseline <- sim_output_file %>%
                filter(baseline == "Baseline") %>%
                select(all_of(c_select),
                       b_cost_HCP = cost_total_HCP_disc,
                       b_cost_soc = cost_total_societal_disc,
                       b_DALY     = DALY_disc)
                  
  
  sim_output_incr <- sim_output_file %>%
                      left_join(baseline, by = c_select,relationship = "many-to-one") %>%
                      mutate(
                        incr_cost_HCP      = cost_total_HCP_disc       - b_cost_HCP,
                        incr_cost_societal = cost_total_societal_disc  - b_cost_soc,
                        incr_DALY          = -(DALY_disc - b_DALY)
                      )
   
  
select_col_names = c("country","scenario","schedule",
                     "cost_total_HCP_disc", "cost_total_societal_disc", "DALY_disc",
                     "incr_")

sim_output_disc = sim_output_incr%>%
                  select(starts_with(select_col_names )) 


df_cost_DALY_mean = aggregate(.~country + scenario + schedule, data =sim_output_disc,mean)
df_cost_DALY_lci = aggregate(.~country + scenario +schedule, data =sim_output_disc,quantile, prob=0.025)
df_cost_DALY_uci = aggregate(.~country + scenario + schedule, data =sim_output_disc,quantile, prob=0.975)

id_cols <- c("country", "scenario", "schedule")
num_cols <- setdiff(names(df_cost_DALY_mean), id_cols)

# check ICER RV3BB vs. Susp vaccine==> sanity check

if(!bool_plot_threshold_price_OneWay && !bool_plot_threshold_price_TwoWay){
  
    base <- subset(df_cost_DALY_mean, scenario %in% "basecase")
    
    ICER_RV3BBvsSuspVac <- with(base,
                                (cost_total_HCP_disc[schedule == "SuspVacc"] -
                                   cost_total_HCP_disc[schedule == select_sch_neonatal]) /
                                  (DALY_disc[schedule == select_sch_neonatal] -
                                     DALY_disc[schedule == "SuspVacc"]) )
    
    soc <- subset(df_cost_DALY_mean, scenario != "basecase")
    
    ICER_RV3BBvsSuspVac_societal <- with(soc,
                                    (cost_total_societal_disc[schedule == "SuspVacc"] -
                                       cost_total_societal_disc[schedule == select_sch_neonatal]) /
                                      (DALY_disc[schedule == select_sch_neonatal] -
                                         DALY_disc[schedule == "SuspVacc"])
    )

} # end of if, this is only for check the results of base case

T_cost_DALY_mean = df_cost_DALY_mean
T_cost_DALY_lci = df_cost_DALY_lci
T_cost_DALY_uci = df_cost_DALY_uci
cost_cols <- grep("cost", num_cols, value = TRUE)

T_cost_DALY_mean[cost_cols] <- df_cost_DALY_mean [cost_cols] / 1e6
T_cost_DALY_lci[cost_cols]  <- df_cost_DALY_lci[cost_cols]  / 1e6
T_cost_DALY_uci[cost_cols]  <- df_cost_DALY_uci[cost_cols]  / 1e6

daly_cols <- grep("DALY", num_cols, value = TRUE)
T_cost_DALY_mean[daly_cols] <- df_cost_DALY_mean [daly_cols] / 1000
T_cost_DALY_lci[daly_cols]  <- df_cost_DALY_lci[daly_cols]  / 1000
T_cost_DALY_uci[daly_cols]  <- df_cost_DALY_uci[daly_cols]  / 1000

T_cost_DALY =T_cost_DALY_mean

for (col in num_cols) {
  T_cost_DALY[[col]] <- gsub(
    "\u2013", "-", 
    sprintf(
      "%.0f (%.0f–%.0f)",
      T_cost_DALY_mean[[col]],
      T_cost_DALY_lci[[col]],
      T_cost_DALY_uci[[col]]
    )
  )
}

idx <- grep("cost", colnames(T_cost_DALY))
colnames(T_cost_DALY)[idx] <- paste0(colnames(T_cost_DALY)[idx], " (million)")
dx <- grep("DALY", colnames(T_cost_DALY))
colnames(T_cost_DALY)[dx] <- paste0(colnames(T_cost_DALY)[dx], " (thousand)")


df_incr = data.frame(country = df_cost_DALY_mean $country,
                     scenario =df_cost_DALY_mean $scenario,
                     schedule =df_cost_DALY_mean $schedule,
                     df_cost_DALY_mean[,grep("incr_",colnames(df_cost_DALY_mean ))])

if(!bool_plot_threshold_price_OneWay && !bool_plot_threshold_price_TwoWay){
  df_incr <- df_incr %>%
    mutate(
      ICER = ifelse(
        scenario == "basecase",
        incr_cost_HCP / incr_DALY,
        incr_cost_societal / incr_DALY
      )
    )
  
}


df_cases = sim_output_incr %>%
           select("country","scenario","schedule","doses_undisc", 
                 "cases_death_undisc","cases_hosp_undisc","cases_outp_undisc","cases_nonMA_undisc")

names(df_cases) = gsub("_undisc","",names(df_cases))
names(df_cases) = gsub("cases_","",names(df_cases))

df_cases_reduction = df_cases 
df_cases_reduction[, c("doses","death", "hosp", "outp", "nonMA")] <- NA_real_

df_cases_baseline_basecase = df_cases %>%
                            filter(schedule == c_baseline) %>%
                            filter(grepl("basecase", scenario))

df_cases_baseline_socital = df_cases %>%
                              filter(schedule == c_baseline) %>%
                              filter(grepl("Societal", scenario))

outcome_cols <- c("doses","death", "hosp", "outp", "nonMA")
for (v in outcome_cols) {
    # basecase
  idx <- grepl("basecase", df_cases$scenario)
    df_cases_reduction[idx, v] = (df_cases_baseline_basecase[[v]] - df_cases[idx, v]) /df_cases_baseline_basecase[[v]]
  
  # societal
    idx <- idx <- grepl("Societal", df_cases$scenario)
    df_cases_reduction[idx, v] =  (df_cases_baseline_socital[[v]] - df_cases[idx, v]) /df_cases_baseline_socital[[v]]
    }


df_case_mean = aggregate(.~country + scenario +schedule, df_cases,mean)
df_case_lci = aggregate(.~country + scenario +schedule, df_cases,quantile, prob=0.025)
df_case_uci = aggregate(.~country + scenario +schedule, df_cases,quantile, prob=0.975)
colnames(df_case_uci) [-(1:3)] = paste0(colnames(df_case_uci) [-(1:3)],"_uci")

df_case_summary = full_join(df_case_mean,df_case_lci, 
                            by=c("country", "scenario", "schedule"),
                            suffix = c("_mean", "_lci")) %>%
                            full_join(df_case_uci,
                                      by = c("country", "scenario", "schedule"))

df_case_summary$doses_k_mean =df_case_summary$doses_mean/1000
df_case_summary$doses_k_lci =df_case_summary$doses_lci/1000
df_case_summary$doses_k_uci =df_case_summary$doses_lci/1000

df_case_output = data.frame(df_case_summary [,c("country", "scenario", "schedule")])

# reduction
df_case_reduction_mean = aggregate(.~country + scenario +schedule, df_cases_reduction,mean)
df_case_reduction_lci = aggregate(.~country + scenario +schedule, df_cases_reduction,quantile, prob=0.025)
df_case_reduction_uci = aggregate(.~country + scenario +schedule, df_cases_reduction,quantile, prob=0.975)
colnames(df_case_reduction_uci) [-(1:3)] = paste0(colnames(df_case_reduction_uci) [-(1:3)],"_uci")

df_case_reduction_summary = full_join(df_case_reduction_mean,df_case_reduction_lci, 
                                      by=c("country", "scenario", "schedule"),
                                      suffix = c("_mean", "_lci")) %>%
                                      full_join(df_case_reduction_uci,
                                                  by = c("country", "scenario", "schedule"))

  

df_case_reduction_output = df_case_reduction_summary %>%
                           mutate(across( matches("_(mean|lci|uci)$"),~ paste0(round(.x * 100, 2), "%") ) 
                                  )




# Loop to simplified previous codes

fmt <- function(x) format(round(x, 0), big.mark = ",", scientific = FALSE)
vars <- c("doses","death", "hosp", "outp", "nonMA")

for (v in vars) {
  df_case_output[[paste0("summary_", v)]] <- paste0(
    fmt(df_case_summary[[paste0(v, "_mean")]]), " (",
    fmt(df_case_summary[[paste0(v, "_lci")]]), " ; ",
    fmt(df_case_summary[[paste0(v, "_uci")]]), ")"
  )
}

df_case_output <- df_case_output[order(df_case_output$scenario), ]

for (v in vars) {
  df_case_reduction_output[[paste0("summary_", v)]] <- paste0(
    df_case_reduction_output[[paste0(v, "_mean")]], " (",
    df_case_reduction_output[[paste0(v, "_lci")]], " ; ",
    df_case_reduction_output[[paste0(v, "_uci")]], ")"
  )
}

df_case_reduction_output <- df_case_reduction_output[order(df_case_reduction_output$scenario),]



### plotting

if(bool_plot_threshold_price_OneWay=="TRUE"|bool_plot_threshold_price_TwoWay =="TRUE"){
  L_incremental_analysis = list(sim_output_incr = sim_output_incr,
                                T_cost_DALY = T_cost_DALY, 
                                df_case_output= df_case_output%>% 
                                             select(starts_with(c("country","scenario", "schedule","summary_")))
                                )
} 


  df_cost_DALY = sim_output_incr %>% 
                              select (ends_with(c("country","scenario", "schedule",
                                           "cost_intervention_disc", 
                                          "cost_total_HCP_disc","cost_total_societal_disc",
                                          'YLD_total_disc',"YLL_disc","DALY_disc")))
  
  names(df_cost_DALY) = gsub("_disc", "",names(df_cost_DALY)) 
  df_cost_DALY$schedule = factor(df_cost_DALY$schedule, levels = f_level_schedule)
  
  df_cost_DALY$cost_directMedical=df_cost_DALY$cost_total_HCP-df_cost_DALY$cost_intervention
  df_cost_DALY$cost_societal=df_cost_DALY$cost_total_societal-df_cost_DALY$cost_intervention
  
  
  df_cost <- bind_rows(
                  df_cost_DALY %>%
                    select(scenario,schedule, cost = cost_directMedical) %>%
                    mutate(cost_pers = "Government"),
                  df_cost_DALY %>%
                    select(scenario,schedule, cost = cost_societal) %>%
                    mutate(cost_pers = "Societal")
                )
                
  my_theme <- theme(panel.background = element_rect(fill = "white"), # White background for the plot
                    legend.position = "none",
                    legend.text = element_text(size = 12),
                    axis.title.x = element_text(size = 12),
                    axis.title.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
                    axis.text.y  = element_text(size = 14),
                    panel.border = element_rect(color = "black", fill = NA), # Border line in black
                   )
 
    
  
  Cost_by_schedule = ggplot(df_cost,aes(x = schedule, y = cost, fill= schedule))+
                        geom_boxplot() + 
                        scale_fill_manual(
                          values = setNames(df_colour$colour, df_colour$strategy)
                        ) + 
                        facet_wrap(~cost_pers,scales="free_y") +
                        scale_y_continuous(
                          labels = label_dollar(scale = 1e-6, suffix = "M")  # show $ in millions
                        ) +
                    
                        labs(
                          x = NULL,                    # removes x-axis label
                          y = "Discounted cost (USD, millions)"   # keep y label clean
                        )  + 
                        theme(
                          strip.text = element_text(size = 14, face = "bold")
                        )+
                        my_theme
  
  DALY_by_schedule = ggplot(df_cost_DALY,aes(x = schedule, y =DALY, fill= schedule))+
                      geom_boxplot() + 
                      scale_fill_manual(
                        values = setNames(df_colour$colour, df_colour$strategy)
                      ) +
                      scale_y_continuous(
                        labels = label_number(scale = 1e-6, suffix = "M")  # show $ in millions
                      ) +
                      
                      labs(
                        x = NULL,                    # removes x-axis label
                        y = "Discounted DALY (millions)"   # keep y label clean
                      )  + 
                      my_theme
  
  Plot_cost_DALY = ggarrange(Cost_by_schedule,DALY_by_schedule, nrow = 1,widths = c(2,1))
  

  # CE planes were ploted only for base case 
  if(!bool_plot_threshold_price_OneWay && !bool_plot_threshold_price_TwoWay){
  
  CE_plane_HCP <- ggplot(df_incr %>% 
                           filter(scenario=="basecase"),
                         aes(x = incr_DALY, y = incr_cost_HCP, colour = schedule, label = schedule)) + 
                        geom_point() + 
                        geom_label_repel(aes(label = schedule),
                                         box.padding   = 0.35, 
                                         point.padding = 0.5,
                                         show.legend = FALSE) +
                        geom_hline(yintercept = 0) +
                        geom_vline(xintercept = 0) +
                        scale_color_manual(
                          values = setNames(df_colour$colour, df_colour$strategy),
                          labels = str_remove(df_colour$strategy, "^Sch_")  # remove "Sch_" from legend
                        ) +
                        xlab("DALY averted") + 
                        ylab("Incremental cost") +
                        theme(legend.position = "none") +
                        ggtitle("Goverment") + 
                        xlim(c(min(df_incr$incr_DALY), max(df_incr$incr_DALY))) +
                        # ylim(c(min(c(df_incr$incr_cost_HCP, df_incr$incr_cost_societal)),
                        #        max(c(df_incr$incr_cost_HCP, df_incr$incr_cost_societal)))) +
                        scale_y_continuous(
                          labels = label_dollar(scale = 1e-6, suffix = "M")  # show $ in millions
                        ) +
                        theme_minimal() +
                        theme(
                          legend.position = "top",
                          legend.title = element_blank(),
                          axis.text.x = element_text(size = 10),
                          axis.text.y = element_text(size = 10)
                        )
                      
  
  CE_plane_societal = ggplot(df_incr %>% 
                             filter(scenario == "Societal"),
                             aes(x = incr_DALY, y = incr_cost_HCP, colour = schedule, label = schedule)) + 
                      geom_point() + 
                      geom_label_repel(aes(label = schedule),
                                       box.padding   = 0.35, 
                                       point.padding = 0.5,
                                       show.legend = FALSE) +
                      geom_hline(yintercept = 0) +
                      geom_vline(xintercept = 0) +
                      scale_color_manual(
                        values = setNames(df_colour$colour, df_colour$strategy),
                        labels = str_remove(df_colour$strategy, "^Sch_")  # remove "Sch_" from legend
                      ) +
                      xlab("DALY averted") + 
                      ylab("Incremental cost") +
                      theme(legend.position = "none") +
                      ggtitle("Societal") + 
                      xlim(c(min(df_incr$incr_DALY), max(df_incr$incr_DALY))) +
                      # ylim(c(min(c(df_incr$incr_cost_HCP, df_incr$incr_cost_societal)),
                      #        max(c(df_incr$incr_cost_HCP, df_incr$incr_cost_societal)))) +
                      scale_y_continuous(
                        labels = label_dollar(scale = 1e-6, suffix = "M")  # show $ in millions
                      ) +
                      theme_minimal() +
                      theme(
                        legend.position = "top",
                        legend.title = element_blank(),
                        axis.text.x = element_text(size = 10),
                        axis.text.y = element_text(size = 10)
                      )
  

 
  ### plot cases
  
  ### PSA
  # data frame to add the line
  df_line_HCP <- df_incr %>%
              filter(scenario == "basecase",
              schedule %in% c("SuspVacc", select_sch_neonatal )
              )
  
  seg_df <- df_incr %>%
          filter(scenario == "Societal",
                 schedule %in% c("SuspVacc", select_sch_neonatal )) %>%
          select(schedule, incr_DALY, incr_cost_societal) %>%
          distinct() %>%   # IMPORTANT: ensure 1 row per schedule
          pivot_wider(
            names_from = schedule,
            values_from = c(incr_DALY, incr_cost_societal)
          ) %>%
          transmute(
            x    = incr_DALY_SuspVacc,
            y    = incr_cost_societal_SuspVacc,
            xend = .data[[paste0("incr_DALY_", select_sch_neonatal)]],
            yend = .data[[paste0("incr_cost_societal_", select_sch_neonatal)]]
          )
  
  CE_plot_schedules_HCP = ggplot(sim_output_disc %>%
                                    filter(scenario == "basecase"),
                                  aes(x = incr_DALY, y = incr_cost_HCP, colour = schedule)) + 
                          geom_point(alpha = 0.3, size = 2,show.legend = FALSE) + 
                          geom_point(data = df_incr %>% filter(scenario == "basecase"),
                                     aes(x = incr_DALY, y = incr_cost_HCP, fill = schedule),
                                     shape = 23, size = 4, colour = "black") +
                          geom_segment(
                            data = df_line_HCP %>% summarise(
                              x = incr_DALY[schedule == "SuspVacc"],
                              y = incr_cost_HCP[schedule == "SuspVacc"],
                              xend = incr_DALY[schedule == select_sch_neonatal ],
                              yend = incr_cost_HCP[schedule == select_sch_neonatal ]
                            ),
                            aes(x = x, y = y, xend = xend, yend = yend),
                            colour = "black",
                            linewidth = 0.8
                          ) +
                          geom_hline(yintercept=0) +
                          geom_vline(xintercept = 0)  + 
                          ylab("Incremneal cost") + xlab("DALY averted")+
                          xlim(c(min(sim_output_disc$incr_DALY), max(sim_output_disc$incr_DALY))) +
                          scale_y_continuous(
                                    labels = label_dollar(scale = 1e-6, suffix = "M")  # show $ in millions
                                  ) +
                          scale_color_manual(
                                    values = setNames(df_colour$colour, df_colour$strategy),
                                    labels = df_colour$strategy # remove "Sch_" from legend
                                  ) +
                          scale_fill_manual(values = setNames(df_colour$colour, df_colour$strategy)) +
                          theme_minimal() +
              
                          theme(    legend.position = "top",
                                    legend.title = element_blank(),
                                    axis.text.x = element_text(size = 10),
                                    axis.text.y = element_text(size = 10)
                                  )
  
  CE_plot_schedules_societal = ggplot(sim_output_disc %>%
                                         filter(scenario == "Societal"),
                                      aes(x = incr_DALY, y = incr_cost_societal, colour = schedule, label = schedule)) + 
                                geom_point(alpha = 0.6, size = 2, show.legend = FALSE) + 
                                geom_point(data = df_incr %>% filter(scenario == "Societal"),
                                           aes(x = incr_DALY, y = incr_cost_societal, fill = schedule),
                                           shape = 23, size = 4, colour = "black") +
                                geom_segment(
                                  data = seg_df,
                                  aes(x = x, y = y, xend = xend, yend = yend),
                                  inherit.aes = FALSE,
                                  colour = "black",
                                  linewidth = 0.8
                                ) +
                               geom_hline(yintercept=0) +
                               geom_vline(xintercept = 0)  + 
                               ylab("Incremneal cost") + xlab("DALY averted")+
                               xlim(c(min(sim_output_disc$incr_DALY), max(sim_output_disc$incr_DALY))) +
                               scale_y_continuous(
                                    labels = label_dollar(scale = 1e-6, suffix = "M")  # show $ in millions
                                  ) +
                               scale_color_manual(
                                    values = setNames(df_colour$colour, df_colour$strategy),
                                    labels = df_colour$strategy # remove "Sch_" from legend
                                  ) +
                                  scale_fill_manual(values = setNames(df_colour$colour, df_colour$strategy)) +
                                  theme_minimal() +
                                  
                                  theme(    legend.position = "top",
                                            legend.title = element_blank(),
                                            axis.text.x = element_text(size = 10),
                                            axis.text.y = element_text(size = 10)
                                  )
  
  # CE_plot_PSA = ggarrange(CE_plot_schedules_HCP+ ggtitle ("HCP") ,
  #                         CE_plot_schedules_societal +ggtitle ("Societal"),  common.legend = TRUE )
  
  
  # count number of times in each quadrant
  quadrant_data_HCP<- sim_output_disc %>%
                        filter(scenario == "basecase") %>%
                              mutate(
                                cost_sign  = ifelse(incr_cost_HCP >= 0, "North", "South"),
                                daly_sign = ifelse(incr_DALY >= 0, "east", "west"),
                                quadrant = paste(cost_sign,daly_sign, sep = "-")
                              ) %>%
                        group_by(schedule, quadrant) %>%
                        summarise(count = n(), .groups = "drop")  %>%
                        group_by(schedule) %>%
                        mutate(
                          percent = 100 * count / sum(count)
                        ) %>%
                      ungroup()  
  quadrant_data_HCP$perspective = "HCP"
  
  quadrant_data_Societal<- sim_output_disc %>%
                            filter(scenario == "Societal") %>%
                            mutate(
                              cost_sign  = ifelse(incr_cost_societal >= 0, "North", "South"),
                              daly_sign = ifelse(incr_DALY >= 0, "east", "west"),
                              quadrant = paste(cost_sign,daly_sign, sep = "-")
                            ) %>%
                            group_by(schedule, quadrant) %>%
                            summarise(count = n(), .groups = "drop")  %>%
                            group_by(schedule) %>%
                            mutate(
                              percent = 100 * count / sum(count)
                            ) %>%
                            ungroup()  
  quadrant_data_Societal$perspective = "Societal"
  quadrant_data = rbind(quadrant_data_HCP,quadrant_data_Societal)
  
 }
  
  
  # write the output
  L_incremental_analysis <- list( sim_output_incr = sim_output_incr,
                                  T_cost_DALY = T_cost_DALY,
                                  Plot_cost_DALY = Plot_cost_DALY,
                                  df_case_output = df_case_output %>%
                                                   select(starts_with(c("country", "scenario", "schedule", "summary_"))),
                                  df_case_reduction_output = df_case_reduction_output %>%
                                                              select(starts_with(c("country", "scenario", "schedule", "summary_")))
                                  
  )
  

  if (!bool_plot_threshold_price_OneWay && !bool_plot_threshold_price_TwoWay){
      L_incremental_analysis$CE_plane_HCP <- CE_plane_HCP
      L_incremental_analysis$CE_plane_societal = CE_plane_societal
      L_incremental_analysis$CE_plot_schedules_HCP <- CE_plot_schedules_HCP
      L_incremental_analysis$CE_plot_schedules_societal = CE_plot_schedules_societal
      L_incremental_analysis$quadrant_data = quadrant_data
  }


return(L_incremental_analysis)
}

