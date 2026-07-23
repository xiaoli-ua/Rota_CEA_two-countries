#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => CALCULATE DISEASE BURDEN
#
#  Copyright 2025, CHERMID, UNIVERSITY OF ANTWERP
#############################################################################

get_DiseasBurden = function (){
  
  L_data_all = L_dynamic_model_data$L_data_all

  options(scipen = 999)
# first, get the data by age, it is very important to estimate the number of death by age

  DiseasBurden_byAge =function(i_df) {

  # i_df=1
  names(L_data_all)  # name of the schedule
  
  df_schedule= L_data_all[[i_df]]

  df_schedule_long = df_schedule %>%
                     pivot_longer(!sim,names_to = "time", values_to = "number") 
  
  # change age group names 
  # df_schedule_long$age_group <- gsub("_\\d{4}$", "", df_schedule_long$time)
  df_schedule_long$age_group =  str_sub(df_schedule_long$time,end=-6)
  df_schedule_long$age_group=  gsub("Y","",df_schedule_long$age_group)
  
  # add median age
  df_schedule_long$mid_age = NA
  df_schedule_long$mid_age [df_schedule_long$age_group=="<1"] =  0
  df_schedule_long$mid_age [df_schedule_long$age_group==">70"] =  72
  
  df_schedule_long$mid_age [is.na(df_schedule_long$mid_age)] = str_extract(df_schedule_long$age_group[is.na(df_schedule_long$mid_age)], "^[^_]+") 
  df_schedule_long$mid_age =as.numeric(as.character( df_schedule_long$mid_age))
  df_schedule_long$mid_age [df_schedule_long$mid_age>5 & df_schedule_long$mid_age<72] = df_schedule_long$mid_age [df_schedule_long$mid_age>5 & df_schedule_long$mid_age<72]+2
  
  # add year
  df_schedule_long$year = str_extract(df_schedule_long$time, "\\d{4}$")
  unique(df_schedule_long$year) %in% modeled_years
  
  df_schedule_wide = df_schedule_long %>% 
                     select(-time) %>% 
                      pivot_wider(names_from = year, values_from = number)
  # split into a list by age group
  L_AgeGroup= split(  df_schedule_wide, f =   df_schedule_wide$age_group)

  return(L_AgeGroup)
 }
 
L_output_AgeGroup = lapply(seq_along(L_data_all), function(i) DiseasBurden_byAge(i_df = i))
names(L_output_AgeGroup)  = names(L_data_all)

# sanity check
names(L_output_AgeGroup[[1]]) == names(L_output_AgeGroup[[3]])
age_group =names(L_output_AgeGroup[[1]])

# estimate the total number of cases 
out_cols <- c("schedule", "age_group", "mid_age", "year", "mean", "lci", "uci",
              "perc_reduction_mean", "perc_reduction_lci", "perc_reduction_uci")

n_rows_total <- sum(sapply(L_output_AgeGroup, function(x) {
  model_horizon * length(x)}))

df_output_bySchedule <- data.frame( matrix(NA_real_, ncol = length(out_cols), nrow = n_rows_total))

colnames(df_output_bySchedule) <- out_cols


  i_baseline_modseve <- which(names(L_output_AgeGroup) == paste0(c_baseline,"_modseve"))
  i_baseline_nonseve <- which(names(L_output_AgeGroup) == paste0(c_baseline,"_nonseve"))
  
  row_start_global <- 1
  # j_schedule=2 
for (j_schedule in 1:length(L_output_AgeGroup)){  
  # i_agegroup =4
  df_output <- data.frame(matrix( NA_real_,
                          ncol = length(out_cols),
                          nrow = model_horizon * length(L_output_AgeGroup[[j_schedule]])
                          )  )
    colnames(df_output) <- out_cols
    
    for (i_agegroup in 1: length(L_output_AgeGroup[[j_schedule]])) {
    
      tmp1 = L_output_AgeGroup[[j_schedule]][[i_agegroup]] # take the age group out
      c_select = which(!colnames(tmp1) %in% c("sim","age_group","mid_age") )
      # keep as data.frame with columns, even if length 1
      X <- tmp1[, c_select]
      
      # add aditional calculation to estimate the % changes
      # choose correct baseline depending on severity
      
      if (grepl("_modseve", names(L_output_AgeGroup)[j_schedule])) {
             i_baseline <- i_baseline_modseve
           } else if (grepl("_nonseve", names(L_output_AgeGroup)[j_schedule])) {
               i_baseline <- i_baseline_nonseve
          } else {
                stop("Cannot identify severity from schedule name")
      }
      
      tmp_base <- L_output_AgeGroup[[i_baseline]][[i_agegroup]]
      
      X_base <- tmp_base[, c_select, drop = FALSE]
      
      # calculate % reduction at simulation level
      
      X_perc_reduction <- (X_base - X) / X_base
      
      
      # align years with selected columns
      if (length(modeled_years) != ncol(X)) {
        stop(sprintf(
          "Length mismatch: modeled_years (%d) vs selected columns (%d) in schedule %s, age_group %s",
          length(modeled_years), ncol(X),
          names(L_output_AgeGroup)[j_schedule],
          as.character(unique(tmp1$age_group)[1])
        ))
      }
      
      ag <- unique(tmp1$age_group)
      ma <- unique(tmp1$mid_age)
      
      if (length(ag) != 1 || length(ma) != 1) {
        stop(sprintf(
          "Expected one unique age_group and one unique mid_age in schedule %s, age-group index %d",
          names(L_output_AgeGroup)[j_schedule], i_agegroup
        ))
      }
      
      

      tmp2 <- data.frame(schedule = names(L_output_AgeGroup)[j_schedule],age_group = ag,
                       mid_age = ma,
                       year = modeled_years,
                       mean = colMeans(X),
                       lci = apply(X, 2, quantile, prob = 0.025),
                       uci = apply(X, 2, quantile, prob = 0.975),
                       perc_reduction_mean = colMeans(X_perc_reduction),
                       perc_reduction_lci = apply(X_perc_reduction, 2, quantile, prob = 0.025),
                       perc_reduction_uci = apply(X_perc_reduction, 2, quantile, prob = 0.975)
                       )
      
      # write into the per-schedule block
      start_i <- (i_agegroup - 1) * model_horizon + 1
      end_i   <-  i_agegroup      * model_horizon
      df_output[start_i:end_i, ] <- tmp2
      
      rows_per_sched <- nrow(df_output)
      row_end_global <- row_start_global + rows_per_sched - 1
      df_output_bySchedule[row_start_global:row_end_global, ] <- df_output
      row_start_global <- row_end_global + 1
  } # end of first loop
  

  rows_per_sched <- nrow(df_output) 
  start_j <- (j_schedule - 1) * rows_per_sched + 1
  end_j   <-  j_schedule      * rows_per_sched
  df_output_bySchedule[start_j:end_j, ] <- df_output

  } # end of the second loop
   
  # df_output_bySchedule =   df_output_bySchedule[order(  df_output_bySchedule$mid_age),]
  df_output_bySchedule$severity =  substr(df_output_bySchedule$schedule, nchar(df_output_bySchedule$schedule) - 6, nchar(df_output_bySchedule$schedule))
  df_output_bySchedule$schedule <- df_output_bySchedule$schedule %>%
                                    str_remove("_(modseve|nonseve)$")

  df_modseve = df_output_bySchedule%>% 
               filter(severity =="modseve" )
    
  df_nonseve = df_output_bySchedule%>% 
               filter(severity =="nonseve" )
  

  #### plotting ####
  my_theme = theme_bw()+
             theme( legend.position = "top",
                    legend.title = element_blank(),
                    legend.text = element_text(size = 14))
  
  theme_set(my_theme)

  
  P_line_modseve = ggplot(data =  df_modseve , aes(x=year, y=mean, group = schedule,colour=schedule)) + 
                    geom_line(aes(color=schedule)) +
                    # geom_ribbon(aes(ymin=lci, ymax=uci,fill = schedule),alpha=0.3,colour = NA) +
                    facet_wrap(.~mid_age,scales = "free" )+
                    ggtitle('modseve') +
                    scale_x_continuous(breaks = seq(2025, 2035, by = 2)) + 
                    scale_y_continuous(labels = label_number(accuracy = 0.01))
    
  # age shift only occurs in modseve, age group 2 plus
  

  P_line_nonseve = ggplot(data =  df_nonseve , aes(x=year, y=mean, group = schedule,colour=schedule)) + 
                                  geom_line(aes(color=schedule)) +
                                  # geom_ribbon(aes(ymin=lci, ymax=uci,fill = schedule),alpha=0.3,colour = NA) +
                                  facet_wrap(.~mid_age,scales = "free" )+
                                  ylim(c(0,NA)) +   ggtitle('nonseve') +
                                  scale_x_continuous(breaks = seq(2025, 2035, by = 2)) 


  ### observed age_shift in >1 years of age group
  
  #### cumulative value #### 
  df_accum = data.frame(sim = 1:num_sim,
                        matrix(NA,nrow=num_sim, ncol= length(L_data_all)))
  colnames(df_accum) [-1]=names(L_data_all)
  
  df_accum_reduction <- df_accum
  
  # only take cases under 5 years 
  for(i_col in 1:length(L_data_all)){
    
    tmp_data_all = L_data_all[[i_col]]
    df_selected <- tmp_data_all %>%
                  select(starts_with("<1y_"),
                         starts_with("1y_"),
                         starts_with("2y_"),
                         starts_with("3y_"),
                         starts_with("4y_"))
                
    df_selected$sum =rowSums(df_selected)
    
    df_accum[,i_col+1] =  df_selected$sum
    
  }
  
# calculate $ reduction
  modseve_cols <- grep("_modseve$", names(df_accum), value = TRUE)
  nonseve_cols <- grep("_nonseve$", names(df_accum), value = TRUE)
  
  baseline_modseve <- paste0(c_baseline, "_modseve")
  baseline_nonseve <- paste0(c_baseline, "_nonseve")
  
  # moderate/severe
  
  for (v in setdiff(modseve_cols, baseline_modseve)) {
        df_accum_reduction[[v]] <-(df_accum[[baseline_modseve]] - df_accum[[v]]) /df_accum[[baseline_modseve]]
  }
  
  # non-severe
  
  for (v in setdiff(nonseve_cols, baseline_nonseve)) {
    df_accum_reduction[[v]] <- (df_accum[[baseline_nonseve]] - df_accum[[v]]) /df_accum[[baseline_nonseve]]
    }
 
  # set the baseline as 0 
  df_accum_reduction[[baseline_modseve]] <- 0
  df_accum_reduction[[baseline_nonseve]] <- 0
  

  df_accum_summary_under5 <-  full_join(F_summarise(df_accum), F_summarise(df_accum_reduction),by="lab",suffix = c("","_reduction"))
    
  
   # formating
  
  df_accum_summary_under5 =   df_accum_summary_under5%>% 
                                mutate(across(c(mean, lci, uci), ~ format(round(.x, 0), big.mark = ",", scientific = FALSE,trim = TRUE)))
  
  df_accum_summary_under5 <- df_accum_summary_under5 %>%
                              mutate(
                            across(ends_with("reduction"),
                                   ~ paste0(round(.x * 100, 2), "%"))
                                )
  
  df_accum_summary_under5$results_number = paste0(df_accum_summary_under5$mean," (",df_accum_summary_under5$lci," ; ",df_accum_summary_under5$uci,")")
  df_accum_summary_under5$results_perc = paste0(df_accum_summary_under5$mean_reduction," (",df_accum_summary_under5$lci_reduction," ; ",df_accum_summary_under5$uci_reduction,")")
 
  
  # for ploting
  
  df_accum_long = df_accum%>% 
                pivot_longer(!sim, names_to = "type", values_to = "sum")
  df_accum_long$severity = sub(".*_", "", df_accum_long$type) 
  df_accum_long$severity = sub("modseve", "moderate-to-severe",  df_accum_long$severity)
  df_accum_long$severity = sub("nonseve", "non-severe",  df_accum_long$severity) 
  df_accum_long$schedule = gsub("_modseve", "", df_accum_long$type)
  df_accum_long$schedule = gsub("_nonseve", "", df_accum_long$schedule )

#### plotting ####

  if(!grepl("_reducedVE",run_tag)){
    df_accum_long =  df_accum_long%>%
                    filter(!grepl("_reducedVE", type))
  }

  P_boxplot_accum = ggplot(df_accum_long, aes(x = schedule, y = sum, colour = schedule)) +
                            geom_boxplot(outlier.alpha = 0.4, width = 0.7) +
                            facet_wrap(~severity, scales = "free_y") +
                            scale_color_manual( values = setNames(df_colour$colour, df_colour$strategy)) +
                            scale_y_continuous(labels = scales::label_number(scale = 1/1000, big.mark = " ", suffix = ""),
                                               expand = expansion(mult = c(0, 0.05))  ) +
                            coord_cartesian(ylim = c(0, NA)) +  
                            labs(y = "Cases (×1 000)",
                                 x = "") +
                            theme_bw(base_size=16) +
                            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                                  legend.position = "none",
                                  strip.text = element_text(face = "bold")
                            )
                            
    

P_disease_burden = list (L_output_AgeGroup = L_output_AgeGroup, 
                         df_output_bySchedule = df_output_bySchedule , 
                         df_accum = df_accum,
                         P_line_modseve = P_line_modseve,  
                         P_line_nonseve = P_line_nonseve ,
                         P_boxplot_accum =P_boxplot_accum,
                         df_accum_summary_under5 =df_accum_summary_under5)
                   



return(  P_disease_burden)
} # end of get burden function

# P_disease_burden = get_DiseasBurden()

F_summarise<- function(df) {
  df <- df %>% select(-sim)
  data.frame(lab  = names(df),
             mean = colMeans(df),
             lci  = apply(df, 2, quantile, 0.025),
             uci  = apply(df, 2, quantile, 0.975) )
}
