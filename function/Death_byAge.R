#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Estimate the rotavirus-related deaths by age
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 

F_deaths_byAge = function(vac_schedule, byAge){

# start with no vaccine
# vac_schedule= run_config$schedule
# byAge = target_ages[2]
  select_age = gsub("Y","",byAge)
  
  ### only need moderate, as nonseve cases lead to no death
  name_modseve = paste0(vac_schedule, "_modseve")
  
  # get the list by age by severity 
  L_modseve_cases_byAge = L_disease_burden_byAge[[name_modseve]]
  
  df_modseve_byAge <- L_modseve_cases_byAge[[select_age]]
  
  df_modseve_byAge_death <- df_modseve_byAge
  df_modseve_byAge_death[, !names(df_modseve_byAge_death) %in% c("sim", "age_group", "mid_age")] <- NA_real_

# mod_severe death

  prob_death_inp_under5 = run_config$prob_mod_sev_MA_under5*
                          run_config$prob_mod_sev_hosp_under5 *
                          run_config$CFR
  # check
  str(run_config$prob_mod_sev_MA_under5)
  mean( prob_death_inp_under5)
  
  prob_death_outp_under5 = run_config$prob_mod_sev_MA_under5*
                           run_config$prob_mod_sev_op_under5*
                           run_config$Prob_death_op
  # check 
  str(prob_death_outp_under5)
  mean(prob_death_outp_under5)
  
  prob_death_nonmA_under5 = run_config$prob_mod_sev_nonMA_under5*run_config$CFR_nonMA 
  # check
  str(prob_death_nonmA_under5)
  mean(prob_death_nonmA_under5)
  
  prob_death_under5 = prob_death_inp_under5 + prob_death_outp_under5 + prob_death_nonmA_under5
  # check 
  str( prob_death_under5)
  mean( prob_death_under5)
  
  
  prob_death_inp_above5 = run_config$prob_mod_sev_MA_above5*
                          run_config$prob_mod_sev_hosp_above5 *
                          run_config$CFR*run_config$CFR_above5_factor
  # check
  mean(prob_death_inp_above5)
  
  prob_death_outp_above5 = run_config$prob_mod_sev_MA_above5*
                           run_config$prob_mod_sev_op_above5*
                           run_config$Prob_death_op*
                           run_config$CFR_above5_factor
  
  prob_death_nonmA_above5 = run_config$prob_mod_sev_nonMA_above5*
                            run_config$CFR_nonMA *
                            run_config$CFR_above5_factor
  
  prob_death_above5 =prob_death_inp_above5 + prob_death_outp_above5 + prob_death_nonmA_above5
  # check
  mean(prob_death_above5)
  
  c_col =which(!names(df_modseve_byAge) %in% c("sim","age_group","mid_age"))
  
  if (all(df_modseve_byAge$mid_age < 5)) {
    for (i_sim in 1:num_sim) {
      df_modseve_byAge_death[df_modseve_byAge_death$sim == i_sim, c_col] = df_modseve_byAge[df_modseve_byAge$sim == i_sim, c_col, drop = FALSE] *
                                                                              prob_death_under5[i_sim]
    }
  } else if (all(df_modseve_byAge$mid_age >= 5)) {
    for (i_sim in 1:num_sim) {
      df_modseve_byAge_death[df_modseve_byAge_death$sim == i_sim, c_col] = df_modseve_byAge[df_modseve_byAge$sim == i_sim, c_col,drop = FALSE] * 
                                                                            prob_death_above5[i_sim]
    }
  } else {
    stop("mid_age contains both <5 and >=5 values")
  }
  

return(df_modseve_byAge_death)
  
} # end of the function


# function below is used for check and plotting 

F_death_accum = function(vac_schedule, c_age, L_death){
  # vac_schedule= run_config$schedule
  # c_age = "under5" # or "above5"
  
  L_death = lapply(1: length(age_group),function(i) {F_deaths_byAge (vac_schedule = vac_schedule, byAge =  age_group[i])})
  names(L_death) = age_group
  if(c_age =="under5"){
    c_age_select = 0:4
  } else if (c_age =="all"){
    c_age_select = 0:99
  } else if (c_age == "above5"){
    c_age_select = 5:99
  }

  
  df_byAge = bind_rows(L_death) %>%
             filter(mid_age %in%c_age_select)%>%
             mutate(sum = rowSums(across(where(is.numeric)), na.rm = TRUE))
  
  c_select = which(!colnames(df_byAge) %in% c("age_group","mid_age"))
  
  # death in all ages by sim
  df_death_accum_age = aggregate(.~sim, data =df_byAge[,c_select], sum)
  
  
  
  df_death_accum = data.frame(schedule = vac_schedule,
                                       year    = c(modeled_years,"total"),
                                       mean    = colMeans(df_death_accum_age[,-1]), 
                                       lci     = apply(df_death_accum_age[,-1], 2, quantile, prob =0.025),
                                       uci     = apply(df_death_accum_age[,-1], 2, quantile, prob =0.975) 
                                       )

  
return(df_death_accum)

} # end of the function



get_deaths_byAge = function(){ 

    df_death_accum_allAge = bind_rows(lapply(1: (length(sim_config_matrix$scenario)/2),function(x) {F_death_accum (vac_schedule = sim_config_matrix$schedule[x], c_age ="all" )}))

    df_death_accum_under5 = bind_rows(lapply(1: (length(sim_config_matrix$scenario)/2),function(x) {F_death_accum (vac_schedule = sim_config_matrix$schedule[x], c_age ="under5" )}))

    df_death_accum_above5 = bind_rows(lapply(1: (length(sim_config_matrix$scenario)/2),function(x) {F_death_accum (vac_schedule = sim_config_matrix$schedule[x], c_age ="above5" )}))

    
    df_death_accum_under5_total = df_death_accum_under5 %>% 
                                  filter(year == "total")
    
    
    P_death_all_age = ggplot(data=df_death_accum_allAge %>%
                                  filter(year != "total"),
                             aes (x=year, y = mean, group = schedule, colour= schedule)) +
                        geom_line() +  ylim(c(0,NA)) + 
                        geom_ribbon(aes(ymin=lci, ymax=uci,fill = schedule),alpha=0.2,colour = NA) +
                        ggtitle("All ages")  


    P_death_under5 = ggplot(data=df_death_accum_under5 %>%
                              filter(year != "total"),
                            aes (x=year, y = mean, group = schedule, colour= schedule)) +
                        geom_line() +  ylim(c(0,NA)) + 
                        geom_ribbon(aes(ymin=lci, ymax=uci,fill = schedule),alpha=0.2,colour = NA) +
                        ggtitle("Under 5") 


    P_death_above5 = ggplot(data=df_death_accum_above5 %>%
                                 filter(year != "total"),
                            aes (x=year, y = mean, group = schedule, colour= schedule)) +
                        geom_line() +  ylim(c(0,NA)) + 
                        geom_ribbon(aes(ymin=lci, ymax=uci,fill = schedule),alpha=0.2,colour = NA) +
                        ggtitle("Above 5")  


    Plot_death = ggarrange(P_death_all_age + theme(legend.position="none"), 
                            P_death_under5 + theme(legend.position="none") ,
                            P_death_above5, nrow=1, widths = c(0.7,0.7,1))
    
    
    
    L_death_age = list (df_death_accum_under5_total = df_death_accum_under5_total,
                          df_death_accum_allAge  = df_death_accum_allAge, 
                          Plot_death = Plot_death )
    


return(L_death_age)
}


