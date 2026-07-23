#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Produce summary output
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 

  F_output = function(vac_schedule){
    # vac_schedule = run_config$schedule
    # L_schedule_undisc =  get_output_schedule(vac_schedule = run_config$schedule,target_ages =target_ages ) 
    # L_schedule_disc =  F_discounting (L_schedule_undisc = L_schedule_undisc,vac_schedule= run_config$schedule, disc_cost= run_config$disc_cost, disc_effect = run_config$disc_effect )
    
    df_summary_undisc = data.frame(sapply(1:length(L_schedule_undisc), function(x){
                                    rowSums(L_schedule_undisc[[x]])} ) )
    colnames(df_summary_undisc) = names(L_schedule_undisc)
    
    df_summary_undisc$DALY_undisc =(df_summary_undisc$YLD_total + df_summary_undisc$YLL_undisc)
    
    df_summary_undisc = cbind( data.frame(
                                num_sim  = seq_len(num_sim),
                                country  = run_config$country,
                                scenario = run_config$scenario,
                                baseline = run_config$baseline,
                                schedule = vac_schedule),
                                df_summary_undisc %>% select(-YLL_disc))
                              
    
    df_summary_disc = data.frame(sapply(1:length(L_schedule_disc), function(x){
                                 rowSums(L_schedule_disc[[x]])} ) )
    
    colnames(df_summary_disc) = names(L_schedule_disc)
    
    df_summary_undisc$DALY_disc =(df_summary_disc$YLD_total + df_summary_disc$YLL_disc)
    
    df_summary_disc = cbind ( data.frame(num_sim = 1:num_sim,
                                         country = run_config$country,
                                         scenario = run_config$scenario,
                                         baseline = run_config$baseline,
                                         schedule = vac_schedule),
                                 df_summary_disc)
    
    df_summary_output = full_join( df_summary_undisc, df_summary_disc, 
                           by= c("num_sim","country","scenario",'baseline',"schedule"),suffix=c("_undisc","_disc") )
    colnames( df_summary_output)


      ### add other parameters for EVPPI
    
    df_output = data.frame(df_summary_output, 
                           CFR           = run_config$CFR,
                           CFR_nonMA     = run_config$CFR_nonMA,
                           CFR_op_perc   = run_config$op_cfr_perc,
                           
                           prob_nonsev_op = run_config$prob_nonsev_op_under5,
                           prob_mod_sev_hosp = run_config$prob_mod_sev_hosp_under5,
                           prob_mod_sev_ma = run_config$prob_mod_sev_MA_under5,
                           
                          
                           cost_hosp     = run_config$cost_hosp_episode,
                           cost_op_mod_sev = run_config$cost_outp_mod_sev,
                           cost_op_nonmod = run_config$cost_outp_nonsev,
                           indirect_cost_op_mod_sev = run_config$indirect_cost_outp_mod_sev,
                           indirect_cost_op_nonsev = run_config$indirect_cost_outp_nonsev,
                           cost_nonMA = run_config$cost_nonMA,
                           indirect_cost_inp = run_config$indirect_cost_inp,
                           wastage_singleVial = run_config$wastage_singlevial,
                           wastage_5doseVial = run_config$wastage_5dosesvial,
                           wastage_10doseVial = run_config$wastage_10dosesvial,
                           
                           mild_YLD = run_config$mild_YLD,
                           moderate_YLD = run_config$moderate_YLD,
                           severe_YLD = run_config$severe_YLD,
          
                           Sche_1to6to10_dop = run_config$Sche_1to6to10_dop, 
                           
                           cost_rotarix = run_config$cost_rotarix,
                           cost_rotavac = run_config$cost_rotavac,
                           cost_RV3BB = run_config$cost_RV3BB,
                           cost_switch = run_config$switching_cost,
                           cost_delivery = run_config$delivery_cost
                          )
    
    return(df_output)
    
  }
  
  
  # summary tables, for sanity check
  
  F_output_summary_table = function(){
    
    colnames(sim_output)
    c_sim = which(colnames(sim_output) %in% c("num_sim", "scenario","baseline"))
    df_summary_mean = aggregate(.~ country+ schedule, sim_output[,-c_sim], mean)
    df_summary_lci = aggregate(.~ country+ schedule, sim_output[,-c_sim],quantile, prob=0.025)
    df_summary_uci = aggregate(.~ country+ schedule, sim_output[,-c_sim],quantile, prob=0.975)
    
    T_summary_ci = full_join( df_summary_lci, df_summary_uci,
                              by =c("country","schedule"), suffix = c("_lci","_uci"))
    T_summary = full_join( df_summary_mean,     T_summary_ci,
                           by =c("country","schedule"))
  
    
    return(T_summary)
  }
