#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Estimate the cost and health outcomes
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 

 get_output_schedule = function(vac_schedule, target_ages){
     
     # start with no vaccine
     # vac_schedule = run_config$schedule
     # target_ages = target_ages
   
     name_modseve = paste0(vac_schedule, "_modseve")
     name_nonseve = paste0(vac_schedule, "_nonseve")
     
     
     # get the list by age by severity ==> need the accumulative value by years
     L_modseve_cases_byAge = L_disease_burden_byAge[[name_modseve]]

     df_modseve_cases_byAge = bind_rows(L_modseve_cases_byAge,.id = "age_group") 
     df_modseve_cases_byAge$sim = as.numeric(df_modseve_cases_byAge$sim)
     
     c_select= which(!colnames(df_modseve_cases_byAge) %in% c("age_group","mid_age"))
     # need to aggregate under and over 5 years 
     df_modseve_cases_under5 = aggregate(.~sim, df_modseve_cases_byAge[df_modseve_cases_byAge$mid_age<5,c_select],sum)
     df_modseve_cases_above5 = aggregate(.~sim, df_modseve_cases_byAge[df_modseve_cases_byAge$mid_age>=5,c_select],sum)
     
     ### non severe cases
     L_nonseve_cases_byAge = L_disease_burden_byAge[[name_nonseve]]
  
     df_nonseve_cases_byAge = bind_rows(L_nonseve_cases_byAge,.id = "age_group")
     df_nonseve_cases_byAge$sim = as.numeric(df_nonseve_cases_byAge$sim)
    
     df_nonseve_cases_under5 = aggregate(.~sim, df_nonseve_cases_byAge[df_nonseve_cases_byAge$mid_age<5,c_select],sum)
     df_nonseve_cases_above5 = aggregate(.~sim, df_nonseve_cases_byAge[df_nonseve_cases_byAge$mid_age>=5,c_select],sum)
     
#### Cases ####
     c_sim = which(colnames(df_modseve_cases_under5) %in% "sim")
     df_tmp   = data.frame(matrix(NA,ncol = model_horizon,nrow=num_sim),
                           row.names=paste0("sim_", 1:num_sim))
     colnames(df_tmp) = colnames(df_modseve_cases_under5)[-c_sim]
    
     df_hosp_under5           = df_tmp
     df_hosp_above5           = df_tmp
     df_outp_modseve_under5   = df_tmp
     df_outp_modseve_above5   = df_tmp
     df_outp_nonseve_under5   = df_tmp
     df_outp_nonseve_above5   = df_tmp
     df_nonMA_modseve_under5  = df_tmp
     df_nonMA_modseve_above5  = df_tmp
     df_nonMA_nonseve_under5  = df_tmp
     df_nonMA_nonseve_above5  = df_tmp
     
     for (i_sim in 1:num_sim){
       
       x_modsev_under5 <- df_modseve_cases_under5[i_sim, -c_sim]
       x_modsev_above5 <- df_modseve_cases_above5[i_sim, -c_sim]
       x_nonsev_under5 <- df_nonseve_cases_under5[i_sim, -c_sim]
       x_nonsev_above5 <- df_nonseve_cases_above5[i_sim, -c_sim]
       
       
      df_hosp_under5 [i_sim,] =  x_modsev_under5*
                                  run_config$prob_mod_sev_MA_under5[i_sim]*
                                  run_config$prob_mod_sev_hosp_under5[i_sim]
      
      df_hosp_above5 [i_sim,] =  x_modsev_above5*
                                 run_config$prob_mod_sev_MA_above5[i_sim]*
                                 run_config$prob_mod_sev_hosp_above5[i_sim]
      # both modseve and nonseve cases
      df_outp_modseve_under5 [i_sim,] = x_modsev_under5*
                                        run_config$prob_mod_sev_MA_under5[i_sim]*
                                        run_config$prob_mod_sev_op_under5 [i_sim]
      
      df_outp_modseve_above5 [i_sim,] = x_modsev_above5[i_sim,-c_sim]*
                                        run_config$prob_mod_sev_MA_above5[i_sim]*
                                        run_config$prob_mod_sev_op_above5 [i_sim]
      
      df_outp_nonseve_under5 [i_sim,] =  x_nonsev_under5*
                                         run_config$prob_nonsev_op_under5[i_sim]
      
      df_outp_nonseve_above5 [i_sim,] = x_nonsev_above5*
                                        run_config$prob_nonsev_op_above5[i_sim]
      
      df_nonMA_modseve_under5[i_sim,] = x_modsev_under5*
                                        run_config$prob_mod_sev_nonMA_under5 [i_sim]
      
      df_nonMA_modseve_above5[i_sim,] =  x_modsev_above5*
                                         run_config$prob_mod_sev_nonMA_above5 [i_sim]
      
      df_nonMA_nonseve_under5[i_sim,] = x_nonsev_under5*
                                        run_config$prob_mod_nonsev_nonMA_under5 [i_sim]
      
      df_nonMA_nonseve_above5[i_sim,] = x_nonsev_above5*
                                        run_config$prob_mod_nonsev_nonMA_above5 [i_sim]
      
     }
     
     # set above 5 age group to 0
     if(length(target_ages) < length(age_group)){
       zero_df <- df_hosp_under5
       zero_df[,] <- 0
       df_hosp_above5  = zero_df
       df_outp_modseve_above5 = zero_df
       df_outp_nonseve_above5 = zero_df
       df_nonMA_modseve_above5 =zero_df
       df_nonMA_nonseve_above5 =zero_df
     }
     
    
    df_hosp           =   df_hosp_under5  +  df_hosp_above5 
    df_outp_modseve   =   df_outp_modseve_under5  +   df_outp_modseve_above5 
    df_outp_nonseve   =   df_outp_nonseve_under5 +  df_outp_nonseve_above5
    df_nonMA_modseve  =   df_nonMA_modseve_under5 + df_nonMA_modseve_above5
    df_nonMA_nonseve  =   df_nonMA_nonseve_under5 + df_nonMA_nonseve_above5 
    df_nonMA          =   df_nonMA_modseve + df_nonMA_nonseve
    df_outp           =   df_outp_modseve + df_outp_nonseve
    
  ### add df_death 
    df_death_by_age  = bind_rows(L_death_byAge)
    if (!all(colnames(df_hosp) %in% colnames(df_death_by_age))) {
      stop("Some df_hosp columns are missing from df_death_by_age")
    }
    
    df_death = aggregate(.~sim, data = df_death_by_age [,c("sim",colnames(df_hosp))],FUN=sum) 
    df_death $sim =as.numeric(    df_death $sim)
    df_death = df_death %>% 
               arrange(sim)   %>%
               select(-sim)
  
    
    #### intervention costs ####

  df_dose1 = L_dose[[paste0(vac_schedule,"_dose1")]]
  df_dose2 = L_dose[[paste0(vac_schedule,"_dose2")]]
  df_dose3 = L_dose[[paste0(vac_schedule,"_dose3")]]
  # check row and columns
  if (!all(
    dim(df_dose1) == dim(df_dose2),
    dim(df_dose1) == dim(df_dose3)
  )) {
    stop("df_dose1, df_dose2, and df_dose3 do not have the same dimensions")
  }
  
  
  df_dose = (df_dose1+ df_dose2 + df_dose3) # total number of doses
  
  vac_cost_swith = rep(0, num_sim)
  wastage = rep(0, num_sim)
  vac_cost_delivery = run_config$delivery_cost
  
  
  if (run_config$Product == "SuspVacc") {
    vac_cost_dose = rep(0, num_sim)
    vac_cost_delivery = rep(0, num_sim)
   } 
  
  if (run_config$Product == "Rotarix") {
    vac_cost_dose = run_config$cost_rotarix
    wastage = run_config$wastage_singlevial
  } 
  
  if (run_config$Product == "Rotavac") {
    vac_cost_dose <- run_config$cost_rotavac
    wastage <- run_config$wastage_5dosesvial
    }
  # condition for change cost of Rotavac
  if (run_config$Product == "Rotavac" && run_config$cost_rotavac_mean == 0.6) {
     wastage <- run_config$wastage_10dosesvial
  }
  if (run_config$Product == "Rotavac" && run_config$cost_rotavac_mean == 0.8) { # this is for the assumption of Rotasiil, 2-dose package
    wastage <- run_config$wastage_singlevial
  }
  # condition on switch cost
  if (run_config$Product == "Rotavac" && ISO3 == "MWI") {
    vac_cost_swith <- run_config$switching_cost_newVac
  }
  
  if (run_config$Product == "Rotavac" && ISO3 == "GHA") {
    vac_cost_swith = rep(0, num_sim)
  }   
  
  if (run_config$Product == "RV3BB") {
    vac_cost_dose = run_config$cost_RV3BB
    # over write the switch cost here
    vac_cost_swith = run_config$switching_cost_newVac
    wastage = run_config$wastage_5dosesvial # assume to be the same as rotavac
  } 
  
 # check length
  if (!all(lengths(list(vac_cost_dose, wastage, vac_cost_delivery)) %in% c(1, num_sim))) {
    stop("Vectors must have length 1 or num_sim")
  }
   vac_cost_inc_admin = vac_cost_dose  * (1+wastage) +  vac_cost_delivery
   
  

  ### calculate cost and outcomes ###
  # create empty data frame
  
  cost_hosp           = df_tmp
  cost_outp_modseve   = df_tmp
  cost_outp_nonseve   = df_tmp
  cost_nonMA          = df_tmp
  cost_indirect       = df_tmp
  cost_intervention   = df_tmp
  YLD_seve            = df_tmp
  YLD_mod             = df_tmp
  YLD_mild            = df_tmp
  
  # loop
  # check rows:
  if (!all(
    nrow(df_hosp) == num_sim,
    nrow(df_outp_modseve) == num_sim,
    nrow(df_outp_nonseve) == num_sim,
    nrow(df_nonMA) == num_sim,
    nrow(df_nonMA_modseve) == num_sim,
    nrow(df_nonMA_nonseve) == num_sim,
    nrow(df_dose) == num_sim
  )) {
    stop("One or more data frames do not have num_sim rows")
  }
  
  
  for (i_sim in 1:num_sim){
    cost_hosp [i_sim,]          = df_hosp[i_sim,]* run_config$cost_hosp_episode[i_sim] #+ run_config$cost_med_inp_episode[i_sim] 
    
    cost_outp_modseve[i_sim,]   = df_outp_modseve [i_sim,]* run_config$cost_outp_mod_sev [i_sim] #+ run_config$cost_med_outp)
    
    cost_outp_nonseve [i_sim,]  = df_outp_nonseve [i_sim,]* run_config$cost_outp_nonsev[i_sim] #+ run_config$cost_med_outp)
    
    cost_indirect [i_sim,]      = df_hosp[i_sim,]* run_config$indirect_cost_inp[i_sim] +  
                                  df_outp_modseve [i_sim,]*run_config$indirect_cost_outp_mod_sev[i_sim] + 
                                  df_outp_nonseve [i_sim,]*run_config$indirect_cost_outp_nonsev[i_sim] +
                                  df_nonMA[i_sim,] * run_config$cost_nonMA[i_sim]
    
    YLD_seve [i_sim,]           = df_hosp[i_sim,]* run_config$severe_YLD[i_sim]
    # note, nonMA modseve is considered as moderate 
    YLD_mod[i_sim,]            = (df_outp[i_sim,] + df_nonMA_modseve[i_sim,]) *run_config$moderate_YLD[i_sim]
    
    YLD_mild[i_sim,]           = df_nonMA_nonseve[i_sim,]*run_config$mild_YLD[i_sim]
    
    cost_intervention [i_sim,] = df_dose [i_sim,] * vac_cost_inc_admin[i_sim] 
  }
  
  
  # cost by category 
  cost_outp = cost_outp_modseve +  cost_outp_nonseve
 
  cost_directmedical = cost_hosp + cost_outp #+ cost_nonMA

  # add switching cost for the first year
  cost_intervention [,1] =  cost_intervention [,1] + vac_cost_swith  # the condition of cost of switch was set above
 
  cost_total_HCP = cost_directmedical + cost_intervention
  cost_total_societal =  cost_directmedical + cost_indirect + cost_intervention
  
  #### DALY ####
  YLD_total =   YLD_seve  + YLD_mod + YLD_mild
  # take YLL need it by year 2025:2034
  YLL_undisc = df_YLL_allAge %>% 
               filter(schedule == vac_schedule)  %>% 
               select(ends_with("_undisc"))
  
  colnames(YLL_undisc) =gsub("_undisc","", colnames(YLL_undisc))
  colnames(YLL_undisc) =gsub("X","", colnames(YLL_undisc))
  
  YLL_disc = df_YLL_allAge %>% 
             filter(schedule == vac_schedule)  %>% 
             select(ends_with("_disc"))
  
  colnames(YLL_disc) = colnames(YLL_undisc)
  
  # output of the functions
  L_output =list( doses = df_dose,
                  cases_death = df_death, 
                  cases_hosp = df_hosp,
                  cases_outp = df_outp,
                  cases_nonMA = df_nonMA,
                  cost_directmedical = cost_directmedical, 
                  cost_indirect = cost_indirect,
                  cost_intervention = cost_intervention,
                  cost_total_HCP = cost_total_HCP, 
                  cost_total_societal = cost_total_societal, 
                  YLD_seve = YLD_seve, 
                  YLD_mod = YLD_mod, 
                  YLD_mild = YLD_mild, 
                  YLD_total = YLD_total, 
                  YLL_disc = YLL_disc  ,
                  YLL_undisc = YLL_undisc)
  

return(L_output)
} # end of the cost function


# Sch_1to6to10_undis = get_output_schedule(vac_schedule = "Sch_1to6to10")

#### discounting ####

F_discounting = function( L_schedule_undisc,
                         vac_schedule,
                         disc_effect,
                         disc_cost ) {
  
  # vac_schedule=  run_config$schedule # which schedule
  # L_schedule_undisc =  get_output_schedule(vac_schedule = run_config$schedule, target_ages = target_ages) 
 
  # set-up discounting-over-time vector (in year) 
  years_after_vaccin     <- (0:(model_horizon-1))
  disc_time_outcome       <- 1/((1+disc_effect)^years_after_vaccin)
  disc_time_cost         <- 1/((1+disc_cost)^years_after_vaccin)
  
  # exclude last data frame (YLL already handled separately)
  idx_use <- seq_len(length(L_schedule_undisc) - 1)
  
  
  # set an empty list
  List_disc =  vector("list", length(idx_use))
  
  for(num_df  in idx_use ) { # min last data frame, as the YLL is undiscounted
    
   c_name = names(L_schedule_undisc) [[num_df]]
   df_undisc = L_schedule_undisc[[num_df]]
   
    
   if(grepl("cases",c_name)) {
     disc_rate =  1 #  we do not discount cases
   } else if(grepl("doses",c_name )) {
     disc_rate =  1
   } else if(grepl("cost",c_name )) {
      disc_rate =  disc_time_cost 
    } else if (grepl("YL",c_name )) {
      disc_rate = disc_time_outcome
    } # end of the if function 
   
  
   # undiscounted results
    df_undisc = L_schedule_undisc [[num_df]]
    cols_cases <- grepl("cases", colnames(df_undisc))
 
    # prepare discounted value for data frame
    df_disc = sweep(df_undisc, 2, disc_rate, `*`)
    
     # summary(df_disc[,"2025"] == df_undisc[,"2025"]) # pass
     # summary(df_disc[,"2026"] / df_undisc[,"2026"]) # pass
    
    List_disc [[num_df]] = df_disc
  } # end of for loop for num_df
  
  
  names(List_disc ) = names(L_schedule_undisc) [-length(names(L_schedule_undisc))]
  return(List_disc)
}

### tables and plotting ####    

## summary table: accumulative over ten years


