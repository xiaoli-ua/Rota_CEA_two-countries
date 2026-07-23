############################################################################ #
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => DEFAULT CONFIGURATION FOR THE ANALYSIS
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################ #


# # code to debug the function, is never executed by default
if(0==1) {
  # # for debugging starting from main.R, run these 2 lines, and then run everything in function
  i_scen <- 3 #intervention option considered
  configList <- sim_config_matrix[i_scen,]
}
#

get_rota_ce_config <- function(configList)
{ 
  # set.seed(rng_seed)
  config <- list()
  # set country
  config$country                   <- configList$country
  config$schedule                  <- configList$schedule
  config$scenario                  <- configList$scenario
  config$Product                   <- configList$Product
  config$nrb_dose                  <- configList$nrb_dose
  config$PriceSA                   <- configList$PriceSA
  config$ISO3                      <- configList$ISO3
  config$baseline                  <- configList$baseline
  # Add directory names for input and output 
  config$inputFileDir              <- "./input/"
  
  config$outputFileDir             <- configList$outputFileDir
  config$model_horizon             <- configList$model_horizon
  config$disc_cost                 <- configList$disc_rate_cost
  config$disc_effect               <- configList$disc_rate_effect
  config$perspective               <- configList$perspective

  prob_cost_input_file =   readxl::read_excel("./input/Input_prob_cost_data.xlsx")
  config$input_file =   prob_cost_input_file  [prob_cost_input_file $ISO3 == ISO3,]


# cost vaccine   
  # source: https://www.unicef.org/supply/media/20851/file/Rota-vaccines-prices-22022024.pdf

  config$cost_rotarix_mean = configList$Rotarix_price_mean
  config$cost_rotarix_low  = configList$Rotarix_price_LCI
  config$cost_rotarix_high = configList$Rotarix_price_UCI
  
  config$cost_RV3BB_mean   = configList$RV3BB_mean # $ 5 dose vial liquid
  config$cost_RV3BB_low    = configList$RV3BB_min # only used in the price threshold analysis
  config$cost_RV3BB_high   = configList$RV3BB_max # only used in the price threshold analysis
  
  config$cost_rotavac_mean   = configList$Rotavac_price_mean # $ 5 dose vial liquid
  config$cost_rotavac_low    = configList$Rotavac_price_LCI # only used in the price threshold analysis
  config$cost_rotavac_high   = configList$Rotavac_price_UCI # only used in the price threshold analysis
  
  config$implementation_cost_newVac_mean   = configList$implementation_cost_newVac_mean
  config$implementation_cost_newVac_min   = configList$implementation_cost_newVac_min
  config$implementation_cost_newVac_max   = configList$implementation_cost_newVac_max
  config$implementation_cost_newVac = rep ( config$implementation_cost_newVac_mean,num_sim) 
  
  config$delivery_cost_mean  = configList$delivery_cost_mean
  config$delivery_cost_min   = configList$delivery_cost_min
  config$delivery_cost_max   = configList$delivery_cost_max
  config$delivery_cost = runif(num_sim, min = config$delivery_cost_min , max =config$delivery_cost_max )
  
  threshold_price          = configList$PriceSA
  
  # fix cost of all these parameters for mean
  config$cost_rotarix = rep(config$cost_rotarix_mean,num_sim)
  mean(config$cost_rotarix )
  config$cost_rotavac = rep(config$cost_rotavac_mean,num_sim)
  mean(config$cost_rotavac)
  config$cost_RV3BB   = rep(config$cost_RV3BB_mean,num_sim)
  mean(config$cost_RV3BB)
  
  # set up for price threshold analysis
  
  if(config$scenario == 'OnewayPrice' |config$scenario == 'OnewayPrice_Societal'){
    # over write the cost
    config$cost_RV3BB   = runif(num_sim, min=config$cost_RV3BB_low ,max = config$cost_RV3BB_high  )
    } 
  if (config$scenario =="OnewaySwitchCost"| config$scenario =="OnewaySwitchCost_Societal") {
    # over write the cost
    config$implementation_cost_newVac = runif(num_sim, min = config$implementation_cost_newVac_min, config$implementation_cost_newVac_max)
  } 
  
  if (threshold_price =="twoWay_price") {
    # over write both costs
    config$cost_RV3BB   = runif(num_sim, min=config$cost_RV3BB_low ,max = config$cost_RV3BB_high  )}
  if (threshold_price =="twoWay_price" & grepl("switchCost", run_tag )) {
    config$implementation_cost_newVac = runif(num_sim, min = config$implementation_cost_newVac_min, config$implementation_cost_newVac_max)
  
    }
    
  # sanity check
  summary(config$cost_rotarix)
  summary(config$cost_rotavac)
  summary(config$cost_RV3BB )
   
 
  
  config$wastage_singlevial_mean = config$input_file$wastage_singlevial_mean
  config$wastage_singlevial = rbeta(num_sim, config$input_file$wastage_singlevial_a, config$input_file$wastage_singlevial_b)
  
  config$wastage_5dosesvial_mean = config$input_file$wastage_5dosesvial_mean
  config$wastage_5dosesvial = rbeta(num_sim, config$input_file$wastage_5dosesvial_a, config$input_file$wastage_5dosesvial_b)
  
  config$wastage_10dosesvial_mean = config$input_file$wastage_10dosesvial_mean
  config$wastage_10dosesvial = rbeta(num_sim, config$input_file$wastage_10dosesvial_a, config$input_file$wastage_10dosesvial_b)
  
  config$switching_cost_newVac = config$implementation_cost_newVac
  
  summary(config$switching_cost)

# probability of health care seeking ==> based on the decision tree shared by Ernest 
  config$prob_mod_sev_MA_mean_under5 = config$input_file$prob_mod_sev_MA_mean_under5

  
  config$prob_mod_sev_MA_alpha_under5 = config$input_file$prob_mod_sev_MA_alpha_under5 
  config$prob_mod_sev_MA_beta_under5 = config$input_file$prob_mod_sev_MA_beta_under5 
  
  config$prob_mod_sev_MA_under5 = rbeta(num_sim,shape1=config$prob_mod_sev_MA_alpha_under5,
                                                shape2=config$prob_mod_sev_MA_beta_under5)
  mean(config$prob_mod_sev_MA_under5)
  max(config$prob_mod_sev_MA_under5)
  summary(config$prob_mod_sev_MA_under5)
  # boxplot(config$prob_mod_sev_MA_under5)
  
  config$prob_mod_sev_nonMA_under5 = 1-config$prob_mod_sev_MA_under5                                          
  mean( config$prob_mod_sev_nonMA_under5)
  max(config$prob_mod_sev_nonMA_under5)
  summary(config$prob_mod_sev_nonMA_under5)
  # boxplot(config$prob_mod_sev_nonMA_under5)
  
  config$prob_mod_sev_hosp_mean_under5 = config$input_file$prob_mod_sev_hosp_mean_under5
  
  config$prob_mod_sev_hosp_lci_under5 = config$input_file$prob_mod_sev_hosp_lci_under5 
  config$prob_mod_sev_hosp_uci_under5 = config$input_file$prob_mod_sev_hosp_uci_under5
  config$prob_mod_sev_hosp_under5 = runif(num_sim,min=config$prob_mod_sev_hosp_lci,
                                                  max = config$prob_mod_sev_hosp_uci_under5)
  
  mean(config$prob_mod_sev_hosp_under5)
  max(config$prob_mod_sev_hosp_under5)
  summary(config$prob_mod_sev_hosp_under5)
  
  config$prob_mod_sev_op_under5 = 1-  config$prob_mod_sev_hosp_under5 

  config$prob_nonsev_op_mean_under5 = config$input_file$prob_nonsev_op_mean_under5
  config$prob_nonsev_op_alpha_under5 = config$input_file$prob_nonsev_op_alpha_under5 
  config$prob_nonsev_op_beta_under5 = config$input_file$prob_nonsev_op_beta_under5
  config$prob_nonsev_op_under5 = rbeta(num_sim,shape1=config$prob_nonsev_op_alpha_under5,
                                          shape2=config$prob_nonsev_op_beta_under5)
  mean(config$prob_nonsev_op_under5)
  max(config$prob_nonsev_op_under5)
  summary(config$prob_nonsev_op_under5)
  
  config$prob_mod_nonsev_nonMA_under5 = 1-  config$prob_nonsev_op_under5 

 # start to switch this part to 0, as we already inspect the above 5 years 
 # probability of health care seeking ==> In pop above 5, the data are not reliable, so we switch this part to 0
  config$prob_mod_sev_MA_mean_above5 = 0.00000000000000000001
  config$prob_mod_sev_MA_uci_above5  = 0.00000000000000000001# dummy data
  config$prob_mod_sev_MA_lci_above5  = 0.00000000000000000001 # dummy data
  config$prob_mod_sev_MA_above5 = F_sample_lognormal_dist (num_sim ,distr_mean =  config$prob_mod_sev_MA_mean_above5, 
                                                           distr_ul = config$prob_mod_sev_MA_uci_above5,
                                                           distr_ll =  config$prob_mod_sev_MA_lci_above5)
  # check
  mean(config$prob_mod_sev_MA_above5)
  max(config$prob_mod_sev_MA_above5)
  config$prob_mod_sev_nonMA_above5 = 1-config$prob_mod_sev_MA_above5                                          
  
  config$prob_mod_sev_hosp_mean_above5 = 0.00000000000000000001
  config$prob_mod_sev_hosp_uci_above5 =  0.00000000000000000001
  config$prob_mod_sev_hosp_lci_above5 =  0.00000000000000000001
  config$prob_mod_sev_hosp_above5 = F_sample_lognormal_dist (num_sim ,distr_mean =   config$prob_mod_sev_hosp_mean_above5, 
                                                             distr_ul = config$prob_mod_sev_hosp_uci_above5,distr_ll =config$prob_mod_sev_hosp_lci_above5)
  # check
  mean( config$prob_mod_sev_hosp_above5)
  config$prob_mod_sev_op_above5 = 1-config$prob_mod_sev_hosp_above5 
  
  config$prob_nonsev_op_mean_above5 =  0.00000000000000000001
  config$prob_nonsev_op_uci_above5  =  0.00000000000000000001
  config$prob_nonsev_op_lci_above5  =  0.00000000000000000001
  config$prob_nonsev_op_above5 = F_sample_lognormal_dist (num_sim ,distr_mean =  config$prob_nonsev_op_mean_above5, 
                                                              distr_ul = config$prob_nonsev_op_uci_above5,distr_ll =  config$prob_nonsev_op_lci_above5)
  mean(config$prob_nonsev_op_above5)
  
  config$prob_mod_nonsev_nonMA_above5 = 1-  config$prob_nonsev_op_above5 
  
  
   # death only occurs in the children under 5
  config$CFR_mean = config$input_file$CFR_mean
  config$CFR_alpha = config$input_file$CFR_alpha # dummy data
  config$CFR_beta = config$input_file$CFR_beta# dummy data
  config$CFR = rbeta(num_sim, config$CFR_alpha, config$CFR_beta )
  # check
  mean(config$CFR)
  max(config$CFR)
  summary(config$CFR)
  
  config$op_cfr_perc = runif(num_sim, min=0,max=1) # this is a probability of the percentage of outpatient deaths
  config$Prob_death_op = config$op_cfr_perc*config$CFR
  mean(config$Prob_death_op)
  max(config$Prob_death_op)
  
  config$CFR_nonMA_mean = config$input_file$CFR_nonMA_mean 
  config$CFR_nonMA_alpha = config$input_file$CFR_nonMA_alpha
  config$CFR_nonMA_beta = config$input_file$CFR_nonMA_beta # dummy data
  config$CFR_nonMA = rbeta(num_sim,  shape1 =config$CFR_nonMA_alpha, shape2 =config$CFR_nonMA_beta )
  # check
  mean(config$CFR_nonMA)
  max(config$CFR_nonMA)
  summary(config$CFR_nonMA)

  config$CFR_above5_factor =  0.000000000000000000000001

#### cost #####
  config$cost_outp_nonsev_mean  = config$input_file$cost_outp_nonsev_mean
  config$cost_outp_nonsev_shape = config$input_file$cost_outp_nonsev_shape
  config$cost_outp_nonsev_rate = config$input_file$cost_outp_nonsev_rate
  config$cost_outp_nonsev= rgamma(num_sim,shape= config$cost_outp_nonsev_shape,rate =config$cost_outp_nonsev_rate )
  mean(config$cost_outp_nonsev)
  max(config$cost_outp_nonsev)
  # boxplot(config$cost_outp_nonsev)
  
  config$cost_outp_mod_sev_mean = config$cost_outp_nonsev_mean # double the cost of non-sereve cases, due to small sample size(n=2) in the original cost publication
  
  config$cost_outp_mod_sev_shape = config$input_file$cost_outp_mod_sev_shape
  config$cost_outp_mod_sev_rate = config$input_file$cost_outp_mod_sev_rate
  
  config$cost_outp_mod_sev = rgamma(num_sim,shape=config$cost_outp_mod_sev_shape,rate = config$cost_outp_mod_sev_rate )
  mean(config$cost_outp_mod_sev)
  max(config$cost_outp_mod_sev)
  # boxplot(config$cost_outp_mod_sev)
  
  config$cost_hosp_mean = config$input_file$cost_hosp_mean
  
  config$cost_hosp_shape = config$input_file$cost_hosp_shape
  config$cost_hosp_rate = config$input_file$cost_hosp_rate
  config$cost_hosp_episode  = rgamma(num_sim,shape=config$cost_hosp_shape,rate =config$cost_hosp_rate)
  mean(config$cost_hosp_episode)
  max(config$cost_hosp_episode)
 

#### direct non-medical cost ####
  # this is switched off, as the indirect costs included this part based on literature
  config$Prop_inp = 0 
  config$Prop_outp = 0 

  config$direct_nonMed_cost_inp =   config$cost_hosp_episode*  config$Prop_inp 
  config$direct_nonMed_cost_outp =   config$cost_hosp_episode*  config$Prop_outp

#### indirect costs (defined as household costs)
   
  config$indirect_cost_inp_mean= config$input_file$cost_household_inpatient_mean
  config$indirect_cost_inp_shape = config$input_file$cost_household_inpatient_shape
  config$indirect_cost_inp_rate = config$input_file$cost_household_inpatient_rate
  config$indirect_cost_inp = rgamma(num_sim,  shape=config$indirect_cost_inp_shape , rate=config$indirect_cost_inp_rate)
  mean(config$indirect_cost_inp)
  max(config$indirect_cost_inp)
  
  
  config$indirect_cost_outp_mod_sev_mean = config$input_file$cost_household_outpatient_mod_sev_mean
  config$indirect_cost_outp_mod_sev_shape = config$input_file$cost_household_outpatient_mod_sev_shape
  config$indirect_cost_outp_mod_sev_rate = config$input_file$cost_household_outpatient_mod_sev_rate
  config$indirect_cost_outp_mod_sev = rgamma(num_sim,  shape=config$indirect_cost_outp_mod_sev_shape , rate=config$indirect_cost_outp_mod_sev_rate)
  mean(config$indirect_cost_outp_mod_sev)
  
  config$indirect_cost_outp_nonsev_mean = config$input_file$cost_household_outpatient_nonsev_mean
  config$indirect_cost_outp_nonsev_shape= config$input_file$cost_household_outpatient_nonsev_shape
  config$indirect_cost_outp_nonsev_rate = config$input_file$cost_household_outpatient_nonsev_rate
  config$indirect_cost_outp_nonsev = rgamma(num_sim,  shape = config$indirect_cost_outp_nonsev_shape , rate=config$indirect_cost_outp_nonsev_rate)
  mean(config$indirect_cost_outp_nonsev)
  
  # added cost of OTC 
  config$cost_nonMA_mean = config$input_file$cost_nonMA_mean
  config$cost_nonMA_lci = config$input_file$cost_nonMA_lci
  config$cost_nonMA_uci = config$input_file$cost_nonMA_uci
  config$cost_nonMA = runif(num_sim,  min= config$cost_nonMA_lci,  config$cost_nonMA_uci)
  mean(config$cost_nonMA)
  
  
#### DALY ####

# Salomon 2012 Lancet
# Diarrhoea: mild 0·061 (0·036–0·093)
# Diarrhoea: moderate 0·202 (0·133–0·299)
# Diarrhoea: severe 0·281 (0·184–0·399)

  
  # duration of illness
  config$days_illness_mean = config$input_file$days_illness_mean
  config$days_illness_lci= config$input_file$days_illness_lci
  config$days_illness_uci=config$input_file$days_illness_uci
  config$days_illness = F_sample_lognormal_dist (num_sim ,distr_mean =  config$days_illness_mean,
                                                 distr_ul =  config$days_illness_uci,distr_ll =   config$days_illness_lci)
  # check
  mean(config$days_illness)
  max(config$days_illness)
   
  # DALY adjusted by duration of illness
  config$mild_YLD_mean = 0.061
  config$mild_YLD_lci = 0.036
  config$mild_YLD_uci = 0.093
  config$mild_YLD_sd = get_se_based_on_ci(  config$mild_YLD_uci,  config$mild_YLD_lci)
  
  config$mild_YLD = sample_rgamma (num_sim, mean =  config$mild_YLD_mean ,stdev =  config$mild_YLD_sd ) *(config$days_illness/365)
  
  config$moderate_YLD_mean = 0.202
  config$moderate_YLD_lci = 0.133
  config$moderate_YLD_uci = 0.299
  config$moderate_YLD_sd = get_se_based_on_ci(  config$moderate_YLD_uci,  config$moderate_YLD_lci)
  #adjusted for duration of illness
  config$moderate_YLD =sample_rgamma (num_sim ,mean =  config$moderate_YLD_mean ,stdev =  config$moderate_YLD_sd ) *(config$days_illness/365)


  config$severe_YLD_mean = 0.281
  config$severe_YLD_lci = 0.184
  config$severe_YLD_uci = 0.399
  config$severe_YLD_sd = get_se_based_on_ci(  config$severe_YLD_uci,  config$severe_YLD_lci)
  config$severe_YLD =sample_rgamma (num_sim ,mean =  config$severe_YLD_mean ,stdev =  config$severe_YLD_sd ) * (config$days_illness/365)
  
 
  if(config$ISO3 == "MWI"){
   Sche_1to6to10_dop  = readxl::read_excel("./input/MWI_wv_5000_simulations.xlsx", sheet ="wv" )
   config$Sche_1to6to10_dop  =  Sche_1to6to10_dop$wv_RV3BB_dop_pooled[1:num_sim]
   }
  
  # added for scenario analysis
  if(config$ISO3 == "MWI" & config$schedule=="RV3BB_1to6to10_reducedVE"){
    Sche_1to6to10_dop  = readxl::read_excel("./input/opv_ghana_malawi_wv_sampling_results.xlsx", sheet ="sampling_wv" )
    config$Sche_1to6to10_dop  =  Sche_1to6to10_dop$wv_RV3BB_dop[1:num_sim]
  }
  
  if(config$ISO3 == "GHA"){
    ### Need to check the dop in Ghana
    Sche_1to6to10_dop  = readxl::read_excel("./input/GHA_wv_5000_simulations.xlsx", sheet ="wv" )
    config$Sche_1to6to10_dop = Sche_1to6to10_dop$wv_RV3BB_dop_pooled[1:num_sim]
    }
  

 # boxplot( config$Sche_1to6to10_dop,xaxt = "n", ylab= "RV3BB duration of protection (weeks)")
      

   return(config)
} # end of get config

# run_config = get_rota_ce_config(configList = sim_config_matrix[i_run,])



