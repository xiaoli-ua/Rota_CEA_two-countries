############################################################################ #
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => MAIN SCRIPT TO RUN THE DUAL-COUNTRY ANALYSES
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################ #


# clear environment 
rm(list=ls())

## set working directory (or open RStudio with this script)
# setwd("C:\User\path\to\the\rcode\folder") ## WINDOWS
# setwd("/Users/path/to/the/rcode/folder") ## MAC

# load package
source("./function/load_package.R")
source('./function/help_functions.R')

# model setup


# import excel data
source("./function/read_data_MWI.R")
source("./function/read_data_GHA.R")
# disease burden data
source("./function/diseaseBurden.R") 
# import cost-effectiveness analysis input data
source("./function/input data for CEA.R")
source("./function/Life_expectancy.R")
source("./function/Death_byAge.R")
source("./function/YLL_byAge.R")
source("./function/cost_outcome.R")
source("./function/summary_output.R")
source("./function/IncrementalAnalyais.R")
source("./function/CEAC.R")
source("./function/EVPPI.R")
source("./function/One_way_threshold.R")
source("./function/Two-way price threshold analysis.R")



####################### #
#### SETTINGS        ####
####################### #

# debug modus with default test-settings? To check if output remains the same.
bool_debug_modus <-FALSE  #FALSE: default config

# number of stochastic simulations (Max: 5000)
num_sim = 5000

model_horizon = 10
nrb_AgeGroup  = 20
modeled_years <- c(2025:2034)



## RUN TAG
   # run_tag <- 'Rota_MWI_basecase'                       # without price SA
   # run_tag <- 'Rota_MWI_oneWay'                         # one-way SA
   # run_tag <- 'Rota_MWI_twoWay_price_dop'               # two-way SA
   # run_tag <- 'Rota_MWI_twoWay_price_dop_Rotavacprice'  # two-way SA
   # run_tag <- 'Rota_MWI_twoWay_price_dop_Gaviprice'     # two-way SA
   # run_tag <- 'Rota_MWI_twoWay_price_deliveryCost'      # two-way SA
   # run_tag <- 'Rota_MWI_oneWay_Gaviprice'               # two-way SA @USD 0.2 per dose
   # run_tag <- 'Rota_MWI_twoWay_price_dop_Rotasiil'      # two-way SA # replace Rotavac by Rotasiil
   # run_tag <- 'Rota_MWI_RV3BB_reducedVE'                # scenario analysis for admin together with OPV, with reduced VE
   # run_tag <- 'Rota_MWI_oneWay_reducedVE'               # scenario analysis for admin together with OPV, with reduced VE
   # run_tag <- 'Rota_MWI_twoWay_price_dop_reducedVE'     # scenario analysis for admin together with OPV, with reduced VE
 
    run_tag <- 'Rota_GHA_basecase'                       # base case Ghana
   # run_tag <- 'Rota_GHA_oneWay'                         # one-way SA
   # run_tag <- 'Rota_GHA_twoWay_price_dop'               # two-way SA
   # run_tag <- 'Rota_GHA_twoWay_price_dop_Rotavacprice'  # two-way SA
   # run_tag <- 'Rota_GHA_twoWay_price_deliveryCost'      # two-way SA
   # run_tag  <- 'Rota_GHA_RV3BB_reducedVE'               # scenario analysis for admin together with OPV, with reduced VE
   # run_tag  <- 'Rota_GHA_oneWay_reducedVE'              # scenario analysis for admin together with OPV, with reduced VE
   # run_tag  <- 'Rota_GHA_twoWay_price_dop_reducedVE'    # scenario analysis for admin together with OPV, with reduced VE

   ### options

# save the tables
bool_table_save             <- TRUE   
# save the graphs 
bool_plot_save              <- TRUE



# random number generator seed
# note: does not necessarily gives the exact same result when used on another system (e.g. other computer)
rng_seed                  <-  20202025 

################################# #
## DEFAULT DEBUG SETTINGS ----
################################# #
if(bool_debug_modus){ 
  
  print('MODEL RUN IN DEBUG MODUS',WARNING = T) 
  
  rng_seed                    <- 20240212 
  num_sim                     <- 100
  run_tag                     <- 'Rota_MWI_basecase'
  output_dir                  <- 'output/debug'
  bool_plot_save              <- TRUE
  bool_table_save             <- TRUE
  
  unlink(output_dir,recursive = T)
}


# # Price threshold analysis? 
bool_plot_threshold_price        = grepl("_price",run_tag) 
bool_plot_threshold_price_OneWay = grepl("_oneWay",run_tag) 
bool_plot_threshold_price_TwoWay =  grepl("_twoWay",run_tag) 


# check country 
ISO3 = sub("^[^_]+_([A-Z]{3})_.*$", "\\1", run_tag)
# load the correct country data

if(ISO3 == "MWI"){
  L_dynamic_model_data= get_dynamic_model_data_MWI(nrow_read = num_sim)
}

if(ISO3 == "GHA"){
  L_dynamic_model_data= get_dynamic_model_data_GHA(nrow_read = num_sim)
}

L_dose  = L_dynamic_model_data$L_dose



####################### #
## MODEL SETUP ----
####################### #

print("****** START RotaCEA ******")
smd_print("WORK DIR:",getwd())


# output directory postfix
output_dir_postfix        <- paste0(run_tag,'_n',num_sim)

# if not in debug modus, add time stamp (month/day/hour/min/sec) to output directory name
if(!bool_debug_modus){
  output_dir        <- paste0('output/',format(Sys.time(),'%m%d%H%M%S_'),output_dir_postfix) 
}

# config file name
config_filename <- paste0('./config/',run_tag,'.xlsx')


# Time of the running
# Step 1: Record the start time
start_time <- Sys.time()
cat("Starting time: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
# print output folder
smd_print("OUTPUT DIR:",output_dir)


####################### #
## LOAD CONFIG ----
####################### #
# load config file in csv format
sim_config_matrix <- read.xlsx(config_filename,sheet='scenarios',na.strings = "-")

if (!(ISO3 %in% unique(sim_config_matrix$ISO3))) {
  stop("ISO3 does not match — stopping execution")
}


c_baseline <- switch(ISO3,
                     "MWI" = "Rotarix_6to10",
                     "GHA" = "Rotavac_6to10to14")
# factor level schedule
f_level_schedule<- switch(ISO3,
                     "MWI" = sim_config_matrix$schedule[1:5],
                     "GHA" = sim_config_matrix$schedule[1:3])


# willingness-to-pay values
num_wtp <-81 #default 81
wtp_max <-1000  #default 1000
if(ISO3 =="GHA"){
 wtp_max <-3500  
}

# set the GDP lines for the plot
gdp_lines <- data.frame(
  x = NA,
  label = c("0.25","0.5","0.75","1xGDP"
  )
)

if(ISO3 == "MWI"){
  gdp_lines$x <- 625.49 * c(0.25, 0.5, 0.75, 1) # GDPpc: 522.6 2024, updated to 2025 value
}

if(ISO3 == "GHA"){
  gdp_lines$x <- 3271.35 * c(0.25, 0.5, 0.75, 1) # GDPpc: 2390.8 in 2024, updated to 2025 value
}


opt_wtp_2way =  seq(0,wtp_max,length.out=5)



# output file name
sim_output_filename  <- file.path(output_dir)

# add simulation details
sim_config_matrix$num_sim                    <- num_sim
sim_config_matrix$scenario_id                <- 1:dim(sim_config_matrix)[1]
sim_config_matrix$rng_seed                   <- rng_seed  
sim_config_matrix$outputFileDir              <- smd_file_path(output_dir)

# Count number of scenarios (i.e. number of interventions compared, excluding comparator ('current practice'))
num_scen <- length(sim_config_matrix$scenario_id)
### the name of neonatal schedule
neonatal_schedule = unique(sim_config_matrix$schedule[sim_config_matrix$Product=="RV3BB"])

# set up colour palette, this color will be used for plotting in get disease burden and all other plot
jama_colors <- c(
  "#374E55FF", # dark blue-gray
  "#DF8F44FF", # warm orange
  "#00A1D5FF", # cyan-blue
  "#B24745FF", # brick red
  "#79AF97FF", # muted green
  "#6A6599FF", # purple
  "#80796BFF"  # brown/gray
  )

df_colour = data.frame(strategy = c("Rotarix_6to10","SuspVacc","Rotarix_6to10to14","Rotavac_6to10to14",neonatal_schedule),
                       colour = c(jama_colors[5],jama_colors[4],jama_colors[1],jama_colors[6],jama_colors[3]))



# get the disease burden
L_disease_burden_all = get_DiseasBurden()
L_disease_burden_byAge = L_disease_burden_all$L_output_AgeGroup

# set up selected age group ==> use under 5 years
age_group = names(L_disease_burden_byAge[[1]])
age_group_under5y = c("<1","1","2","3", "4" )
target_ages <- intersect(age_group_under5y, age_group)


# start parallel cluster
smd_start_cluster(timeout = 1000)

### Start to run the CEA model
i_run = 2
foreach(i_run = 1:num_scen,
        .packages = c(all_packages,'simid.rtools'),
        .combine = rbind) %dopar% {

# set rng seed (once)
set.seed(rng_seed)

run_config = get_rota_ce_config(configList = sim_config_matrix[i_run,])

# life expectancy by age
df_lifeExp_byAge = get_LE_YLL(disc_effect = run_config$disc_effect)


# death by age only focus on under 5 years
  # Keep only the age groups selected

 L_death_byAge <- lapply(target_ages, function(a) {
   F_deaths_byAge(vac_schedule = run_config$schedule, byAge = a)
 })
 
names(L_death_byAge ) =target_ages

# YLL by age

 L_YLL_byAge <- lapply(target_ages, function(target_ages_i) {
                F_YLL_byAge(vac_schedule = run_config$schedule, byAge = target_ages_i)
                })
 
 names(L_YLL_byAge) = target_ages


 df_YLL_allAge = data.frame(schedule = rep(run_config$schedule,each=num_sim),
                            Reduce(`+`, L_YLL_byAge ) )

 L_schedule_undisc =  get_output_schedule(vac_schedule = run_config$schedule,
                                         target_ages=target_ages) 

 L_schedule_disc =  F_discounting (L_schedule_undisc = L_schedule_undisc,
                                   vac_schedule= run_config$schedule, 
                                   disc_cost= run_config$disc_cost, 
                                   disc_effect = run_config$disc_effect )

output =   F_output (vac_schedule = run_config$schedule)

 } -> sim_output # end of the run_tag loop

sim_output$schedule =factor(sim_output$schedule, levels = f_level_schedule)

saveRDS(sim_output, file = paste0(sim_output_filename,"/sim_output_",num_sim,".rds") )

# set the name of neonatal schedule
select_sch_neonatal = sim_config_matrix$schedule[grep("RV3BB_1to6to10",sim_config_matrix$schedule)][1]

L_incremental_analysis = get_incremental_analysis()


# run the function in CEAC
if(!(bool_plot_threshold_price_TwoWay )){
    L_CEAC_EVPI = get_CEAC_EVPI (wtp_max =wtp_max,num_wtp =num_wtp)
    Plot_evppi = get_EVPPI()
 } 

if(bool_plot_threshold_price_OneWay ){
   Plot_one_way = get_threshold_oneWay ( num_wtp =81, wtp_max = wtp_max)
   } # end of one way
 
if(bool_plot_threshold_price_TwoWay ){
  if(grepl("_dop", run_tag)){
    Plot_two_way_price_Dop = get_price_threshold_twoWay ( wtp_max = wtp_max,
                                                        second_parameters = "dop",
                                                        opt_wtp_2way = opt_wtp_2way)
  } # end of 1st if
  
  # cost of switch

  if(grepl("_switchCost", run_tag)){

  Plot_two_way_price_costSwitch = get_price_threshold_twoWay ( wtp_max = wtp_max,
                                                              second_parameters = "cost_switch",
                                                                          opt_wtp_2way = opt_wtp_2way)
    } # end of 2nd if

 # delivery cost
  
  if(grepl("_deliveryCost", run_tag)){
    
    Plot_two_way_price_deliveryCost= get_price_threshold_twoWay ( wtp_max = wtp_max,
                                                                 second_parameters = "cost_delivery",
                                                                 opt_wtp_2way = opt_wtp_2way)
     } # end of 3rd if
  
  } # end of 2 way

# record the ending time
end_time <- Sys.time()
cat("Ending time: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")

# Calculate the duration
duration <- end_time - start_time
cat("Time taken: ", duration, "\n")


#### file

### save tables

if(bool_table_save  & (!bool_plot_threshold_price_OneWay)){
  # summary of dynamic model output
  write.csv(L_disease_burden_all$df_accum_summary,paste0(sim_output_filename,"/accum_case_DTM_under5y_",num_sim,".csv"),row.names = F)
  write.csv(L_incremental_analysis$df_case_output%>% 
            filter (scenario == "basecase"),
            paste0(sim_output_filename,"/accum_HealthcareCases_",num_sim,".csv"),row.names = F)
  
  write.csv(L_incremental_analysis$df_case_reduction_output%>% 
              filter (scenario == "basecase"),
            paste0(sim_output_filename,"/accum_HealthcareCases_Reduction_",num_sim,".csv"),row.names = F)
  
  # cost, DALY and incremental differences
  write.csv(L_incremental_analysis$T_cost_DALY,paste0(sim_output_filename,"/cost_DALY",num_sim,".csv"),row.names = F, fileEncoding = "UTF-8")
  write.csv(L_incremental_analysis$quadrant_data,file = paste0(sim_output_filename, '/Table_CE_PLANE.csv'), row.names = F, fileEncoding = "UTF-8")
}



### save plots

if(bool_plot_save == "TRUE") {
  
#### Plotting
  

# disease burden graph
ggsave(L_disease_burden_all$P_line_modseve, file= paste0(sim_output_filename,"/modseve_10years_byAge_",num_sim,".pdf"), width = 297, height = 210, units = "mm")
ggsave(L_disease_burden_all$P_line_nonseve, file= paste0(sim_output_filename,"/nonseve_10years_byAge_",num_sim,".pdf"), width = 297, height = 210, units = "mm")
ggsave(L_disease_burden_all$P_boxplot_accum, file= paste0(sim_output_filename,"/impact_accum_boxplot_", num_sim,".pdf"), width = 210, height = 210, units = "mm")


# YLL by age
P_boxplot_perSchedule = F_boxplot(df_long = sim_output)
ggsave(P_boxplot_perSchedule, file = paste0(sim_output_filename,"/YLL_bySchedule_",num_sim,".pdf"), width = 210, height = 297, units = "mm" )

# cost and outcome
 ggsave(L_incremental_analysis$Plot_cost_DALY, file = paste0(sim_output_filename,"/cost_DALY_bySchedule_",num_sim,".pdf"), width = 297, height = 180, units = "mm" )

# incremental analysis
 ggsave(L_incremental_analysis$CE_plot_schedules_HCP, file = paste0(sim_output_filename,"/PSA_HCP_",num_sim,".pdf"), width = 297, height = 210, units = "mm" )
 ggsave(L_incremental_analysis$CE_plot_schedules_societal, file = paste0(sim_output_filename,"/PSA_societal",num_sim,".pdf"), width = 297, height = 210, units = "mm" )
 ggsave(L_incremental_analysis$CE_plane_HCP, file = paste0(sim_output_filename,"/CEplane_HCP_",num_sim,".pdf"), width = 297, height = 210, units = "mm" )
 ggsave(L_incremental_analysis$CE_plane_societal, file = paste0(sim_output_filename,"/CEplane_societal_",num_sim,".pdf"), width = 297, height = 210, units = "mm" )
}
 
 # CEAC
 if(!bool_plot_threshold_price_TwoWay ){
 
 
 
 ggsave(L_CEAC_EVPI$basecase$Plot_CEAC_ENL, file = paste0(sim_output_filename,"/Plot_CEAC_ENL.pdf"), width = 297, height = 120, units = "mm" )
 ggsave(L_CEAC_EVPI$Societal$Plot_CEAC_ENL, file = paste0(sim_output_filename,"/Plot_CEAC_ENL_Societal.pdf"), width = 297, height = 120, units = "mm" )
 

 ggsave(Plot_evppi$basecase,
        file = paste0(sim_output_filename,"/Plot_EVPPI.pdf"), width = 260, height =200, units = "mm" )
  
 ggsave(Plot_evppi$Societal,   
        file = paste0(sim_output_filename,"/Plot_EVPPI_societal.pdf"), width = 260, height =200, units = "mm" )
 }
 
# One-way price threshold 

if (bool_plot_threshold_price_OneWay){
  
 Plot_one_way_price = Plot_one_way$OnewayPrice
 Plot_one_way_DoP = Plot_one_way$OnewayDoP
 Plot_one_way_SwitchCost = Plot_one_way$OnewaySwitchCost
 
 legend_shared <- get_legend(
   Plot_one_way_price + theme(legend.position = "top")
 ) # define the legend
 
 Plot_oneway =plot_grid( legend_shared,
                 plot_grid(Plot_one_way_price + theme(legend.position = "none"),  
                           Plot_one_way_DoP + theme(legend.position = "none"),  
                           Plot_one_way_SwitchCost + theme(legend.position = "none"),  
                           ncol = 3, 
                           align = "hv",
                           # labels = c("A", "B"),  
                           labels = c("A", "B", "C"),
                           label_size = 14),
                 ncol = 1,
                 rel_heights = c(0.12, 1)
               )

 
 Plot_one_way_price_societal = Plot_one_way$OnewayPrice_Societal
 Plot_one_way_DoP_societal = Plot_one_way$OnewayDoP_Societal
 Plot_one_way_SwitchCost_societal = Plot_one_way$OnewaySwitchCost_Societal
 
 Plot_oneway_societal = plot_grid( legend_shared,
                         plot_grid(Plot_one_way_price_societal + theme(legend.position = "none"),  
                                   Plot_one_way_DoP_societal + theme(legend.position = "none"), 
                                   Plot_one_way_SwitchCost_societal + theme(legend.position = "none"),
                                   ncol = 3, 
                                   align = "hv",
                                   labels = c("A", "B", "C"),  
                                   label_size = 14),
                         ncol = 1,
                         rel_heights = c(0.12, 1)
 )

ggsave(Plot_oneway,file = paste0(sim_output_filename,paste0("/Plot_",run_tag,num_sim,".pdf")), width = 297, height = 150, units = "mm" )
ggsave(Plot_oneway_societal,file = paste0(sim_output_filename,paste0("/Plot_",run_tag,num_sim,"_societal.pdf")), width = 297, height = 150, units = "mm" )

 
} # end of plotting one-way graphs


# Two-way price threshold 

if(bool_plot_threshold_price_TwoWay ){
source("./function/Two_way_threshold plot.R")
} # end of the two way



