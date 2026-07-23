#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Conduct One-way threshold analysis: 
#### Price, duration of protection and switch cost,
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 

get_threshold_oneWay = function(wtp_max,num_wtp){

# # willingness-to-pay values

  opt_wtp <- seq(0,wtp_max,length.out=num_wtp)

  # start 
  if(unique(sim_config_matrix$PriceSA) !="No") {
      sim_output_incr = L_incremental_analysis$sim_output_incr
     } else if  (unique(sim_config_matrix$PriceSA) =="No") {
     print("STOP")
     }

# sanity check
summary(sim_output_incr$schedule)

# set names
name_schedule = unique(sim_output_incr$schedule)
name_scenario = unique(sim_output_incr$scenario)
scen_scenario_opt = name_scenario 

L_CEAC_EVPI_all =get_CEAC_EVPI (wtp_max =wtp_max,num_wtp =num_wtp)
# this is only for legend
net_benefit_legend = L_CEAC_EVPI_all[[1]]$net_benefit_legend

# check using correct dop data
c_dop = "Sche_1to6to10_dop"

### Set up an empty list
Plot_price_threshold_all=list()

i_scen = 2

for (i_scen in 1:length(scen_scenario_opt) ){

country_data =    sim_output_incr  [ sim_output_incr$scenario ==scen_scenario_opt [i_scen] , ]

# select the first scenario only (because uncertain input is repeated for each scenario)
country_param <- country_data

net_benefit_all = L_CEAC_EVPI_all[[scen_scenario_opt[i_scen]]]$net_benefit_all
# sanity check
names(net_benefit_all)
 
mean(sim_output_incr$cost_RV3BB)
mean(sim_output_incr$cost_RV3BB[1:num_sim]) # normally, the first one is one-way price threshold 

mean(sim_output_incr$cost_switch)

mean(tail(sim_output_incr$cost_switch[sim_output$schedule==select_sch_neonatal],num_sim)) # the last scenarios is about the switch cost


# set up the Y-axis name
scenario_name <- scen_scenario_opt[i_scen]

if (grepl("OnewayPrice", scenario_name)) {
  psa_inputs <- data.frame(oneway = country_data$cost_RV3BB[seq_len(num_sim)])
  label_y <- "RV3-BB price per dose (USD)"
  
} else if (grepl("OnewaySwitchCost", scenario_name)) {
  psa_inputs <- data.frame(oneway = tail(country_data$cost_switch, num_sim))
  label_y <- "Cost of switching vaccine"
  
} else if (grepl("OnewayDoP", scenario_name)) {
  psa_inputs <- data.frame(oneway = country_data[seq_len(num_sim), c_dop])
  label_y <- "RV3-BB protective duration (weeks)"
 } else {
  stop("No matching one-way scenario found")
  
}

# check whether a price sensitivity plot is possible

opt_oneway <- unique(psa_inputs$oneway)

if(length(opt_oneway)<30){
  message('Not enough simulations for threshold price sensitivity analysis ("num_sim<30")')
  return(NULL)
} 

# predict with regression model
resolution = 500 # nrb elements in the sequence factor
r  <- range(psa_inputs, na.rm = TRUE)
xs <- seq(r[1], r[2], length.out = resolution)
g  <- data.frame(oneway = xs)


### run a loop
z_wtp = data.frame(matrix(NA_real_, nrow = resolution,ncol = length(opt_wtp)))
colnames(z_wtp) = opt_wtp 

# i_wtp=17
for(i_wtp in 1:length(opt_wtp)){
  
  INBwithzero <- net_benefit_all[[i_wtp]]
  
  pta_legend <- colnames(INBwithzero)
  psa_nb = INBwithzero #decision options: sampled net benefit values for each intervention
  
  # get regression models
  reg_function  = function(x) gam(x ~ te(oneway), data = psa_inputs)
  c_baseline_col = which (colnames(psa_nb) == c_baseline)
  m = lapply(psa_nb[,-c_baseline_col, drop = FALSE], 
             reg_function) #no intervention column is dropped
  
  # predict with regression model
  # p     <- data.frame(baseline =0, 
  #                     sapply(m, function(x) predict(x, g)))
  
  p = data.frame( baseline = 0,
                  sapply(m, predict, newdata = g))
  
  colnames(p)[1]=   c_baseline
  
  p <- p[, colnames(psa_nb), drop = FALSE]
  

  class <- apply(p, 1, which.max) # the optimal strategy (highest INB) by each WTP value

  z_wtp[,i_wtp] = class 
  
}


df_plot = cbind(g,z_wtp) %>% 
          pivot_longer( cols = -colnames(g),
                        names_to = "wtp",
                        values_to = "strategy")

df_plot$wtp=as.numeric(as.character(df_plot$wtp))
df_plot = df_plot[order(df_plot$wtp),]

df_plot2 = df_plot %>%
            mutate(schedule = case_when(strategy == 1 ~ colnames(psa_nb)[1],
                                        strategy == 2 ~ colnames(psa_nb)[2],
                                        strategy == 3 ~ colnames(psa_nb)[3],
                                        strategy == 4 ~ colnames(psa_nb)[4],
                                        strategy == 5 ~ colnames(psa_nb)[5])
                                      
                   )

lim_y_min = min(psa_inputs$oneway)
lim_y_max = max(psa_inputs$oneway)


# Create dynamic color mapping

lim_y_min = floor(min(df_plot2$oneway))
lim_y_max = ceiling (max(df_plot2$oneway))

# set up the text position 
y_gdp = 5

if(grepl( "OnewayDoP", scen_scenario_opt[i_scen])){

  lim_y_min = round_down_next_25(min(sim_output$Sche_1to6to10_dop[1:num_sim],na.rm=T))
  lim_y_max = round_up_next_25(max(sim_output$Sche_1to6to10_dop[1:num_sim],na.rm=T))
  y_gdp =125 # set the max Y-axis value
}


if(grepl( "OnewaySwitchCost", scen_scenario_opt[i_scen])){
  lim_y_min = 0
  lim_y_max = 5000000
  y_gdp = lim_y_max *1.005
}


# shorten the schedule names
df_plot2$schedule = factor(df_plot2$schedule , levels = f_level_schedule )

label_level =  f_level_schedule


unique_schedules <- levels(df_plot2$schedule)

gdp_lines$y = y_gdp


level_colors_filtered = df_colour %>% 
                         filter(strategy %in% unique_schedules )
Plot_price_threshold = ggplot(df_plot2, aes(x = wtp, y = oneway, group = schedule, fill = schedule)) + 
                              geom_tile() + 
                              xlab("Willingness to pay per DALY averted") + 
                              ylab(label_y) + 
                              scale_fill_manual(values = setNames(level_colors_filtered$colour, level_colors_filtered$strategy))+
                              geom_vline(data = gdp_lines,
                                         aes(xintercept = x),
                                         linetype = "dotted",
                                        color = "grey50") +
                              geom_text( data = gdp_lines,
                                         aes(x = x, y = y, label = label),
                                         vjust = 1,size = 4,color = "grey40",inherit.aes = FALSE ) +
                              coord_cartesian(ylim = c(lim_y_min, lim_y_max)) +
                              theme_minimal(base_size = 16) +
                              theme(legend.position = "top",
                                    legend.text = element_text(size = 14),
                                    legend.title = element_blank(),
                                    axis.text = element_text(size = 12),
                                    axis.title = element_text(size = 12, face = "bold") )

if (grepl("OnewaySwitchCost", scen_scenario_opt[i_scen])) {
    Plot_price_threshold <- Plot_price_threshold +
    scale_y_continuous(
      labels = scales::label_dollar(scale = 1e-6, suffix = "M"),
      breaks = seq(0, 5e6, by = 1e6))
  }

Plot_price_threshold_all[[i_scen]] =Plot_price_threshold 

} # end of 1st loop
names(Plot_price_threshold_all)= scen_scenario_opt

return(Plot_price_threshold_all)
} # end of the function


