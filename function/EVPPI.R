#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Conduct value of information analysis: Expected value of partial perfect information (EVPPI)
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 


get_EVPPI = function(){
  
  opt_wtp <- seq(0,wtp_max,length.out=num_wtp)

  sim_output_incr = L_incremental_analysis$sim_output_incr
  
  name_schedule = unique(sim_output_incr$schedule)
  name_scenario = unique(sim_output_incr$scenario)
  scen_scenario_opt = name_scenario 
  

  L_CEAC_EVPI_all =get_CEAC_EVPI (wtp_max =wtp_max,num_wtp =num_wtp)

  # creat an empty list before the loop
  Plot_evppi_all = list()
  
  # set up theme
  my_theme <- theme(
              panel.background = element_rect(fill = "white"),     # white background
              legend.position  = "right",                         # legend on the right
              legend.text      = element_text(size = 14),
              
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x  = element_text(size = 16),
              axis.text.y  = element_text(size = 16),
              
              panel.border      = element_rect(color = "black", fill = NA),
              panel.grid.major  = element_line(color = "lightgray", linewidth = 0.5),
              panel.grid.minor  = element_line(color = "lightgray", linewidth = 0.5)
            )
  
  
  i_scen = 1
  for (i_scen in 1:length(scen_scenario_opt) ){
      country_data =    sim_output_incr  [ sim_output_incr$scenario ==scen_scenario_opt [i_scen] , ]
      net_benefit_all =  L_CEAC_EVPI_all [[i_scen]]$net_benefit_all

      names(sim_output_incr)

      c_dop = "Sche_1to6to10_dop"
  
      c_input_names = c("CFR","CFR_nonMA","CFR_op_perc", "prob_nonsev_op", 
                        "prob_mod_sev_hosp","prob_mod_sev_ma","cost_delivery",                
                        "cost_hosp","cost_op_mod_sev", "cost_op_nonmod",  
                        "wastage_singleVial",  "wastage_5doseVial","wastage_10doseVial",
                        "mild_YLD", "moderate_YLD","severe_YLD",
                        "cost_RV3BB", 'cost_nonMA')
                        # 'cost_rotarix','cost_rotavac',c_dop )

      evppi_colors = c( "#332288", "#88CCEE", "#44AA99", "#117733",
                        "#999933", "#DDCC77", "#CC6677", "#882255",
                        "#AA4499", "#661100", "#6699CC", "#AA4466",
                        "#4477AA", "#228833", "#CCBB44", "#EE6677",
                        "#AA3377", "#661100", "#6699CC", 
                        "#44BB99", "#9966CC" )

    # adding input parameters         
        if(scen_scenario_opt [i_scen] == "Societal"){
          c_input_names = c(c_input_names, "indirect_cost_inp", "indirect_cost_op_mod_sev",'indirect_cost_op_nonsev')
        }
        
        if(n_distinct(sim_output$cost_RV3BB) == 1){
          c_input_names =c_input_names[c_input_names != "cost_RV3BB"]
        }
        
        country_data_input = sim_output[1:num_sim,c_input_names]
        
        num_param = ncol(country_data_input)

# check evpi


evpi_gam <- rep(NA_real_,num_wtp)

param_matrix <- as.matrix(country_data_input)

if (nrow(param_matrix) != num_sim) {
  stop("Mismatch: country_data_input rows != num_sim")
  } # end of the if for check

  # j <- 3
  for(j in 1:num_wtp){
      NB_tmp = net_benefit_all[[j]]
      
      if (nrow(NB_tmp) != num_sim) {
        stop(sprintf("Mismatch in NB_tmp at j = %d", j))
        }
      evpi_gam[j] <- evppi_gam( NB=NB_tmp,
                                model_parameter_values= param_matrix)
  
  } # end of the loop for j

# EVPPI

evppi <- data.frame (matrix(NA_real_,num_wtp,(num_param)),
                     row.names = opt_wtp)

colnames(evppi) = colnames(country_data_input)

# Double loop to get EVPI using gam approach 
# j <- 1 

for (j in 1:num_wtp){
  NB_tmp = net_benefit_all[[j]]
  # add a check
  if (nrow(NB_tmp) != nrow(country_data_input)) {
    stop(sprintf(
      "Row mismatch at j = %d: NB_tmp has %d rows, country_data_input has %d rows",
      j, nrow(NB_tmp), nrow(country_data_input)
                ))
    }
  # i <- 1
  for(i in 1:(num_param)){
    evppi[j,i] = evppi_gam ( NB=NB_tmp,
                             model_parameter_values=country_data_input[,i])
  }

}  


# plot EVPPI
evppi_long = data.frame (wtp=opt_wtp,evppi)%>% 
             pivot_longer(!wtp,names_to = "parameters", values_to = "evppi_value")

max(evppi_long$evppi_value)


wtp_max = evppi_long[which.max(evppi_long$evppi_value), "wtp"][[1]]


wtp_nonzero_counts <- evppi_long %>%
                      group_by(wtp) %>%
                      summarise(n_nonzero = sum(evppi_value != 0, na.rm = TRUE)) %>%
                      arrange(desc(n_nonzero))



# Step 2: Filter the data to just that wtp
evppi_at_wtp_max <- evppi_long %>%
                   filter(wtp == wtp_max)

# this is to set the y-axis, for Malawi, at very high WTP value, the max values are very high in some scenarios
if(ISO3== "MWI"){
  evppi_at_wtp_max <- evppi_long %>%
                      filter(wtp == wtp_nonzero_counts$wtp[1])
}


# Step 3: Rank parameters by evppi_value at that wtp, hence it is order in the ggplot
category_order = evppi_at_wtp_max %>%
                  group_by(parameters) %>%
                  summarise(evppi_at_peak = sum(evppi_value), .groups = "drop") %>%  # use `sum` or `mean` depending on structure
                  arrange(desc(evppi_at_peak)) %>%
                  pull(parameters)


evppi_long$parameters = factor(evppi_long$parameters, levels = category_order)

# rename levels instead of replacing values
new_levels <- category_order
new_levels <- gsub("CFR_op_perc", "Out/Inpatient CFR ratio", new_levels)
new_levels <- gsub("hosp", "inpatient", new_levels)
new_levels <- gsub("nonsev_op", "nonsev MA", new_levels)
new_levels <- gsub("op", "outpatient", new_levels)
new_levels <- gsub("mod_sev", "moderate/severe", new_levels)
new_levels <- gsub("nonmod", "nonSevere", new_levels)
new_levels <- gsub("prob", "%", new_levels)
new_levels <- gsub("ma", "MA", new_levels)
new_levels <- gsub("_", " ", new_levels)
new_levels <- gsub("RV3BB", "RV3-BB per dose", new_levels)

evppi_long$parameters_names <- factor(  evppi_long$parameters,  levels = category_order,  labels = new_levels)

summary(evppi_long$parameters_names)

param_levels <- factor(evppi_long$parameters_names)

# at least length(param_levels) colors in evppi_colors
evppi_cols_named <- setNames(evppi_colors[seq_along(param_levels)], param_levels)


Plot_evppi <- ggplot(evppi_long,
                     aes(x = wtp, y = evppi_value, colour = parameters_names)) +
                geom_line(linewidth = 1.1, show.legend = TRUE) +
                scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
                scale_color_manual(
                  values = evppi_colors   # first N colors used for N levels
                ) +
                guides(color = guide_legend(ncol = 1)) +
                labs(
                  y = "EVPPI (USD in millions)",
                  x = "Willingness to pay per DALY averted"
                ) +
                my_theme


Plot_evppi_all[[i_scen]]= Plot_evppi
  } # end of first loop

  names( Plot_evppi_all) =scen_scenario_opt
  
return(Plot_evppi_all)
}

# Plot_evppi_dop= get_EVPPI(select_sch_neonatal   = select_sch_neonatal)
  
  

evppi_gam <- function(NB,model_parameter_values){
  
  # note: current should have NB == 0
  D_opt  <- which(colSums(NB) != 0)
  D <- ncol(NB)
  N <- nrow(NB)
  
 
  g.hat_new <- matrix(0, nrow=N,ncol=D)
  
  for(d in D_opt)
  {
    model <- gam(NB[,d] ~ s(model_parameter_values,bs='cr')) #adjusted
 
    g.hat_new[,d] <- fitted(model)
  }  
  
  
  perfect.info  <- mean(apply(g.hat_new,1,max))
  baseline      <- max(colSums(g.hat_new)/N)
  
  partial.evpi  <- round(perfect.info - baseline, digits = 4) ## estimate EVPPI 
  
  return(partial.evpi)
}

