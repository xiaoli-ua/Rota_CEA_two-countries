#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Conduct full cost-effectiveness analysis 
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 



get_CEAC_EVPI = function(wtp_max,num_wtp){
  
# # willingness-to-pay values
# num_wtp <- 21 #default 80
# wtp_max <-1000  #default 1000

opt_wtp <- seq(0,wtp_max,length.out=num_wtp)  
  

sim_output_incr = L_incremental_analysis$sim_output_incr

summary(as.factor (sim_output_incr$schedule) )
summary(as.factor (sim_output_incr$scenario) )

# schedule, set names

name_schedule = unique(sim_output_incr$schedule)
name_scenario = unique(sim_output_incr$scenario)
name_scenario_schedule = paste0(rep(name_schedule,length(name_scenario)),"_",
                                rep(name_scenario, each = length(name_schedule)))
## prepare for CEAC
scen_scenario_opt = name_scenario 

my_theme <- theme(panel.background = element_rect(fill = "white"), # White background for the plot
                  legend.position = "none",
                  legend.text = element_text(size = 12),
                  axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12),
                  axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
                  axis.text.y  = element_text(size = 14),
                  panel.border = element_rect(color = "black", fill = NA), # Border line in black
                  )

# creat an empty list before the loop
L_CEAC_EVPI_all = list()

 i_scen =1

for (i_scen in 1:length(scen_scenario_opt) ){
  country_data = sim_output_incr[sim_output_incr$scenario ==scen_scenario_opt [i_scen], ]
  # country_data$schedule_scenario = paste0(country_data$schedule,"_",country_data$scenario)
  name_schedule_country = unique(country_data$schedule)
  
  # Sanity check
  # summary(as.factor (country_data$schedule) )
  # unique(country_data$schedule)
  # summary(as.factor (country_data$scenario) )

  # select correct perspectives
  use_societal <- grepl("Societal", scen_scenario_opt[i_scen])
  
  cost_col <- if (use_societal) "incr_cost_societal" else "incr_cost_HCP"
  
  L_country_by_schedule <- split(country_data, country_data$schedule)
  
  df_tmp = data.frame(wtp = opt_wtp,
                     matrix(NA_real_,nrow = num_wtp, ncol = length(name_schedule_country ))
                     )  # set up an empty data frame

  colnames(df_tmp)[-1] = as.character (name_schedule_country)

  prob_high_net_benefit      = df_tmp  #CEAC
  mean_net_benefit           = df_tmp # expected INMB for each intervention option, and each WTP value
  mean_net_loss              = df_tmp  # expected NL for each intervention option, and each WTP value
  prob_high_mean_net_benefit = df_tmp  # CEAF
  evpi                       = data.frame (wtp = opt_wtp, evpi=NA_real_)
  net_benefit_all            = vector("list", num_wtp)
  net_loss_all               = vector("list", num_wtp)

  # A double loop over WTP levels and number of schedule
  # i_wtp=1
  for (i_wtp in 1: num_wtp){
      df_net_benefit = data.frame(matrix(NA_real_, nrow = num_sim, ncol = length(name_schedule_country))
                                  ) # temperate data frame per wtp value
      colnames(df_net_benefit) = name_schedule_country  
  # Calculate the wtp per schedule 
      # i_schedule =2
      for (i_schedule in 1: length(name_schedule_country)){
        tmp <- L_country_by_schedule[[name_schedule_country[i_schedule]]]
        
        if (nrow(tmp) != num_sim) {
            stop(sprintf(
            "Schedule %s has %d rows, expected %d",
            name_schedule_country[i_schedule], nrow(tmp), num_sim
            ))
          } # end of if
        
        df_net_benefit[, i_schedule] = opt_wtp[i_wtp] * tmp$incr_DALY - tmp[[cost_col]]
        
      } # end of the first loop with i_schedule
        
    
    # check by row, which has the highest net benefit
    high_net_benefit = t(apply(X = df_net_benefit, MARGIN = 1,FUN=function(X){X==max(X,na.rm=T)}))
    
    # aggregate to get a probability
    prob <- colSums(high_net_benefit) / num_sim
    # sanity check
    sum(prob,na.rm=T) ==1
    # store probability of highest net benefit => CEAC
    colnames(prob_high_net_benefit[-1]) 
    prob_high_net_benefit[i_wtp,-1] <- prob
    
    # mean net benefit per intervention option, per WTP value (still in the loop)
    mean_nb   = colMeans(df_net_benefit) 
    # store mean net benefit => INMB plot
    mean_net_benefit[i_wtp,-1] = mean_nb 
    
    # if not the highest "mean net benefit", set NA => CEAF
    prob_mean <- rep(NA_real_, length(mean_nb))
    prob_mean[mean_nb == max(mean_nb)] = prob[mean_nb == max(mean_nb, na.rm = TRUE)]
    # store
    prob_high_mean_net_benefit[i_wtp, -1] <- prob_mean
  
    
    net_benefit_all[[i_wtp]] <- df_net_benefit
    
    # Compute net monetary loss for each intervention for each sample

    # Obtain intervention with highest INMB for each sample drawn with probabilistic sensitivity analysis
    max.str <- max.col(df_net_benefit)
 
    net.loss <- df_net_benefit[cbind(1:num_sim, max.str)] - df_net_benefit# Abraham :net_benefit[cbind(1:num_sim, max.str)] extract maximum (out of the the 5 alternative strategies) for each simultion 
    
    net_loss_all[[i_wtp]]=net.loss
    
    mean.net.loss = colMeans(net.loss)
    mean_net_loss[i_wtp,-1] <- mean.net.loss
    
} # end of second loop

length(net_benefit_all)
names(net_benefit_all) = paste0("WTP_", opt_wtp)

length(net_loss_all)
names(net_loss_all) = paste0("WTP_", opt_wtp)


# set up legend for plotting
net_benefit_legend = data.frame(df_colour,
                                product = c("Rotarix", "no vaccine", "Rotarix","Rotavac", "RV3BB"),
                                name_legend =c("Rotarix:6to10", "vaccine suspension", "Rotarix: 6to10to14", "Rotavac: 6to10to14", "RV3BB") )

# # get EVPI
evpi      = data.frame (wtp  = opt_wtp ,
                         evpi = apply(mean_net_loss[,-1],1,min))


df_CEAC_plot = prob_high_net_benefit%>%
               pivot_longer(!wtp, names_to = "schedule", values_to = "prob")

c_wtp = which("wtp" %in% colnames(prob_high_mean_net_benefit))

df_CEAF =data.frame(wtp =prob_high_mean_net_benefit$wtp,
                    ceaf = apply(prob_high_mean_net_benefit[,-c_wtp],1,max,na.rm=T))


## create the CEAC and CEAF graph

# set the colour
df_colour_plot = df_colour %>% 
                  filter( strategy %in% as.character(f_level_schedule) )
                
df_CEAC_plot$schedule = factor(df_CEAC_plot$schedule,
                               levels = df_colour_plot$strategy)

#Build a named palette vector keyed by the cleaned strategy names
pal_vec <- setNames(df_colour_plot$colour, df_colour_plot$strategy)




Plot_CEAC = ggplot(df_CEAC_plot, aes(x = wtp, y = prob, color = schedule)) +
                    geom_line(linewidth = 1.2) +
                    geom_line(data = df_CEAF,
                              aes(x = wtp, y = ceaf, linetype = "Frontier"),
                              color = "black", linewidth = 1.5) +
                    scale_linetype_manual(values = c("Frontier" = "dashed"))+
                    
                    # Vertical GDP reference lines
                    geom_vline( data = gdp_lines,
                      aes(xintercept = x),linetype = "dotted", color = "grey40" ) +
                    
                    # Labels for GDP lines
                    annotate("text",
                             x = gdp_lines$x, y = 1,
                             label = gdp_lines$label,
                             angle = 0, vjust = -0.2, size = 3, color = "grey30") +
                    scale_color_manual(values = pal_vec, breaks = levels(df_CEAC_plot$schedule)) +
                    guides(color = guide_legend(title = NULL)) +
                
                    coord_cartesian(ylim = c(0, 1.05), clip = "off") +
                    theme(plot.margin = margin(t = 15, r = 5, b = 5, l = 5)) +  
                    ylab("Probability") +
                    ylab("Probability") +
                    xlab("Willingness-to-pay per DALY averted") +
                    my_theme



# Expected Net Loss: curves and frontier plot

df_net_loss_plot = mean_net_loss %>%
                   pivot_longer(!wtp, names_to = "schedule", values_to = "NetLoss")

df_net_loss_plot$schedule = factor(df_net_loss_plot$schedule,
                                  levels = df_colour_plot$strategy)
max_evpi = max(evpi$evpi)*10


 # y_max = max_evpi
 y_max = 150000000
 
 
 
 Plot_ENL = ggplot(data = df_net_loss_plot, aes(x = wtp, y = NetLoss)) +
                    geom_line(aes(color = schedule), linewidth = 1.2) +
                    guides(color = guide_legend(title = NULL)) +
                    geom_line(data = evpi,
                              aes(x = wtp, y = evpi, linetype = "EVPI"),
                              color = "black", 
                              linewidth = 1.1 ) +
                    guides(linetype = guide_legend(title = NULL), linewidth = 1.5) +
                    ylab("Expected Net Loss") +
                    xlab("Willingness-to-pay per DALY averted") +
                    scale_linetype_manual(values = c("EVPI" = "dotted")) +
                    scale_color_manual(values = pal_vec, breaks = levels(df_net_loss_plot$schedule)) +
                    scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M") ) +
                    coord_cartesian(ylim = c(0, y_max)) +
                    geom_vline(data = gdp_lines,
                               aes(xintercept = x),
                               linetype = "dotted",
                               color = "grey40" ) +
                    annotate("text", 
                             x = gdp_lines$x, 
                             y = y_max,
                             label = gdp_lines$label,
                             angle = 0,
                             vjust = -0.2,
                             size = 3,
                             color = "grey30" ) +
                    my_theme


Plot_CEAC_ENL =  ggarrange(Plot_CEAC, Plot_ENL, nrow = 1, common.legend = TRUE)  


L_CEAC_EVPI = list(Plot_CEAC_ENL              = Plot_CEAC_ENL,
                   net_benefit_all            = net_benefit_all,
                   net_loss_all               = net_loss_all,
                   prob_high_net_benefit      = prob_high_net_benefit,
                   mean_net_benefit           = mean_net_benefit, 
                   mean_net_loss              = mean_net_loss, 
                   prob_high_mean_net_benefit = prob_high_mean_net_benefit,
                   evpi                       = evpi,
                   net_benefit_legend         = net_benefit_legend)

L_CEAC_EVPI_all [[i_scen]] =L_CEAC_EVPI


 } # end of the first loop
 names(L_CEAC_EVPI_all) = scen_scenario_opt

return(L_CEAC_EVPI_all)
}

# Plot_CEC_ENL=get_CEAC_EVPI (wtp_max =21,num_wtp =1000)

 