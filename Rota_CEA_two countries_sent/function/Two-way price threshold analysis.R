#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Conduct TWO-way threshold analysis: 
#### Price vs duration of protection and price vs. switch cost, Price vs. deilvery costs
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 


get_price_threshold_twoWay = function(wtp_max,second_parameters,opt_wtp_2way){
  
  # second_parameters = 'dop'  #  # "dop"#"cost_switch", "cost_delivery"

  num_wtp_2way <- length( opt_wtp_2way)
  
  v.wtp <-opt_wtp_2way 
  
  if(bool_plot_threshold_price_TwoWay  == TRUE) {
    sim_output_incr = L_incremental_analysis$sim_output_incr
  } else if  (bool_plot_threshold_price != TRUE) {
    print("STOP")
  }
  
  
  name_schedule = unique(sim_output_incr$schedule)
  name_scenario = unique(sim_output_incr$scenario)
  scen_scenario_opt = name_scenario 
  
  # take the output from the incremental analysis
  L_CEAC_EVPI_all =get_CEAC_EVPI (wtp_max =wtp_max,num_wtp =num_wtp_2way)
  
  net_benefit_legend  = L_CEAC_EVPI_all$basecase$net_benefit_legend 
  # ### write this manually for now
  c_dop = "Sche_1to6to10_dop"
  
  
  if (second_parameters == "dop") {
      param_opt <- c("cost_RV3BB", c_dop)
    } else if (second_parameters == "cost_switch") {
      param_opt <- c("cost_RV3BB", "cost_switch")
    } else if (second_parameters == "cost_delivery") {
      param_opt <- c("cost_RV3BB", "cost_delivery")
      } else {
      stop("Unknown second_parameters value")
      }
  
  num_param <- length(param_opt)
  

  # i_scen =2
  
  Plot_2wayprice_threshold <- vector("list", length = length(scen_scenario_opt) )

  for (i_scen in 1:length(scen_scenario_opt) ){
  
    L_CEAC_EVPI =   L_CEAC_EVPI_all[[i_scen]]

    net_benefit_all = L_CEAC_EVPI$net_benefit_all
  
    country_data = sim_output_incr[ sim_output_incr$scenario ==scen_scenario_opt [i_scen] ,]
    # sanity check
    summary(as.factor(country_data$scenario))
    summary(as.factor (country_data$schedule) )
    dim(country_data)
  
    country_param_temp = country_data[,param_opt]
    summary(country_param_temp) # switch cost is 0 for non-new 3 dose schedule
  
    if(ncol(country_param_temp) != length(param_opt)){
      smd_print('ISSUE ON PARAMETER SELECTION FOR PRICE THRESHOLD ANALYSIS',WARNING = TRUE)
      param_opt[!param_opt %in% colnames(country_param_temp)]
    }
  
    country_param = country_param_temp[1:num_sim,] # take the first set, but the input number should be the same for all 3
  # sanity check
    n_blocks = length(name_schedule)   
    
    block_size <- num_sim
    n_blocks = nrow(country_param) / block_size
    
    if (n_blocks != floor(n_blocks)) {
       stop("Data is not a multiple of num_sim")
    }
    
    blocks <- split(country_param, rep(seq_len(n_blocks), each = block_size))
    
    all_equal <- all(sapply(blocks[-1], function(x) identical(x, blocks[[1]])))
    
    all_equal # passed sanity check
    
  # need to over write the cost of switch
  if(second_parameters == "cost_switch"){
    c_switch= country_param_temp$cost_switch
    c_switch_nonzero = c_switch[c_switch!=0]
    
    if (length(c_switch_nonzero) < num_sim) {
      stop("Not enough non-zero cost_switch values for num_sim")
    }
    
    country_param$cost_switch = tail(c_switch_nonzero, num_sim) # need to use the last simulations
    # sanity check
    mean(country_param$cost_switch) # pass
  }
  
  summary(country_param)
  # check whether a price sensitivity plot is possible
  opt_cost <- unique(country_param)
  
  if(nrow(opt_cost )<=1){
    smd_print('Simulations results are not suited for threshold price sensitivity analysis')
    return(NULL)
  } 
  if(nrow(opt_cost )<30){
    smd_print('Not enough simulations for threshold price sensitivity analysis ("num_sim<30")')
    return(NULL)
  } 
  
  
  resolution = 100 #100 Influence the time of the analysis
  

  psa_inputs = country_param 
  colnames(psa_inputs)[2]='parameters'
  # cost vaccine vs. implementation

  ### set up for ploting
  ylab_th='RV3BB price per dose (USD)' #second parameter

  xlab_map <- list(cost_switch   = "Switch cost (USD)",
                    dop           = "Duration of protection (weeks)",
                    cost_delivery = "Delivery cost (USD)")
  
  if (!second_parameters %in% names(xlab_map)) {
      stop("Unknown second_parameters value")
   }
  
  xlab_th <- xlab_map[[second_parameters]]
 
  # set axis 
  lim_x_min = 0
  lim_x_max = round(max(psa_inputs$parameters) / 1e6) * 1e6
  
  if(second_parameters == "dop"){
    lim_x_min = round_down_next_25(min(sim_output[1:num_sim,grep("_dop",colnames(sim_output))],na.rm=T))
    lim_x_max =  round_up_next_25(max(sim_output[1:num_sim,grep("_dop",colnames(sim_output))],na.rm=T))
  }
  
  ## setup a list

  Plot_2wayprice_threshold_wtp <- vector("list", length = length(v.wtp))

   # k=3
  for (k in 1: length(v.wtp)){

  kchoose = which(v.wtp==v.wtp[k])
  kvalue = paste('WTP = USD',round(v.wtp[kchoose]),'per DALY averted')
  
  # standard care column needs to be in the front
  k_name = paste0('WTP_',v.wtp[kchoose])
  INBwithzero <- net_benefit_all[[k_name]]

  # which strategy has the highest ENB
  psa_nb = INBwithzero ##decision options: sampled net benefit values for each intervention
  
  #plot
  n <- nrow(psa_nb) # Returns the number of rows if the object has dimensions; otherwise, it returns the length of the object, never NU

  c_baseline_col = which (colnames(psa_nb) ==c_baseline)
  # this next part calculates INB from NB if there is no zero column
  
  if(sum(apply(psa_nb, 2, function(x) length(unique(c(x, 0))) == 1)) == 0) {
  
    psa_nb <- psa_nb - psa_nb[,  c_baseline]
  }
  
  D <- ncol(psa_nb) # number of decision options
  # regression models
  inputs <- psa_inputs[, c("cost_RV3BB","parameters")]
 
  reg_function <- function(x) gam(x ~ te(parameters, cost_RV3BB,bs='cr'), data = inputs)

  m <- lapply(psa_nb[, -c_baseline_col, drop = FALSE], reg_function) # baseline  column is dropped, m is gam predicted value of INB
  
 
  # make grid
  r <- sapply(inputs, range, na.rm = TRUE)
  ys <- seq(r[1,1], r[2,1], length.out = resolution)
  xs <- seq(r[1,2], r[2,2], length.out = resolution)
  

  # sample the values (resolution * resolistion 100)
  g <- cbind(rep(ys, each=resolution), rep(xs, time = resolution))
  colnames(g) <- colnames(r)
  # boxplot(g)
  g <- as.data.frame(g)

  p <- cbind (0,sapply(m, predict, newdata = g, type = "response"))  # predicted NB
  dim(p) # this is the results of all simulated values
  colnames(p)[1]=   c_baseline
  p <- p[, colnames(psa_nb), drop = FALSE] 
  
  # check which one
  class <- apply(p, 1, which.max) # highest 
  
  df_class =data.frame(cbind(g,p),
                       strategy = class)
  
  
  message("The proportion (%) of the space in which decision option d is optimal:")
  decision_option <- class
  print(round(prop.table(table(decision_option)) * 100, 1))
  


  
  df_plot_2way = df_class[,c("cost_RV3BB","parameters","strategy")]


  df_plot_2way = df_plot_2way %>%
                                mutate(schedule = case_when(strategy == 1 ~ colnames(psa_nb)[1],
                                                            strategy == 2 ~ colnames(psa_nb) [2],
                                                            strategy == 3 ~ colnames(psa_nb)[3],
                                                            strategy == 4 ~ colnames(psa_nb)[4],
                                                            strategy == 5 ~ colnames(psa_nb)[5])
                                )
    

  unique(df_plot_2way$strategy )
 


  
  unique_schedules <-   unique(df_plot_2way$schedule)

  level_colors <- c("Rotarix_6to10" =  jama_colors[5], # muted green
                    "SuspVacc" =  jama_colors[4], # brick red
                    "Rotarix_6to10to14" =  jama_colors[1], # dark blue-gray
                    "Rotavac_6to10to14" = jama_colors[6]) # purple
  
  level_colors[neonatal_schedule] <- jama_colors[3]# cyan-blue
  
  level_colors_filtered <- level_colors[unique_schedules]
  

  plot_2wayprice_threshold <- ggplot(df_plot_2way, aes(x = parameters, y = cost_RV3BB, fill = schedule)) +
                               geom_tile() +
                               scale_fill_manual(values = level_colors_filtered) +
                               labs(x = xlab_th, y = ylab_th, fill = "strategy") +
                               # xlim(c(lim_x_min, lim_x_max)) +
                               ggtitle(kvalue) +
                               theme(
                                 plot.title   = element_text(size = 14, face = "bold", hjust = 0.5),  # main title
                                 axis.title.x = element_text(size = 14, vjust = -0.5),
                                 axis.title.y = element_text(size = 14, vjust = 2),
                                 axis.text.x  = element_text(size = 14),
                                 axis.text.y  = element_text(size = 14),
                                 legend.position = "top",
                                 legend.title = element_text(size = 14, face = "bold"),
                                 legend.text  = element_text(size = 14),
                                 legend.key.size = unit(1.2, "cm"),
                                 legend.spacing.x = unit(0.5, "cm"),
                                 plot.margin = margin(10, 20, 10, 20)
                               )
   
   
   if(second_parameters == "cost_switch"){
       plot_2wayprice_threshold  <-  plot_2wayprice_threshold  + scale_x_continuous(
                                                                   labels = scales::label_number(scale = 1e-6, suffix = "M"),
                                                                   breaks = seq(0, 5e6, by = 1e6)
                                                                 )
   }
   
   if(second_parameters == "dop"){
     plot_2wayprice_threshold  <- plot_2wayprice_threshold  + xlim(c(lim_x_min, lim_x_max)) 
   }
   
    
  Plot_2wayprice_threshold_wtp[[k]]       =    plot_2wayprice_threshold
  names(Plot_2wayprice_threshold_wtp) [k] = paste0("WTP_",v.wtp[k])
  
  } # loop of WTP
  
  Plot_2wayprice_threshold [[i_scen]] = Plot_2wayprice_threshold_wtp

 } # loop of i_scen
  names(Plot_2wayprice_threshold) = scen_scenario_opt

return(Plot_2wayprice_threshold)
  } # end of the function 

