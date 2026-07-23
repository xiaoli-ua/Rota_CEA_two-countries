#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Plot the results of Two-way threshold analysis
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 

F_plot =function(p_list){
  
 # p_list <-    Plot_two_way_price_costSwitch[[1]]
 # p_list <-    Plot_two_way_price_Dop[[1]]
 
   f_level_schedule

  df_colour = data.frame(strategy = c("Rotarix_6to10","SuspVacc","Rotarix_6to10to14","Rotavac_6to10to14",neonatal_schedule),
                         colour = c(jama_colors[5],jama_colors[4],jama_colors[1],jama_colors[6],jama_colors[3]))
  
  df_colour_plot = df_colour %>% 
                   filter( strategy %in% as.character(f_level_schedule) )

  
  colour_plot = ggplot(  df_colour_plot, aes(x = strategy, y = 1, fill = strategy)) +
                        geom_bar(stat = "identity") +
                        scale_fill_manual(
                          values = setNames(df_colour_plot$colour,   df_colour_plot$strategy),
                          name = "Strategy"
                        ) +
                        theme_void() +
                        theme(
                          legend.position = "right",
                          legend.title = element_text(size = 16, face = "bold"),
                          legend.text = element_text(size = 14)
                        )
  
 
  
  legend_only <- cowplot::get_legend(  colour_plot)
  
  
  p_list_nolegend <- lapply(p_list, function(p) p + theme(legend.position = "none"))

  # arrange 5 plots + legend in a 3×2 grid
  combined_2way_plot  = ggarrange(p_list_nolegend[[1]], p_list_nolegend[[2]], p_list_nolegend[[3]],
                                p_list_nolegend[[4]], p_list_nolegend[[5]],   legend_only ,
                                ncol = 3, nrow = 2,
                                widths = c(1, 1, 1),   # adjust legend space width
                                heights = c(1, 1)
                                    )

  return(combined_2way_plot )
}


if(grepl("_dop", run_tag)){
  names(Plot_two_way_price_Dop)
  plot_results <- lapply(Plot_two_way_price_Dop, F_plot)

  for (i in seq_along(plot_results)) {
    plot_name <- names(plot_results)[i]        # use element name for file name
    file_path <-  sim_output_filename

    ggsave(
      filename =  paste0(sim_output_filename,paste0("/Plot_",run_tag,"_",plot_name,"_",num_sim,".pdf")),
      plot = plot_results[[i]],
      width = 297, height = 210, units = "mm"
    )
  }
  
}


if(bool_plot_threshold_price_TwoWay&grepl("_switchCost", run_tag)){
 
  combined_2way_plot_costSwitch <- F_plot(p_list =   Plot_two_way_price_costSwitch$basecase)
  combined_2way_plot_costSwitch_Societal <- F_plot(p_list = Plot_two_way_price_costSwitch$Societal)
 
  ggsave( plot = combined_2way_plot_costSwitch, 
          file = paste0(sim_output_filename,paste0("/Plot_",run_tag,"_costSwitch",num_sim,".pdf")),
          width = 297, height = 210 ,units = "mm") # A4 horizontal dimensions
  
  ggsave( plot = combined_2way_plot_costSwitch_Societal, 
          file = paste0(sim_output_filename,paste0("/Plot_",run_tag,"_costSwitch_Societal",num_sim,".pdf")),
          width = 297, height = 210, units = "mm") # A4 horizontal dimensions
  
} # end of the switch cost


if(bool_plot_threshold_price_TwoWay&grepl("_deliveryCost", run_tag)){
  
  combined_2way_plot_deliveryCost <- F_plot(p_list =  Plot_two_way_price_deliveryCost$basecase)
  combined_2way_plot_deliveryCost_Societal <- F_plot(p_list = Plot_two_way_price_deliveryCost$Societal)
  
  ggsave( plot = combined_2way_plot_deliveryCost, 
          file = paste0(sim_output_filename,paste0("/Plot_",run_tag,"_deliveryCost",num_sim,".pdf")),
          width = 297, height = 210 ,units = "mm") # A4 horizontal dimensions
  
  ggsave( plot = combined_2way_plot_deliveryCost_Societal, 
          file = paste0(sim_output_filename,paste0("/Plot_",run_tag,"_deliveryCost_Societal",num_sim,".pdf")),
          width = 297, height = 210, units = "mm") # A4 horizontal dimensions
  
} # end of the delivery cost

