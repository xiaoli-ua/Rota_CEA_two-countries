#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Estimate the Years of life lost (YLL) by age
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################# 

F_YLL_byAge = function(vac_schedule,byAge){

  # vac_schedule = run_config$schedule
  # byAge = age_group[1]

  # df_modseve_byAge_death = F_deaths_byAge (vac_schedule =run_config$schedule,  byAge=byAge)
  # c_select <- which(!colnames(df_modseve_byAge_death) %in% c("sim", "age_group", "mid_age"))
  
  df_modseve_byAge_death = F_deaths_byAge (vac_schedule =run_config$schedule,
                                           byAge=byAge)
  X <- df_modseve_byAge_death[, !colnames(df_modseve_byAge_death) %in% c("sim", "age_group", "mid_age") ]

  mid_age_val <- unique(df_modseve_byAge_death$mid_age)
  ### need to calculate undiscounted and discounted life year losses
  

  if (length(mid_age_val) != 1) {
    stop("Expected exactly one unique mid_age in df_modseve_byAge_death")
  }

  life_exp <- df_lifeExp_byAge$Life_expectancy_atAge[df_lifeExp_byAge$Age == mid_age_val]
  disc_life_exp <- df_lifeExp_byAge$discountedLY[df_lifeExp_byAge$Age == mid_age_val]

  if (length(life_exp) != 1 || length(disc_life_exp) != 1) {
    stop("Could not find exactly one matching life expectancy value for mid_age")
  }

  df_YLL_undiscounted <- X * life_exp
  colnames(df_YLL_undiscounted) <- paste0(colnames(df_YLL_undiscounted), "_undisc")

  df_YLL_discounted <- X * disc_life_exp
  colnames(df_YLL_discounted) <- paste0(colnames(df_YLL_discounted), "_disc")

  df_YLL <- cbind(df_YLL_undiscounted,
                  df_YLL_discounted)

  return(df_YLL)
}




F_boxplot = function (df_long){
  
  # df_long = sim_output

  P_boxplot_disc = ggplot(data=df_long , aes(x= schedule, y=YLL_disc,group = schedule, colour=schedule)) + 
                            geom_boxplot() + ggtitle("disc") +
                            ylab("YLL") + ylim(c(0,max(df_long $YLL_undisc))) +
                            theme(axis.text.x = element_text(angle = 90,size = 8))
  
  P_boxplot_undisc = ggplot(data=df_long , aes(x= schedule, y=YLL_undisc,group = schedule, colour=schedule)) + 
                            geom_boxplot() + ggtitle("undisc")+
                            ylab("YLL") + ylim(c(0,max(df_long $YLL_undisc))) +
                            theme(axis.text.x = element_text(angle = 90,size = 8))
  
  P_boxplot = ggarrange( P_boxplot_undisc +theme(legend.position="none"), 
                         P_boxplot_disc +theme(legend.position="none") ,nrow = 1)
  
  return( P_boxplot)
}


# P_boxplot_perSchedule = F_boxplot(df_long = sim_output)






