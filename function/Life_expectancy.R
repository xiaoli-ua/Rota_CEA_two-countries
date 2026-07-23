#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => Estimate life expectancy (undiscounted and discounted)
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
#############################################################################

get_LE_YLL = function(disc_effect){
    # disc_effect = run_config$disc_effect

  # introduce life table
  if(ISO3 == "MWI"){
   df_lifeExp_byAge = read.csv("./input/MWI_lifeExp.csv")
  }
  
  if(ISO3 == "GHA"){
  df_lifeExp_byAge = read.csv("./input/GHA_lifeExp.csv")
  }
  
  
# discounting the life year

df_lifeExp_byAge$discountedLY =NA_real_

for (i in 1: max(df_lifeExp_byAge$Age)){
  le <- df_lifeExp_byAge$Life_expectancy_atAge[i] # life expectance at the age
  df_discounting = data.frame( year = 0:(floor(le)-1),
                               undiscounted_ly =1)
  
  frac_part <- le - floor(le)  # check the decimal point
  
  if( frac_part >0){
    df_discounting[nrow(df_discounting) + 1, ] <- c(le - 1, frac_part)
  }
  
  df_discounting$discounted_ly <- df_discounting$undiscounted_ly / ((1 + disc_effect)^df_discounting$year)
  
  df_lifeExp_byAge$discountedLY[i] <- sum(df_discounting$discounted_ly)
}
  
df_lifeExp_byAge$discountedLY[df_lifeExp_byAge$Age==100] = df_lifeExp_byAge$Life_expectancy_atAge[df_lifeExp_byAge$Age==100] 

return(df_lifeExp_byAge)
}


