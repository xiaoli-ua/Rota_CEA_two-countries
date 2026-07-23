############################################################################ #
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => READ THE OUTPUTS OF THE DYNAMIC MODELS
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
############################################################################ #

# read the output file from the dynamic models for Ghana


get_dynamic_model_data_GHA = function(nrow_read) {
  
  # nrow_read = num_sim

# Specify the file path to the Excel file
get_file_name_ghana <- function(strategy_name){
  dir("./input",pattern = paste0("GHA_",strategy_name,'.xlsx'), full.names = TRUE)
}

# Specify the row and column numbers
# nrow_read = num_sim 
table1 = paste0("A2:GS",nrow_read+2)
table2 = paste0("GT2:OL",nrow_read+2)
table3 = paste0("ON2:OW",nrow_read+2)
table4 = paste0("OY2:PH",nrow_read+2)
table5 = paste0("PJ2:PS",nrow_read+2)



# Read the specific cell
# Susp Vacc
SuspVacc_modseve <- readxl::read_excel(get_file_name_ghana("novacc"), range = table1)
SuspVacc_nonseve <- readxl::read_excel(get_file_name_ghana("novacc"), range = table2)


 # Schedule 1-6-10 using Ghana Rotavac 6-10-14 dop
Rotavac_6to10to14_modseve <- readxl::read_excel(get_file_name_ghana("rotavac_6_10_14"), range = table1)
Rotavac_6to10to14_nonseve <- readxl::read_excel(get_file_name_ghana("rotavac_6_10_14"), range = table2)
 

# Schedule 1-6-10 # same response rate with rotavac
RV3BB_1to6to10_modseve <- readxl::read_excel(get_file_name_ghana("rv3bb_sampling_wv"), range = table1)
RV3BB_1to6to10_nonseve <- readxl::read_excel(get_file_name_ghana("rv3bb_sampling_wv"), range = table2)

# added an OPV reduced RV3-BB scenario 
RV3BB_1to6to10_reducedVE_modseve <- readxl::read_excel(get_file_name_ghana("opv_rv3bb_sampling_wv"), range = table1)
RV3BB_1to6to10_reducedVE_nonseve <- readxl::read_excel(get_file_name_ghana("opv_rv3bb_sampling_wv"), range = table2)

L_data_all=Filter(function(x) is(x, "data.frame"), mget(ls())) # pulls all data frames from the global environment into a named list.
names(L_data_all)

change_first_col <- function(df) {
  if (ncol(df) == 0) return(df)
  
  colnames(df)[1] <- "sim"
  df$sim = gsub("Simul_","",df$sim)
  return(df)
}

# Apply the function to each data frame in the list
L_data_all <- lapply(L_data_all, change_first_col)

#### doseage ==> put 3 dose into one data frame, then in the list

L_dose = list() 

L_dose$SuspVacc_dose1 <- readxl::read_excel(get_file_name_ghana("novacc"), range = table3)
L_dose$SuspVacc_dose2 <- readxl::read_excel(get_file_name_ghana("novacc"), range = table4)
L_dose$SuspVacc_dose3 <- readxl::read_excel(get_file_name_ghana("novacc"), range = table5) # sh
# check 
 sum(L_dose$SuspVacc_dose1) + sum(L_dose$SuspVacc_dose2) + sum(L_dose$SuspVacc_dose3)
 
L_dose$RV3BB_1to6to10_dose1 <- readxl::read_excel(get_file_name_ghana("rv3bb_sampling_wv"), range = table3)
L_dose$RV3BB_1to6to10_dose2 <- readxl::read_excel(get_file_name_ghana("rv3bb_sampling_wv"), range = table4)
L_dose$RV3BB_1to6to10_dose3 <- readxl::read_excel(get_file_name_ghana("rv3bb_sampling_wv"), range = table5)
# check 
sum(L_dose$RV3BB_1to6to10_dose1)
sum(L_dose$RV3BB_1to6to10_dose2)
sum (L_dose$RV3BB_1to6to10_dose3)

L_dose$Rotavac_6to10to14_dose1 <- readxl::read_excel(get_file_name_ghana("rotavac_6_10_14"), range = table3)
L_dose$Rotavac_6to10to14_dose2 <- readxl::read_excel(get_file_name_ghana("rotavac_6_10_14"), range = table4)
L_dose$Rotavac_6to10to14_dose3 <- readxl::read_excel(get_file_name_ghana("rotavac_6_10_14"), range = table5)
# check
sum(L_dose$Rotavac_6to10to14_dose1)
sum(L_dose$Rotavac_6to10to14_dose2)
sum(L_dose$Rotavac_6to10to14_dose3)


L_dose$RV3BB_1to6to10_reducedVE_dose1 <- L_dose$RV3BB_1to6to10_dose1
L_dose$RV3BB_1to6to10_reducedVE_dose2 <- L_dose$RV3BB_1to6to10_dose2
L_dose$RV3BB_1to6to10_reducedVE_dose3 <- L_dose$RV3BB_1to6to10_dose3


L_data = list(L_data_all = L_data_all,
              L_dose = L_dose)

return(L_data)

}

