#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => SCRIPT TO LOAD ALL PACKAGES AND FUNCTIONS
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
#############################################################################


library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(devtools)
library(dampack)
library(mgcv)
library(scales)
library(openxlsx)
library(gridExtra)
library(statmod) 
library(scales)
library(cowplot)



all_packages <- c('openxlsx','XML','MESS', 'compare','abind','scales','mgcv',
                  'ggplot2','dplyr','RColorBrewer','wbstats','grid','dplyr', 'ps',
                  'useful','simid.rtools','dampack', 
                  'tidyverse','tidyr','lubridate','ISOweek','splines',
                  'ggrepel','foreach','ggpubr','gamlss','stringr','RColorBrewer') 

# load the "simid.rtools" package for help functions on parallel computing etc.
# make sure it is at least package version 0.1.48
if('simid.rtools' %in% installed.packages()&&
   utils::compareVersion(paste(packageVersion("simid.rtools")),'0.1.48') < 0) {
  
  # to install packages from github 
  # For MAC OS: make sure that "XCode Command Line Tools" are installed on your system.
  # FYI: https://mac.install.guide/commandlinetools/4.html
  if(!'devtools' %in% installed.packages()){
    install.packages('devtools')
  }
  library(devtools)
  
  # install the simid.rtools package from GitHub
  devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
}
suppressPackageStartupMessages(library(simid.rtools))
# load the doParallel package separately so we can use the list 'all_packages' in a parallel foreach
all_packages_with_parallel <- c(all_packages,'doParallel')

# load all packages (and install them if not present yet)
smd_load_packages(all_packages_with_parallel)

