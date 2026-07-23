#############################################################################
# This file is part of the project 
# "Cost-Effective Threshold Price for Alternative Infant and Neonatal Rotavirus Vaccines: A Dual-Country Evaluation"
# 
# => SCRIPT TO LOAD ALL HELP FUNCTIONS
#
#  Copyright 2026, CHERMID, UNIVERSITY OF ANTWERP
#############################################################################


### load help functions
##############
# functions for uncertainty 
##############

# get standard error based on confidence interval
get_se_based_on_ci=function(ul,ll){
  return((ul-ll)/(2*1.96))
}

#### function to sample gamma distribution

sample_rgamma <- function(mean,stdev,num_sim){
  alpha <- (mean^2) / (stdev^2)
  beta  <- (stdev^2) / (mean)
  sample <- rgamma(num_sim,shape=alpha,rate = 1/beta)
} # end of rgamma

#### function to sample lognromal
sample_lognormal <- function(mean,stdev,num_sim){
  a <- log((mean^2) / sqrt(mean^2 + stdev^2))
  s <- sqrt(log(1+ stdev^2/mean^2))
  sample <- rlnorm(n=num_sim,meanlog=a[1],sdlog = s[1])
} # end of r_lognormal


# help function
F_sample_lognormal_dist <- function(num_sim,distr_mean,distr_ul,distr_ll){
  # sample from Beta distribution
  lognormal.mu=log(distr_mean^2/sqrt(((distr_ul-distr_ll)/3.92)^2 + distr_mean^2))
  lognormal.sigma=sqrt(log(((distr_ul-distr_ll)/3.92)^2/ distr_mean^2+1))
  sample_all <- rlnorm(num_sim,lognormal.mu,lognormal.sigma)
  
  
  # # truncate negative values
  # sample_all[sample_all<0] <- 0 
  
  #return samples
  return(sample_all)
  
} # end of the sample lognormal distribution

# sample from Beta distribution

F_sample_beta_dist <- function(num_sim,distr_mean,distr_sd){
  # sample from Beta distribution
  beta.a=(distr_mean^2*(1-distr_mean)/distr_sd^2)-distr_mean
  beta.b=beta.a*(1-distr_mean)/distr_mean
  sample_all <- rbeta(num_sim,shape1=beta.a,shape2=beta.b)
  
  # truncate negative values
  sample_all[sample_all<0] <- 0 
  
  #return samples
  return(sample_all)
  
}

# calculate the net benefit, given a price per DALY 'p'
get_net_benefit <- function(daly_averted,incr_cost_disc,wtp_level)
{
  NB <-   wtp_level*daly_averted - incr_cost_disc
  return(NB)
}

# round up and down
round_up_next_25 <- function(x) {
  ceiling(x / 25) * 25
}

round_down_next_25 <- function(x) {
  if (x <= 25) {
    return(0)  # Ensure it doesn't go below 25
  } else {
    return(floor(x / 25) * 25)
  }
}

