#---------------------------------------------------
#      Empirical Exercise
#      Effect of ACA on Health insurance
#---------------------------------------------------
#-----------------------------------------------------------------------------

# Load packages
#-----------------------------------------------------------------------------
# Libraries
library(readstata13)
library(ggpubr)

#Download latest version of did package and load it
devtools::install_github("bcallaway11/did")
library(did)
#Note: 

library(DRDID)
#Note: DiD with covariates

library(here)
#Note: Allows you to set the absolute path once

library(tidyverse)
library(dplyr)

library(fixest)
#Note: For fixed effects

library(estimatr)
#Note: Allows you to reliably estimate regressions


#--------------------------------------------------------
# Load data
acafs <- data.frame(read.dta13(here("data",'ehec_data.dta')))

# Only use 2 periods of data 2013 and 2014
acafs_sub <- filter(acafs, acafs$year==2014 | acafs$year==2013)
#--------------------------------------------------------


# Do some data manipulations

# Ensure that stfips are numeric
acafs_sub$stfips2 <- as.numeric(acafs_sub$stfips)

# Make sure year is numeric (and correct)
acafs_sub$year #Notice that this is a factor variable. Let's change that.
acafs_sub$year2 <- 1985 + as.numeric(acafs_sub$year)

# Create treatment group dummy
acafs_sub$treated <- as.numeric(acafs_sub$yexp2==2014)
acafs_sub$treated[is.na(acafs_sub$treated)] <- 0 #Replace missing values as 0 because they are never-treated

# Create Post dummy
acafs_sub$post <- as.numeric(acafs_sub$year==2014)
#--------------------------------------------------------


#--------------------------------------------------------
# Start the analysis
#--------------------------------------------------------

# Get TWFE coefficient
twfe <- fixest::feols(dins ~ treated + post + I(treated * post) + stfips, 
                      data = acafs_sub,
                      weights = ~W, 
                      cluster = ~stfips2)
#Note: We are using feols function from the fixest package for this. In theory we don't need "fixest::" because we already loaded the fixest package. But it is a good discipline to name the package because there may be multiple functions in R which have the same name but do different things! And R doesn't have a centralized way to resolve such conflicts. 
#Note: As with Stata, the intercept is built-in; don't need to include it in the regression equation.
#Note: This is like redhdfe in Stata
#Note: Adding state FEs would not change the results because they are time-invariant and are at the unit of analysis.


summary(twfe)


#--------------------------------------------------------
# Alternative ways of estimating the ATT
lm_reg <- lm_robust(dins ~ treated + post + I(treated * post), 
           data = acafs_sub,
           weights = W,
           cluster = stfips2)
#Note: Should return exactly the same coeff of interest as twfe above, but SEs will be slightly different.

summary(lm_reg)


#--------------------------------------------------------
# We can do it by brute-force, too:

mean_y_treat_post = mean(
  acafs_sub$W[acafs_sub$post==1 & acafs_sub$treated==1]*
  acafs_sub$dins[acafs_sub$post==1 & acafs_sub$treated==1]) / 
  mean(
    acafs_sub$W[acafs_sub$post==1 & acafs_sub$treated==1])
#Note: The brackets are like writing an "if" condition.
#Note: It is a weighted average, because we are multiplying by and then dividing by Ws.
  
mean_y_treat_pre = mean(
  acafs_sub$W[acafs_sub$post==0 & acafs_sub$treated==1]*
  acafs_sub$dins[acafs_sub$post==0 & acafs_sub$treated==1])/
  mean(
    acafs_sub$W[acafs_sub$post==0 & acafs_sub$treated==1])

mean_y_untreat_post = mean(
  acafs_sub$W[acafs_sub$post==1 & acafs_sub$treated==0]*
  acafs_sub$dins[acafs_sub$post==1 & acafs_sub$treated==0])/
  mean(
    acafs_sub$W[acafs_sub$post==1 & acafs_sub$treated==0])

mean_y_untreat_pre = mean(
  acafs_sub$W[acafs_sub$post==0 & acafs_sub$treated==0]*
  acafs_sub$dins[acafs_sub$post==0 & acafs_sub$treated==0])/
  mean(
    acafs_sub$W[acafs_sub$post==0 & acafs_sub$treated==0])

DiD_brute_force <- mean_y_treat_post - (mean_y_treat_pre + mean_y_untreat_post - mean_y_untreat_pre)
DiD_brute_force

twfe
