all_did_subsample <- function(subsample){
  #------------------------------------------------------------------------------------------------------
  # Attach the subsample
  base::attach(subsample)
  #------------------------------------------------------------------------------------------------------
  # create all specifications' sets of controls, outcomes pre and post, treatment
  # The different specifications
  x_linear <- cbind(age, educ, black, married, nodegree, hisp, re74)
  x_dw <- cbind(age, educ, black, married, nodegree, hisp, re74, ze74, agesq, agecub, educsq, educre74)
  x_aug_dw <- cbind(age, educ, black, married, nodegree, hisp, re74, ze74, agesq, agecub, educsq, educre74, mare74, maze74)
  # outcomes
  y1 <- c(re78)
  y0 <- c(re75)
  # Group/Treatment dummy
  d <- c(treated2)
  #------------------------------------------------------------------------------------------------------
  # detach the subsample
  base::detach(subsample, unload=FALSE)
  #------------------------------------------------------------------------------------------------------
  # Compute all DID's with X linear
  did_linear <- all_did_estimators(y1, y0, d, x_linear)
  # Compute all DID's with X as in DW
  did_dw <- all_did_estimators(y1, y0, d, x_dw)
  # Compute all DID's with X as in Augmented DW
  did_aug_dw <- all_did_estimators(y1, y0, d, x_aug_dw)

  #------------------------------------------------------------------------------------------------------
  # put all outputs in a list
  all_did <- list( linear = did_linear,
                   dw = did_dw,
                   aug_dw = did_aug_dw)
  return(all_did)
} 