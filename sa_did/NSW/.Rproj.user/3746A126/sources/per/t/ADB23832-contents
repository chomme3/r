all_did_estimators <- function(y1, y0, d, x){
  #------------------------------------------------------------------------------------------------------
  #Two-way Fixed Effets
  #twfe_did <- did_twfe_panel(y1, y0, d, x, boot = boot, nboot = nboot)
  twfe_did <- DRDID::twfe_did_panel(y1, y0, d, x, boot = F, nboot = NULL)
  #Compute Regression estimates
  #reg_did <- did_reg_panel_linear(y1, y0, d, x, boot = boot, nboot = nboot)
  reg_did <- DRDID::reg_did_panel(y1, y0, d, x, boot = F, nboot = NULL)
  #Abadie's IPW
  #ipw_did <- did_ipw_panel(y1, y0, d, x, boot = boot, nboot = nboot)
  ipw_did <- DRDID::ipw_did_panel(y1, y0, d, x, boot = F, nboot = NULL)
  #Standardized IPW
  std_ipw_did <- DRDID::std_ipw_did_panel(y1, y0, d, x, boot = F, nboot = NULL)
  # "Traditional" Doubly-Robust: based on OLS regression and MLE pscore
  #dr_trad_did <- dr_did_panel_tr_linear(y1, y0, d, x, x, boot = boot, nboot = nboot)
  dr_trad_did <- DRDID::drdid_panel(y1, y0, d, x, boot = F, nboot = NULL)
  # "Bias-reduced" Doubly-Robust: based on prperly weighted OLS and IPS pscore
  #dr_br_did <- dr_did_panel_br_linear(y1, y0, d, x, boot = boot, nboot = nboot)
  dr_imp_did <- DRDID::drdid_imp_panel(y1, y0, d, x, boot = F, nboot = NULL)
  #------------------------------------------------------------------------------------------------------
  # Put all outputs in a list
  out <- list(twfe = twfe_did,
              reg = reg_did,
              ipw = ipw_did,
              std_ipw = std_ipw_did,
              dr_trad = dr_trad_did,
              dr_imp = dr_imp_did
  )
  #------------------------------------------------------------------------------------------------------
  return(out)
  #------------------------------------------------------------------------------------------------------
}
