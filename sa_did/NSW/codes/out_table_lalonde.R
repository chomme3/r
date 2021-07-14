# Create tables

out_table_lalonde <- function(sample1, benchmark = NULL){
 
  l1_lin <- cbind(sample1$linear$twfe$ATT,
                  sample1$linear$reg$ATT,
                  sample1$linear$ipw$ATT,
                  sample1$linear$std_ipw$ATT,
                  sample1$linear$dr_trad$ATT,
                  sample1$linear$dr_imp$ATT)
  
  l2_lin <- cbind(sample1$linear$twfe$se,
                  sample1$linear$reg$se,
                  sample1$linear$ipw$se,
                  sample1$linear$std_ipw$se,
                  sample1$linear$dr_trad$se,
                  sample1$linear$dr_imp$se)
  
  
  l4_lin <- cbind(sample1$dw$twfe$ATT,
                  sample1$dw$reg$ATT,
                  sample1$dw$ipw$ATT,
                  sample1$dw$std_ipw$ATT,
                  sample1$dw$dr_trad$ATT,
                  sample1$dw$dr_imp$ATT)
  
  l5_lin <- cbind(sample1$dw$twfe$se,
                  sample1$dw$reg$se,
                  sample1$dw$ipw$se,
                  sample1$dw$std_ipw$se,
                  sample1$dw$dr_trad$se,
                  sample1$dw$dr_imp$se)
  
  
  l7_lin <- cbind(sample1$aug_dw$twfe$ATT,
                  sample1$aug_dw$reg$ATT,
                  sample1$aug_dw$ipw$ATT,
                  sample1$aug_dw$std_ipw$ATT,
                  sample1$aug_dw$dr_trad$ATT,
                  sample1$aug_dw$dr_imp$ATT)
  
  l8_lin <- cbind(sample1$aug_dw$twfe$se,
                  sample1$aug_dw$reg$se,
                  sample1$aug_dw$ipw$se,
                  sample1$aug_dw$std_ipw$se,
                  sample1$aug_dw$dr_trad$se,
                  sample1$aug_dw$dr_imp$se)
  
  if(!is_null(benchmark)){
    l3_lin <- l1_lin/benchmark
    l6_lin <- l4_lin/benchmark
    l9_lin <- l7_lin/benchmark
  } else {
    l3_lin <- l1_lin*0
    l6_lin <- l4_lin*0
    l9_lin <- l7_lin*0
  }
  
  
  table <- rbind(l1_lin, l2_lin, l3_lin,
                 l4_lin, l5_lin, l6_lin,
                 l7_lin, l8_lin, l9_lin)
  
  colnames(table) <- c("TWFE", "REG", "IPW","STD-IPW",  "DR-TR", "DR-imp")
  rownames(table) <- c("lin_att", "lin_se", "lin_ev_bias",
                       "dw_att", "dw_se", "dw_ev_bias",
                       "aug_dw_att", "aug_dw_se", "aug_dw_ev_bias")
  return(table)
  
}