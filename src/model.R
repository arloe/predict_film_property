#' A function that estimate linear regression with robust sandwich variance
#' 
#' @param formula The formula to use in the model
#' @param data Data to use

model <- function( formula, data ){
  # define argument
  formula = as.formula( object = formula )
  
  # estimate linear regression
  fit = lm( formula = formula, data = data )
  
  # estimate robust sandwich variance
  sandwich_se = as.vector( x = diag(x = sandwich::vcovHC(x = fit, type = "HC") )^.5 )
  
  # confidence interval
  coef_fit = as.vector(x = coef(object = fit))
  z_value  = qnorm(p = .025, mean = 0, sd = 1, lower.tail = FALSE)
  upper = coef_fit + sandwich_se * z_value
  lower = coef_fit - sandwich_se * z_value
  
  # t-statistic & p-value
  t_stat   = coef_fit/sandwich_se
  p_values = pchisq(q = t_stat^2, df = 1, lower.tail = FALSE)
  
  # output
  out = cbind( coef_fit, sandwich_se, t_stat, p_values, upper, lower )
  colnames(x = out) = c("Estimate", "Std(Robust)", "t value", "Pr(>|t|)", "C.I.(Upper)", "C.I.(lower)")
  rownames(x = out) = names(x = coef(object = fit))
  
  out = round(x = out, digits = 3)
  
  return( list(model = fit, coefficient = out) )
}