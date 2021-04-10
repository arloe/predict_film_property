#' A function that calculate MAPE, RMSE (Quantitative evaluation)
#' 
#' @param data the dataset
#' @param actual the name of target variable
#' @param predict the name of predicted variable

eval_quantitative <- function(data, actual, predict){
  if( !data.table::is.data.table(x = df) ) stop(" 'df' must be data.table ")
  
  # define value
  actual_value  = df[, get(actual) ]
  predict_value = df[, get(predict) ]
  
  # calculate MAPE, RMSE
  MAPE = mean(x = abs(actual_value - predicted_value)/actual_value) * 100
  RMSE = sqrt(x = mean(x = (actual_value - predicted_value)^2 ))
  
  MAPE = round(x = MAPE, digits = 3)
  RMSE = round(x = RMSE, digits = 3)
  
  cat( "MAPE:", MAPE, "& RMSE:", RMSE )
}
