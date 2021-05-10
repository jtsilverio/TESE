datetime.to.decimal = function(datetime, digits = 2){
  library(lubridate)
  decimal_time = round(lubridate::hour(datetime) + lubridate::minute(datetime)/60 + lubridate::second(datetime)/3600, digits = digits)
  return(decimal_time)
}