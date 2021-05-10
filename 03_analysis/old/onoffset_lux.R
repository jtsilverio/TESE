onoffset_lux = function(datetime, activity, decimal_time = TRUE, format = "long"){
	library(data.table)
	library(lubridate)
	library(tidyr)
	
	# Combina dados de entrada em um dataframe, o que facilita a manipulação.
	df = data.frame(datetime, activity)
	df$date = as_date(df$datetime)
	df$time = lubridate::hour(df$datetime) + lubridate::minute(df$datetime)/60 + lubridate::second(df$datetime)/3600 # converting time to decimals numbers
	
	
	onoffset.lux = df %>% 
		group_by(date) %>% 
		filter(activity > 2) %>% 
		summarise(onset = first(time), offset = last(time)) %>% 
		ungroup()
	
	if(format == "long"){
		onoffset.lux = pivot_longer(data = onoffset.lux, 
									cols = c("onset","offset"), 
									names_to = "type", 
									names_ptypes = list(type = factor(levels = c("onset","offset"))), 
									values_to = "time")
	}
	
	return(onoffset.lux)
}
