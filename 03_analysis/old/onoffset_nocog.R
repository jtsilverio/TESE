## Função para calcular o onset e offset dos dados de um animal
onoffset_nocog = function(datetime, activity, decimal_time = T, mav.window = 72, format = "long"){
  
  # Combina dados de entrada em um dataframe, o que facilita a manipulação.
  df = data.frame(datetime, activity)
  df$date = as_date(df$datetime)
  df$time = lubridate::hour(df$datetime) + lubridate::minute(df$datetime)/60 + lubridate::second(df$datetime)/3600 # converting time to decimals numbers
  
  # smooth data #
  # calcula média móvel usando a função do pacote data.table.
  # padrão de n = 72; que é igual a 6 horas.
  if (mav.window != 0){
    df$activity = frollmean(df$activity, n = mav.window, align = "center", na.rm = T)
  }
  
  # omite NAs
  df = na.omit(df)
  
  onoffset.nocog = df %>% 
    group_by(date) %>% 
    summarise(mean.activity = mean(activity, na.rm = T), 
              onset = datetime[min(which(activity > mean.activity, arr.ind = T))], 
              offset = datetime[max(which(activity > mean.activity, arr.ind = T))])%>% 
    ungroup()
  
  if(decimal_time){
    onoffset.nocog$onset = hour(onoffset.nocog$onset) + lubridate::minute(onoffset.nocog$onset)/60
    onoffset.nocog$offset = hour(onoffset.nocog$offset) + lubridate::minute(onoffset.nocog$offset)/60
  }
  
  if(format == "long"){
    onoffset.nocog = pivot_longer(data = onoffset.nocog, 
                                  cols = c("onset","offset"), 
                                  names_to = "type", 
                                  names_ptypes = list(type = factor(levels = c("onset","offset"))), 
                                  values_to = "time")
  }
  
  return(onoffset.nocog)  
}