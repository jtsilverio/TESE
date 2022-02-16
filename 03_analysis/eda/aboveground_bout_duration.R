# Do Running Length Decoding to get Start and End of Activity/Sleep
act_rle = tuco %>%
    group_by(ID, season) %>%
    summarise(status = rle(as.vector(state))$values,
              length = rle(as.vector(state))$lengths) %>%
    ungroup() %>% 
    mutate(end = cumsum(length),
           start = end - length + 1)

act_rle = act_rle %>% 
    group_by(ID, season) %>% 
    mutate(
        state = status,
        start = start,
        end = end,
        start_datetime = (tuco$datetime[start]),
        end_datetime = (tuco$datetime[end]))


x = act_rle %>% 
    group_by(ID,
             date = date(start_datetime),
             hour = lubridate::hour(start_datetime),
             state,
             season) %>% 
    summarise(length = mean(length), freq = n()) %>% 
    group_by(ID,
             hour,
             state,
             season) %>% 
    summarise(length = mean(length), freq = mean(freq))
    


ggplot(x, aes(length, fill = state, color = state)) +
    geom_density() +
    facet_grid(season~state, scales = "free_x")

ggplot(x, aes(freq, fill = state, color = state)) +
    geom_density() +
    facet_grid(season~state, scales = "free_x") +
    xlab("numero de bouts/hora")

ggplot(x, aes(freq, length, fill = state, color = state)) +
    geom_point(size = 0.8) +
    facet_grid(season~state,scales = "free_x") +
    xlab("numero de bouts/hora") +
    ylab("duração média do bout")

