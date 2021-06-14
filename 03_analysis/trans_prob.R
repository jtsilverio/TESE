cov.index = 
    c(which(tuco$season == "March")[1],
      which(tuco$season == "July")[1],
      which(tuco$season == "October")[1],
      which(tuco$season == "February")[1])


df = data.frame(
    March = momentuHMM::getTrProbs(m2, covIndex = cov.index)[1,,1],
    July = momentuHMM::getTrProbs(m2, covIndex = cov.index)[1,,2],
    October = momentuHMM::getTrProbs(m2, covIndex = cov.index)[1,,3],
    February = momentuHMM::getTrProbs(m2, covIndex = cov.index)[1,,4]
)

#df$to_state = row.names(df)
df = tidyr::pivot_longer(df, 1:4, names_to = "season", values_to = "prob")
df$to_state = rep(c("rest → rest","rest → medium","rest → high"), each = 4)
df$season = factor(df$season, levels = c("March", "July", "October", "February"))
df = df %>% 
    filter(to_state != "rest → rest")

ggplot(df) +
    geom_point(aes(x = season, y = prob, color = to_state, group = to_state)) +
    geom_line(aes(x = season, y = prob, color = to_state, group = to_state)) +
    scale_y_continuous(limits = c(0, 0.05)) +
    theme_article() +
    ylab("Prob(Transition)") +
    scale_color_manual(values = unname(color_pal[3:2]))

                       