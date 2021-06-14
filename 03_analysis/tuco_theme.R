tuco_pal = c(rest = "#DDAA33",
             medium = "#BB5566",
             high = "#004488")

theme_tuco =
    egg::theme_article() +
    theme(text = element_text(size = 14) 
          #axis.title = element_text(size = 12),
          #legend.text = element_text(size = 10),
          #legend.title = element_text(size = 12)
        )

theme_set(theme_tuco)
