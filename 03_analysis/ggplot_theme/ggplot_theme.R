tuco_pal = c(Rest = "#DDAA33",
             Medium = "#BB5566",
             High = "#004488",
             vedba = "#37474f")

theme_tuco =
    egg::theme_article() +
    theme(text = element_text(size = 14) 
          #axis.title = element_text(size = 12),
          #legend.text = element_text(size = 10),
          #legend.title = element_text(size = 12)
    )

theme_set(theme_tuco)