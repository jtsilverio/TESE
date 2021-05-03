library(ggplot2)

ggplot(tuco) +
    geom_point(aes(x = datetime, y = vedba)) +
    facet_wrap("ID", scales = "free_x")

