library(ggplot2)
library(egg)
library(patchwork)

plants = read.csv("01_data/plants/plant_species.csv")
plants = data.frame(table(plants))
plants = plants[order(plants$Freq, decreasing = T),]
n = sum(plants$Freq)


plot_family = ggplot(plants, aes(reorder(family, -(Freq/sum(Freq))), Freq)) +
    geom_bar(stat = "identity", fill = "darkolivegreen", color = "darkolivegreen") +
    xlab("") +
    ylab("Relative Frequency") +
    theme_article() +
    theme(panel.grid.major.y = element_line(color = "lightgrey", linetype = "dashed"))


plot_species = ggplot(plants, aes(reorder(Species, -(Freq/sum(Freq))), Freq)) +
    geom_bar(stat = "identity", fill = "darkolivegreen", color = "darkolivegreen") +
    xlab("") +
    ylab("Relative Frequency") +
    theme_article() +
    scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) +
    theme(panel.grid.major.y = element_line(color = "lightgrey", linetype = "dashed"),
          axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

plot = plot_family / plot_species + plot_annotation(tag_levels = 'A')

ggsave("05_figures/plants/plant_frequency.png", plot, "png", width = 7, height = 10)
