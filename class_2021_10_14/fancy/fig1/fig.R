p = GGally::ggpairs(mtcars)

ggplot2::ggsave("fig.png", p, width=10, height=10)
