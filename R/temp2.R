library(grid)
library(ggplot2)
library(gridExtra)
dfs <- data.frame(x = c(1:9), y = c(2: 10))
p <- ggplot(dfs, aes(x = x, y = y))
p <- p + geom_point() + science_theme+
  theme(axis.text.x  = element_text(angle=45, vjust= 0, hjust = 1),
        axis.text.y  = element_text(hjust = 0))
p2 <- arrangeGrob(p, p, ncol = 1)
grid.draw(p2)

ggsave(filename = "tt.pdf", p2)

plots = lapply(1:5, function(.x) qplot(1:10,rnorm(10), main=paste("plot",.x)))
do.call(grid.arrange,  plots)
require(lattice)
grid.arrange(qplot(1:10), xyplot(1:10~1:10),
             tableGrob(head(iris)), nrow=2, as.table=TRUE, main="test main",
             sub=textGrob("test sub", gp=gpar(font=2)))
