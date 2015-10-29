require(ggplot2)
pl <- lapply(1:11, function(.x) qplot(1:10,rnorm(10), main=paste("plot",.x)))
ml <- do.call(marrangeGrob, c(pl, list(nrow=2, ncol=2)))
## interactive use; open new devices
ml
## non-interactive use, multipage pdf
ggsave("multipage.pdf", ml)

devtools::install_github("jrnold/ggthemes")
library(ggthemes)

science_theme <- theme(panel.border = element_rect(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       axis.ticks.length = unit(-.2, "lines"),
                       # axis.ticks.margin = unit(.5, "lines"),
                       legend.position = c(.5, .93), 
                       legend.title = element_blank(),
                       legend.key.width = unit(2.5, "lines"),
                       legend.key.height = unit(.8, "lines"),
                       legend.key = element_blank(),
                       legend.background = element_rect(fill = "transparent", colour = NA))
