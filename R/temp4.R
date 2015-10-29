# top panels
p_no <- WBFig_sub(data = subset(TrtMean, variable == "no"), 
                  ylab = expression(NO[3]^'-'*-N~(ng~cm^"-2"~d^"-1"))) +
  theme(legend.position = c(.7, .89),
        axis.text.x  = element_blank(),
        plot.margin = unit(c(2, .5, 0, 0), "line"))

p_nh <- WBFig_sub(data = subset(TrtMean, variable == "nh"),
                  ylab = expression(NH[4]^'+'*-N~(ng~cm^"-2"~d^"-1"))) +
  theme(axis.text.x  = element_blank(),
        plot.margin = unit(c(2, .5, 0, 0), "line"))

# bottom panels
p_p <- WBFig_sub(data = subset(TrtMean, variable == "p"),
                 ylab = expression(PO[4]^'3-'*-P~(ng~cm^"-2"~d^"-1"))) +
  theme(plot.margin = unit(c(-.5, .5, 0, 0), "line"))
p_np <- WBFig_sub(data = subset(TrtMean, variable == "logNP"), 
                  ylab = "log(N:P ratios)") +
  theme(plot.margin = unit(c(-.5, .5, 0, 0), "line"))

g <- arrangeGrob(p_no, p_nh, p_p, p_np, nrow = 2)
grid.draw(g)
ggsave(filename = "test.pdf", g)
ggsave(filename = "test.pdf", p_p)

# install.packages("cowplot")
# library(cowplot)
# g2 <- plot_grid(p_no, p_nh, p_p, p_np, nrow = 2, align = "hv")
# grid.arrange(g2)
# ?plot_grid


gp1<- ggplot_gtable(ggplot_build(p_no))
gp2<- ggplot_gtable(ggplot_build(p_nh))
gp3<- ggplot_gtable(ggplot_build(p_p))
gp4<- ggplot_gtable(ggplot_build(p_np))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp3$widths[2:3], gp4$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
gp3$widths[2:3] <- maxWidth
gp4$widths[2:3] <- maxWidth

g3 <- arrangeGrob(gp1, gp2, gp3, gp4, nrow = 2)
grid.arrange(g3)
class(g3) <- c("gg", "ggplot", class(g3))
ggsave(filename = "test.pdf", g3)
ggsave(filename = "test.pdf", p_no)
?ggsave

ggsave <- ggplot2::ggsave
body(ggsave) <- body(ggplot2::ggsave)[-2]
body(ggplot2::ggsave)[2]
ggsave(filename = "test.pdf", g)
class(gp1)
class(g)
class(p_no)
?class


inherits(plot, "ggplot")
inherits(p_no, "ggplot")
?inherits











