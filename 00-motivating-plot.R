setwd("C:/Users/josue/Box/bridge")

library(ggplot2)
library(ggnetwork)

# coordinates and labels for each node
coords <- 
  data.frame(x      = c(1, 1, 2, 2, 3, 3),
             y      = c(1, 3, 2, 4, 1, 3),
             comm   = rep(c("1", "2", "3"), each = 2),
             labels = c("A2", "A1", "B1", "B2", "C2", "C1"))


# hyp <- expression(italic(H)[1]*":"*
#                    ~bolditalic(rho)[A1]*">"~bolditalic(rho)[A2]
#                    *">"~bolditalic(rho)[C1]*">"~bolditalic(rho)[C2]
#                    *"> 0"
#                    )
motivating <-
  ggplot(coords, aes(x, y)) +
    # segments
    annotate(geom = "segment", 
             x    = c(1, 1, 1, 2, 2, 2, 3),
             xend = c(1, 2, 2, 2, 3, 3, 3),
             y    = c(1, 1, 3, 2, 2, 2, 1),
             yend = c(3, 2, 2, 4, 3, 1, 3)) +
    # corr labels
    annotate(geom  = "label",
             label = c(0.24, 0.32, 0.19, 0.12),
             x     = c(1.5, 1.5, 2.5, 2.5),
             y     = c(1.5, 2.5, 2.5, 1.5)) +
    # bridge symptom
    annotate(geom  = "text",
             label = "Bridge\nNode",
             x     = 2,
             y     = 0.85) +
    # arrow
    annotate(geom = "segment",
             arrow = arrow(angle = 45, unit(0.25, "cm"), type = "open"),
             x = 2,
             xend = 2,
             y = 1.05,
             yend = 1.355) +
    # hypothesis
    # annotate(geom  = "text",
    #          label = hyp,
    #          parse = TRUE,
    #          x     = 2,
    #          y     = 0.05,
    #          size  = 8)  +
    # nodes
    geom_point(size = 41) +
    geom_point(aes(fill = comm), size = 40, shape = 21) +
    geom_text(aes(label = labels), size = 10) +
    ggthemes::scale_fill_few() +
    # add some whitespace arround plot
    lims(x = c(0.5, 3.5), y = c(0, 5)) +
    guides(fill = FALSE) +
    theme_blank()

print(motivating)

ggsave(
  file   = "figs/00-motivating.pdf",
  plot   = motivating,
  dpi    = 360,
  width  = 8,
  height = 6,
  units  = "in",
  scale  = 1
)
