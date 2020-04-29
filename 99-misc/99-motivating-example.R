setwd("C:/Users/josue/Box/bridge")

library(BDgraph)
library(ggnetwork)
library(network)
library(BGGM)
library(cowplot)

set.seed(5)

# - - - - create covariance matrix, used as example partial correlation values - - |
s <- matrix(0,
            nrow     = 6,
            ncol     = 6,
            dimnames = list(LETTERS[1:6], LETTERS[1:6]))



s[lower.tri(s)] <- c(0.33, 0, 0, 0.27, 0, # A neighbors
                     0, 0, 0.29, 0, # B
                     0.33, 0.18, 0, # C
                     0.17, 0, # D
                     0.34) # E

s[upper.tri(s)] <- t(s)[upper.tri(s)]
diag(s) <- 1

# - - - - create adjacency matrix for conditional dependence - - |
a_cd <- ifelse(s == 0, 0, 1)

# - - - create network  - - |
pcor_cd <- as.network(a_cd,
                      directed    = FALSE,
                      ignore.eval = FALSE,
                      names.eval  = "weights")

set.edge.value(pcor_cd, "weights", s)

# convert weights to string, remove leading zeros
set.edge.value(pcor_cd, "weights_c", numform::f_num(s, digits = 2))

set.vertex.attribute(pcor_cd, "membership", value = rep(c("1", "2", "3"), each = 2))
set.vertex.attribute(pcor_cd, "node_labels", value = LETTERS[1:6])

set.edge.attribute(pcor_cd, "is_bridge", as.character(c(2, 1, 1, 2, 1, 1, 2)))

# color palette
pal <- ggthemes::few_pal()(8)
node_cols <- c("1" = pal[1], "2" = pal[2], "3" = pal[3])


edge_weights <- c(0.2, 1.2, 1.2, 0.2, 0.6 , 0.6, 0.2)
edge_cols <- ifelse(edge_weights == 0.2, "gray70", ifelse(edge_weights == 0.6, "gray60", "gray50"))
node_shape <- ifelse(edge_weights == 0.2, 1, 2)

# net_cd <-
  ggplot(pcor_cd,
         aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(linetype = is_bridge)) +
    geom_edgelabel(aes(label = weights_c)) +
    geom_nodes(aes(color = membership), size = 10) +
    geom_nodetext(aes(label = node_labels)) +
    scale_color_manual(values = pal) +
    theme_blank() +
    theme(legend.position = "top")
  

# - - - - create adjacency matrix for conditional independence - - |
# a_ci <- 1 - a_cd

# - - - create network  - - |
# pcor_ci <- as.network(a_ci,
#                       directed    = FALSE,
#                       ignore.eval = FALSE,
#                       names.eval  = "weights")


# net_ci <-
#   ggnet2(net             = pcor_ci, 
#          mode            = "circle",
#          label           = TRUE,
#          edge.color      = "gray80",
#          edge.size       = 0.5,
#          edge.alpha      = 1,
#          edge.lty        = "dotted",
#          edge.label.size = 5,
#          node.label      = LETTERS[1:6],
#          node.color      = node_cols,
#          node.size       = 12,
#          node.alpha      = 1) +
#   theme(plot.margin = unit(c(1, 1, 1, 0), "cm"))


l1 <- substitute(bold("A")^italic("CD"))
# l2 <- substitute(bold("A")^italic("CI"))


# final plot
# motivating <-
#   plot_grid(
#     net_cd,
#     net_ci,
#     nrow = 1
#   ) + 
#     draw_label(label = l1,
#                x     = 0.1,
#                y     = 0.95, 
#                size  = 25) + 
#     draw_label(label = l2,
#                x     = 0.5,
#                y     = 0.95, 
#                size  = 25) 
motivating <-
  net_cd + 
    draw_label(label = l1,
               x     = 0.1,
               y     = 0.95, 
               size  = 25)  

print(motivating)

# ggsave(
#   file   = "figs/00-motivating.pdf",
#   plot   = motivating,
#   dpi    = 360,
#   width  = 8,
#   height = 5,
#   units  = "in",
#   scale  = 1
# )
