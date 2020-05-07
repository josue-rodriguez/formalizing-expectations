library(BGGM)
library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(2)
# use random data, does not matter
d <- MASS::mvrnorm(n = 1e5, mu = rep(0, 2), Sigma = diag(2))

# 
# BGGM:::delta_solve(c(0.05, 0.1, 0.25, 0.5))
adj <- 3

# prior sd = 0.10.5, delta = 99
# x <- explore(d, prior_sd = 0.05, iter = 1e5)
# delta_399 <- density(x$prior_samp$pcors[1, 2, ], adjust = adj)
# df399 <- data.frame(x = delta_399$x, y = delta_399$y)

# prior sd = 0.10, delta = 99
x <- explore(d, prior_sd = 0.10, iter = 1e6)
delta_99 <- density(x$prior_samp$pcors[1, 2, ], adjust = adj)
df99 <- data.frame(x = delta_99$x, y = delta_99$y)

# prior sd = 0.25, delta = 15
x <- explore(d, prior_sd = 0.25, iter = 1e6)
delta_15 <- density(x$prior_samp$pcors[1, 2, ], adjust = adj)
df15<- data.frame(x = delta_15$x, y = delta_15$y)
# prior sd = 0.50, delta = 3  
x <- explore(d, prior_sd = 0.50, iter = 1e6)
delta_03 <- density(x$prior_samp$pcors[1, 2, ], adjust = adj)
df03<- data.frame(x = delta_03$x, y = delta_03$y)
# delta_labs <- c(
#   expression(delta * " = 3"),
#   expression(delta * " = 15"),
#   expression(delta * " = 99")
# )

plot_data <-
  bind_rows(`99` = df99, `15` = df15, `3` = df03, .id = "delta") %>% 
  as_tibble %>% 
  mutate(delta = factor(delta, levels = c("99", "15", "3")))

pal <- c("#000000", "#009E73", "#0072B2")


priors_plot <- 
  ggplot(plot_data, aes(x, y)) +
    geom_line(aes(col = delta), size = 1.1) +
    # scale_color_manual(values = pal, name = expression(delta)) +
    ggthemes::scale_color_colorblind(name = expression(delta)) +
    scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, expand = c(0.01,0)) +
    labs(x = expression("Implied Prior Distribution for "* italic(rho[ij])),
         y = "") +
    guides(col = guide_legend(override.aes = list(size = 2))) +
    theme_bw(base_size = 14) +
    theme(axis.text = element_text(size = 10),
          legend.title = element_text(size = 14),
          legend.key = element_blank(),
          panel.grid = element_line(size = 0.1,
                                    color = "grey97"),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1, "lines"),
          axis.text.x = element_text(angle = 0,
                                     size = 8,
                                     hjust = 0.5,
                                     vjust = 0.5),
          strip.background = element_rect(fill = "grey94"))
priors_plot
   
ggsave(
  "06-figs/04-plot-priors.pdf",
  priors_plot,
  dpi = 320,
  height = 3,
  width = 4,
  units = "in"
)
