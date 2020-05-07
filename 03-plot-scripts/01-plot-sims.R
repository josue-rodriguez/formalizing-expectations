# constraints_plot_data %>% 
#   filter(hypothesis == "H1") %>% 
#   ggplot(aes(n, mean_pmp)) +
#   geom_line(aes(group = const), size = 2.5) +
#   geom_line(aes(col = const), size = 1.5) +
#   # geom_ribbon(aes(ymin = mean_pmp - sd(mean_pmp), 
#   #                 ymax = mean_pmp + sd(mean_pmp), 
#   #                 fill = const),
#   #             alpha = 0.2) +
#   facet_wrap(~ sd, labeller = labeller(sd = label_parsed)) +
#   scale_x_continuous(breaks = seq(100, 1500, length.out = 5)) +
#   scale_colour_manual(values = pal, 
#                       name = "") +
#   # scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
#   # scale_linetype_manual(values = c(1, 2, 6)) +
#   # guides(col = guide_legend(override.aes = list(size = 1)),
#   #        linetype = guide_legend(override.aes = list(size = 0.4))) +
#   labs(x = "Sample Size",
#        y = y_lab) +
#   ylim(0, 1) + 
#   theme_bw(base_size = 14) +
#   theme(axis.text = element_text(size = 10),
#         legend.position = "top",
#         legend.title = element_text(size = 14),
#         legend.key = element_blank(),
#         panel.grid = element_line(size = 0.1,
#                                   color = "grey97"),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(1, "lines"),
#         axis.text.x = element_text(angle = 0,
#                                    size = 8,
#                                    hjust = 0.5,
#                                    vjust = 0.5),
#         strip.background = element_rect(fill = "grey94"))
# constraints_plot_data %>% 
#   ggplot(aes(n, mean_pmp)) +
#   geom_line(aes(col = hypothesis, linetype = const), size = 1) +
#   # geom_ribbon(aes(fill = hypothesis, ymin = pmp-scl, ymax = pmp+scl), alpha = 0.5) +
#   facet_wrap(~ sd, labeller = labeller(sd = label_parsed)) +
#   scale_x_continuous(breaks = seq(100, 1500, length.out = 5)) +
#   scale_colour_manual(values = pal, 
#                       name = "") +
#   scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
#   # scale_linetype_manual(values = c(1, 2, 6)) +
#   guides(col = guide_legend(override.aes = list(size = 1)),
#          linetype = guide_legend(override.aes = list(size = 0.4))) +
#   labs(x = "Sample Size",
#        y = y_lab,
#        linetype = linetype_lab) +
#   ylim(0, 1) + 
#   theme_bw(base_size = 14) +
#   theme(axis.text = element_text(size = 10),
#         legend.position = "top",
#         legend.title = element_text(size = 14),
#         legend.key = element_blank(),
#         panel.grid = element_line(size = 0.1,
#                                   color = "grey97"),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(1, "lines"),
#         axis.text.x = element_text(angle = 0,
#                                    size = 8,
#                                    hjust = 0.5,
#                                    vjust = 0.5),
#         strip.background = element_rect(fill = "grey94"))
#===================================================================
#===============
# Set up
#===============
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggpubr)
library(cowplot)

sim_constraints <- read.csv("05-data/01-sim-constraints.csv")
sim_sd <- read.csv("05-data/02-sim-sd.csv")

#============================
# Plot Constraint Simulation
#===========================

# tidy data
tidy_constraints <-
  sim_constraints%>% 
  as_tibble %>% 
  dplyr::select(-X)

# set up labels

#  std. devs to deltas
# BGGM:::delta_solve(c(0.25, 0.5)) # 15, 3

strip_labs <- c(expression(delta*" = 15"),
                expression(delta*" = 3"))
linetype_lab <- expression("Pr("*bolditalic(rho) %in% bold(Omega)~"|"~italic(H)[1]*")")
y_lab <- expression(italic(p)*"("*italic(H)[1]*"|"*bold(Y)*")")



# compute summary statistics and clean factor levels
constraints_plot_data <-
  tidy_constraints %>% 
  as_tibble %>% 
  pivot_longer(pmp1:pmp3,
               names_to = "hypothesis",
               values_to = "pmp",
               names_prefix = "pmp") %>% 
  group_by(sd, const, hypothesis, n) %>% 
  summarise(mean_pmp = mean(pmp),
            scl = sd(pmp),
            ub = mean_pmp + scl,
            lb = mean_pmp - scl) %>% 
  ungroup %>%  
  mutate(hypothesis = recode(hypothesis, "1" = "H1", "2" = "H2", "3" = "H3"),
         const = factor(const),
         # prior proportions in agreement with unconstrained [.04, .008, .001]
         # const = recode(const, "1" = ".04", "2" = ".008", "3" = ".001"),
         const = recode(const, "1" = ".04", "2" = ".008", "3" = ".001"),
         sd = factor(sd, labels = strip_labs))

# set color palette
# pal <- colorblind_pal()(8)[6:8]
pal <- c("#117733", "#44AA99", "#88CCEE")

# plot results
plots <- list()

for (i in 1:2) {
  # mask for filtering by std. devs
  mask <- levels(constraints_plot_data$sd)
  plots[[i]] <- 
  constraints_plot_data %>%
    filter(sd == mask[i], hypothesis == "H1") %>% 
    ggplot(aes(n, mean_pmp)) +
    geom_line(aes(col = const), size = 1, alpha = 0.75) +
    # geom_ribbon(aes(fill = const, ymin = lb, ymax = ub), alpha = 0.5) +
    # geom_line(aes(y = ub, group = const)) +
    facet_wrap(~ sd, labeller = labeller(sd = label_parsed)) +
    scale_x_continuous(breaks = seq(100, 1500, length.out = 5)) +
    scale_colour_manual(values = pal,
                        name = "") +
    scale_fill_manual(values = pal,
                      name = "") +
    # scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
    # scale_linetype_manual(values = c(1, 2, 6)) +
    guides(
      col = guide_legend(override.aes = list(size = 1, alpha = 1))
      # linetype = guide_legend(override.aes = list(size = 0.4))
           ) +
    labs(x = "Sample Size",
         y = y_lab,
         col = linetype_lab) +
    ylim(0.4, 1) +
    theme_bw(base_size = 14) +
    theme(axis.text = element_text(size = 10),
          legend.position = "top",
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
}

plots[[1]]
#============================
# Plot SD Simulation
#===========================

# tidy data
tidy_sd <- 
  sim_sd %>%
  as_tibble %>%
  select(-X)

# deltas 
# deltas <- BGGM:::delta_solve(c(0.2, 0.3, 0.4, 0.5)) # 15, 3
deltas <- BGGM:::delta_solve(c(0.1, 0.25, 0.4, 0.5)) # 15, 3

# compute summary statistics and clean factor levels
sd_plot_data <-   
  tidy_sd %>% 
  rename(pmp = h12) %>%
  mutate(sd = factor(sd)) %>%
  group_by(n, sd) %>%
  summarise(mean_pmp = mean(pmp)) %>% 
  mutate(sd = factor(sd),
         dummy = factor("1", labels = expression(italic(H)[1])))
# pal <- colorblind_pal()(8)[-c(2:4)]

# plot results
plots[[3]] <- 
  ggplot(sd_plot_data, aes(n, mean_pmp)) +
  geom_line(aes(col = sd), size = 1) +
  facet_wrap(~ dummy, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(100, 1500, length.out = 5)) +
  scale_color_grey(name = expression(delta),
                   labels = round(deltas, 2)) +
  # guides(col = guide_legend(override.aes = list(size = 2))) +
  labs(x = "N",
       y = "Posterior Model Probability") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = 10),
        legend.position = "top",
        legend.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.grid   = element_line(size = 0.1,
                                    color = "grey97"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        axis.text.x=element_text(angle = 0,
                                 size = 8,
                                 hjust = 0.5,
                                 vjust = 0.5),
        strip.background = element_rect(fill = "grey94"))
plots[[3]]

#================
# Combine plots
#===============

# plot first simulation
sim_1_legend <- 
  plot_grid(
    NULL, get_legend(plots[[1]]), NULL,
    ncol = 3,
    rel_widths = c(1,10,1)
  )
sim_1_results <- 
  plot_grid(
    plots[[1]] + theme(legend.position = "none") + xlab(""),
    plots[[2]] + theme(legend.position = "none") + ylab(""),
    ncol = 2
  ) 

panel_a <- 
  plot_grid(
    sim_1_legend,
    sim_1_results,
    nrow = 2,
    rel_heights = c(1, 10)
  )
panel_a

# plot second simulation
sim_2_legend <- 
  plot_grid(
    NULL,
    get_legend(plots[[3]]),
    ncol = 2,
    rel_widths = c(1, 10))

panel_b <- 
  plot_grid(
    sim_2_legend,
    plots[[3]] + theme(legend.position = "none") + labs(x = "", y = ""),
    nrow = 2,
    rel_heights = c(1, 10)
  )
panel_b

sim_results_plot <-
  plot_grid(
    NULL,
    plot_grid(
      panel_a ,
      NULL, 
      panel_b,
      ncol = 3,
      rel_widths = c(4, 0.25, 2)
    ),
    nrow = 2,
    rel_heights = c(1, 10)
  ) +
  draw_label("A",
             x = 0.025,
             y = 0.95,
             size = 35) +
  draw_label("B",
             x = 0.71,
             y = 0.95,
             size = 35)

sim_results_plot

# save
ggsave(
  "06-figs/03-plot-sims.pdf",
  sim_results_plot,
  width =  12,
  height = 5.5,
  units = "in",
  dpi = 320
)

