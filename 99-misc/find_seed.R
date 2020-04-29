# libraries
library(BGGM)
library(MASS)
library(networktools)
library(network)
library(ggplot2)
setwd("C:/Users/jer421/Box/bridge/")
source("03-plot-adj.R")


for (i in 1:2000) {
set.seed(i)

net_data <- read.csv("data/cov-beard-depanx.csv")[, -1]

sim_data <- MASS::mvrnorm(n = 1029, mu = rep(0, 16), Sigma = net_data, empirical = TRUE)
col_names <- 
  sub(pattern = "[A-Z].",
      replacement = "",
      x = colnames(net_data)) 

col_names <- tolower(col_names)

colnames(sim_data) <- col_names
node_names <- 
  c(
    paste0("D", 1:9),
    paste0("A", 1:7)
  )

comms <- c(
  rep("depression", 9),
  rep("anxiety", 7)
)


pal <-  ggthemes::colorblind_pal()(8)[c(4, 6)]

split <- sample(1:1029, size = floor(1029 * .5))

exp1 <- explore(sim_data[split, ])
slct1 <- BGGM::select(exp1, alternative = "greater", BF_cut = 3)
pcors_exp <- round(slct1$partials_positive, 2)
# pcors_exp <- round(slct1$partials_non_zero, 2)
dimnames(pcors_exp) <- list(node_names, node_names)
bridge_strengths <- sort(bridge(pcors_exp, communities = comms)$`Bridge Strength`, decreasing = TRUE) 
bridge_strength_cutoff <- quantile(bridge_strengths, 0.9)
X <- bridge_strengths[bridge_strengths > bridge_strength_cutoff]
print(paste("iteration: ", i))
if (all(names(X) == c("D8", "D6"))) message(i)
}