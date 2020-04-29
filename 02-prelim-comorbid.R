

# s$Adj_10[1:9, 1:9] <- 0
# s$Adj_10[10:16, 10:16] <- 0

plot(s,
     node_labels = c(rep("D", 9), rep("A", 7)),
     node_groups = c(rep("D", 9), rep("A", 7)),
     edge_multiplier = 3)$plt

     

pcor_var <- function(theta_ij, n, p){
  c <- p - 2
  (1 - theta_ij) / (n - c - 3)
}

variances <- 
  pcor_var(
  s$partials_non_zero[lower.tri(s$partials_non_zero)],
  515,
  16
)

pcors <- s$partials_non_zero[lower.tri(s$partials_non_zero)] 

abs(pcors / sqrt(variances)) > (0.087808) 


hyp <- c("1--13 > 0;
          6--16 = 0;
          1--13 > 0 > 6--16")
con <- confirm(sim_data[-split, ],
               hypothesis = hyp)
con

# focus on node 13
bridge(s$Adj_10, communities = c(rep("D", 9), rep("A", 7)))
