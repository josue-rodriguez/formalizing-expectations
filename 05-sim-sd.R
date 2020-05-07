library(foreach)
library(doParallel)
library(doSNOW)

ncores <- detectCores()
cl <- makeCluster(6)
registerDoSNOW(cl)
# registerDoParallel(cl)
# registerDoSEQ()

#================
# Set up data
#===============

# true partials
theta_ij <- -seq(0.1, 0.3, length.out = 6)

# precision matrix
Theta <- diag(4)
Theta[lower.tri(Theta)] <- theta_ij
Theta[upper.tri(Theta)] <- t(Theta)[upper.tri(Theta)]

# check values
true_pcors <- -cov2cor(Theta)
diag(true_pcors) <- 1
true_pcors

# precision mat ==> cov mat
# inverse of Theta is Sigma
S <- solve(Theta)

# hypotheses, with increasing number of constraints
hypothesis <- c("2--4 > 2--3 > 1--4 > 1--3 > 1--2")

#=============
# Simulation
#============

# helper function
sim_function <- function(n, S, hyp, prior_sd) {
  # generated_data <- bdgraph.sim(n = n, K = precision_mat)$data
  generated_data <- MASS::mvrnorm(n = n, mu = rep(0, ncol(S)), Sigma = S)
  test_hyp <- confirm(generated_data, 
                      hypothesis = hyp,
                      prior_sd = prior_sd,
                      iter = 50000)
  return(test_hyp)
}


# parameters to iterate through
N <- seq(100, 1500, by = 100)
iter <- 500
prior_sd <- c(0.1, 0.25, 0.4, 0.5)
# > BGGM:::delta_solve(prior_sd)
# [1] 99.00 15.00  5.25  3.00

# data.frame to store results
results <- data.frame()

# set progress bar
total_iter <- length(N) * length(prior_sd) * iter
pb <- txtProgressBar(max = total_iter, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# begin simulation
t0 <- Sys.time()

# ------ begin core simulation ---------------- |
results <- foreach(n = N,
                   .combine = rbind,
                   .options.snow = opts) %:%
  foreach(s = prior_sd,
          .combine = rbind) %:%
  foreach(icount(iter),
          .combine = rbind,
          .packages = c("BGGM")) %dopar% {
            
            sim <- sim_function(n = n, 
                                S = S, 
                                hyp = hypothesis, 
                                prior_sd = s)
            # pmp <- sim$post_prob
            h12 <- sim$out_hyp_prob[[1]]
            
            pmp_list <- c(h12 = h12, 
                          n   = n,
                          sd  = s)
            
            return(pmp_list)
            
          }

stopCluster(cl)
# --------------- end  ------------------- |


write.csv(results, "C:/Users/jer421/Box/bridge/data/03-sim-sd2.csv")
t1 <- Sys.time() - t0
print(t1)

library(dplyr)
library(ggplot2)
results %>%
  as_tibble %>%
  rename(pmp = h12) %>%
  mutate(sd = factor(sd)) %>%
  group_by(n, sd) %>%
  summarise(mu = mean(pmp)) %>%
  ggplot(aes(n, mu)) +
  geom_line(aes(col = sd)) +
  scale_color_discrete() +
  theme_bw()


# tst <- sim_function(100, k, hypothesis, 0.24)
# tst$post_prob
