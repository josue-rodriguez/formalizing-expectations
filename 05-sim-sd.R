library(foreach)
library(doParallel)
library(doSNOW)

ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoSNOW(cl)
registerDoParallel(cl)
registerDoSEQ()

#================
# Set up data
#===============

# precision matrix
lower_tri_k <- -seq(0.01, 0.15, length.out = 6)

k <- diag(4)
k[lower.tri(k)] <- lower_tri_k
k[upper.tri(k)] <- t(k)[upper.tri(k)]

# hypotheses, with increasing number of constraints
hypothesis <- c("2--4 > 2--3 > 1--4 > 1--3 > 1--2")

#=============
# Simulation
#============

# helper function
sim_function <- function(n, precision, hyp, prior_sd) {
  generated_data <- bdgraph.sim(n = n, K = precision)$data
  test_hyp <- confirm(generated_data, hypothesis = hyp, prior_sd = prior_sd)
  return(test_hyp)
}


# parameters to iterate through
N <- seq(100, 1500, by = 100)
prior_sd <- seq(0.2, 0.5, by = 0.1)
iter <- 500

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
                   # .options.snow = opts,
                   .verbose = TRUE) %:%
  foreach(s = prior_sd,
          .combine = rbind) %:%
  foreach(icount(iter),
          .combine = rbind,
          .packages = c("BGGM", "BDgraph")) %dopar% {
            
            sim <- sim_function(n = n, 
                                precision = k, 
                                hyp = hypothesis,
                                prior_sd = s)
            # pmp <- sim$post_prob
            h12 <- sim$post_prob[[1]]
            
            pmp_list <- c(h12 = h12, 
                          n   = n,
                          sd  = s)
            
            return(pmp_list)
            
          }

stopCluster(cl)
# --------------- end  ------------------- |


write.csv(results, "C:/Users/jer421/Box/bridge/data/03-sim-sd.csv")
t1 <- Sys.time() - t0
print(t1)

# library(dplyr)
# library(ggplot2)
# results %>% 
#   as_tibble %>% 
#   rename(pmp = h12) %>% 
#   mutate(sd = factor(sd)) %>% 
#   group_by(n, sd) %>% 
#   summarise(mu = mean(pmp)) %>% 
#   ggplot(aes(n, mu)) +
#   geom_line(aes(col = sd)) +
#   scale_color_discrete() +
#   theme_bw()


# tst <- sim_function(100, k, hypothesis, 0.24)
# tst$post_prob
