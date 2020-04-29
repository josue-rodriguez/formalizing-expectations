library(foreach)
library(doParallel)
library(doSNOW)

#================
# Set up data
#===============

# precision matrix
lower_tri_k <- -seq(0.01, 0.15, length.out = 6)

k <- diag(4)
k[lower.tri(k)] <- lower_tri_k
k[upper.tri(k)] <- t(k)[upper.tri(k)]

# hypotheses, with increasing number of constraints
hypotheses <- c("2--3 > 1--4 > 1--3 > 1--2;
                (2--3 , 1--4 , 1--3 , 1--2) = 0",
                
                "2--4 > 2--3 > 1--4 > 1--3 > 1--2;
                (2--4 , 2--3 , 1--4 , 1--3 , 1--2) = 0",
                
                "3--4 > 2--4 > 2--3 > 1--4 > 1--3 > 1--2;
                (3--4 , 2--4 , 2--3 , 1--4 , 1--3 , 1--2) = 0")

#=============
# Simulation
#============

# helper function
sim_function <- function(n, precision, hyp, prior_sd) {
  generated_data <- bdgraph.sim(n = n, K = precision)$data
  test_hyp <- confirm(generated_data, hypothesis = hyp, prior_sd = prior_sd)
  return(test_hyp)
}


# grab prior probabiltie s,
# sim_function(100, k, hypotheses[1], 0.25)$BF_computation # ~0.04
# sim_function(100, k, hypotheses[2], 0.25)$BF_computation # ~ .008
# sim_function(100, k, hypotheses[3], 0.25)$BF_computation # ~ .001

# parameters to iterate through
N <- seq(100, 1500, by = 100)
prior_sd <- c(0.25, 0.50)
iter <- 500

# data.frame to store results
results <- data.frame()

# make and register clusters
ncores <- detectCores()
cl <- makeCluster(ncores)
registerDoSNOW(cl)

# set progress bar
total_iter <- length(N) * length(prior_sd) * length(hypotheses) * iter
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
            foreach(i = 1:3,
                    .combine = rbind) %:%
            foreach(icount(iter),
                    .combine = rbind,
                    .packages = c("BGGM", "BDgraph")) %dopar% {
        
        sim <- sim_function(n = n, 
                            precision = k, 
                            hyp = hypotheses[i], 
                            prior_sd = s)
        # pmp <- sim$post_prob
        bf12 <- sim$BF_matrix["H1", "H2"]
        bf13 <- sim$BF_matrix["H1", "Hc"]
        
        rescaled_pmp12 <- bf12 / (bf12 + 1)
        rescaled_pmp13 <- bf13 / (bf13 + 1)
        
        pmp_list <- c(h12 = rescaled_pmp12, 
                      h13 = rescaled_pmp13,
                      n   = n,
                      const = i,
                      sd  = s)
        
        return(pmp_list)
        
      }
                             
stopCluster(cl)                    
# --------------- end  ------------------- |


write.csv(results, "C:/Users/jer421/Box/bridge/data/02-sim-constraints.csv")
t1 <- Sys.time() - t0
print(t1) # Time difference of 11.1079 hours
