library(foreach)
library(doParallel)
library(doSNOW)

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

# tst <- MASS::mvrnorm(n = 10000, mu = rep(0, ncol(S)), Sigma = S)
# 
# 
# Theta <- solve(cov(tst))
# pcors <- -cov2cor(Theta)
# diag(pcors) <- 1
# pcors
# psych::partial.r(tst)
# # precision matrix
# lower_tri_k <- -seq(0.01, 0.15, length.out = 6)
# 
# k <- diag(4)
# k[lower.tri(k)] <- lower_tri_k
# k[upper.tri(k)] <- t(k)[upper.tri(k)]

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

# library(BDgraph)
library(BGGM)

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

# grab prior probabiltie s,
# tmp <- sim_function(10000, S, hypotheses[1], 0.25) # ~0.04
# sim_function(100, k, hypotheses[2], 0.25)$BF_computation # ~ .008
# sim_function(100, k, hypotheses[3], 0.25)$BF_computation # ~ .001

# parameters to iterate through
# N <- seq(100, 1500, by = 1000)
# prior_sd <- c(0.25, 0.50)
# iter <- 3
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
                    .packages = c("BGGM")) %dopar% {
        
        sim <- sim_function(n = n, 
                            S = S, 
                            hyp = hypotheses[i], 
                            prior_sd = s)
        pmp1 <- sim$out_hyp_prob[[1]]
        pmp2 <- sim$out_hyp_prob[[2]]
        pmp3 <- sim$out_hyp_prob[[3]]
        
        pmp_list <- c(pmp1 = pmp1,
                      pmp2 = pmp2,
                      pmp3 = pmp3,
                      n   = n,
                      const = i,
                      sd  = s)
        
        # bf12 <- sim$BF_matrix["H1", "H2"]
        # bf13 <- sim$BF_matrix["H1", "Hc"]
        # 
        # rescaled_pmp12 <- bf12 / (bf12 + 1)
        # rescaled_pmp13 <- bf13 / (bf13 + 1)
        # 
        # pmp_list <- c(h12 = rescaled_pmp12, 
        #               h13 = rescaled_pmp13,
        #               n   = n,
        #               const = i,
        #               sd  = s)
        
        return(pmp_list)
        
      }
                             
stopCluster(cl)                    
# --------------- end  ------------------- |


write.csv(results, "C:/Users/jer421/Box/bridge/data/02-sim-constraints2.csv")
t1 <- Sys.time() - t0
print(t1) # Time difference of 11.1079 hours
          # second time difference of 59.70749 mins
