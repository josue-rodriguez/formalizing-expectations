library(BGGM)
library(BDgraph)
library(foreach)
library(doParallel)

set.seed(24)

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

# x <- confirm(bdgraph.sim(n = 100, K = precision)$data, 
#              hypothesis = hypotheses[1], 
#              prior_sd = 0.5)
# x$BF_computation

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
prior_sd <- c(0.25, 0.50)
iter <- 150

# data.frame to store results
results <- data.frame()

# set cores
cores <- detectCores()

# begin simulation
t0 <- Sys.time()
for (i in 1:3) {
  hyp <- hypotheses[i]
  
  for (s in prior_sd) {
    
    for (n in N){

      # message to keep track of simulation
      message("|n: ", n, " |condition: ", i, "|sd: ", s)
      # ------ begin core simulation ---------------- |
      sim_list <- mclapply(1:iter, mc.cores = 6,
                           FUN = function(x) {
                            message("iter: ", x)
                            
                            sim <- sim_function(n = n, 
                                                precision = k, 
                                                hyp = hyp, 
                                                prior_sd = s)
                            # pmp <- sim$post_prob
                            bf12 <- sim$BF_matrix["H1", "H2"]
                            bf13 <- sim$BF_matrix["H1", "Hc"]
                            
                            rescaled_pmp12 <- bf12 / (bf12 + 1)
                            rescaled_pmp13 <- bf13 / (bf13 + 1)
                            
                            pmp_list <- c(h12 = rescaled_pmp12, 
                                          h13 = rescaled_pmp13)
                            
                            return(pmp_list)
        })
      # --------------- end  ------------------- |
      # flatten lists and turn into data.frame
      tmp_list <- do.call(rbind, sim_list)
      tmp_df <- as.data.frame(tmp_list)
      # add in necessary variables
      tmp_df$n <- n
      tmp_df$const <- i + 2
      tmp_df$sd <- s
      # append results from current "n" into final data.frame
      results <- rbind.data.frame(results, tmp_df)
    }
  }
}


write.csv(results, "C:/Users/jer421/Box/bridge/data/02-simulation-results")
t1 <- Sys.time() - t0
print(t1)