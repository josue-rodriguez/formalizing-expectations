library(BGGM)
library(MASS)
library(networktools)
library(network)
library(ggplot2)

set.seed(1)
col_names <- 
  c(
    "Intrusive Thoughts",
    "Nightmares",
    "Flashbacks",
    "Physiological/psychological reactivity",
    
    "Avoidance of thoughts",
    "Avoidance of situations",
    "Amnesia",
    "Disinterest in activities",
    "Feeling detached",
    "Emotional numbing",
    "Foreshortened future",
    
    "Sleep problems",
    "Irritability",
    "Concentration problems",
    "Hypervigilance",
    "Startle response"
  )

node_names <- c(
  paste0("B", 1:4), # re-experienceing
  paste0("C", 1:7), # avoidance
  paste0("D", 1:5) # arousal
)

comms <- c(
  rep("Re-Experiencing", 4),
  rep("Avoidance", 7),
  rep("Arousal", 5)
)

# color palette
pal <- ggthemes::few_pal()(3)
node_cols <- c(
  "Re-Experiencing" = pal[1],
  "Avoidance" = pal[2],
  "Arousal" = pal[3])

#--- exploratory ---|
ptsd1 <- mvrnorm(n = 965, mu = rep(0, 16), Sigma = ptsd_cor4, empirical = TRUE)
colnames(ptsd1) <- col_names

# explore and select graph
explore1 <- explore(ptsd1, chains = 4, cores = 4)
select1 <- BGGM::select(explore1, alternative = "greater", bf_cut = 2)

# print A^CI, A^CD, par cor amtrix
adj_ci <- select1$Adj_01
adj_cd <- select1$Adj_20
dimnames(adj_ci) <- dimnames(adj_cd) <- list(node_names, node_names)

round(select1$partials_positive, 3)

# keep only bridges edges
select1$Adj_20[1:4, 1:4] <- 0
select1$Adj_20[5:11, 5:11] <- 0
select1$Adj_20[12:16, 12:16] <- 0

# plot bridges
plot(select1,
     node_labels = node_names,
     node_inner_size = 15,
     layout = "circle",
     txt_size = 5,
     node_groups = comms,
     palette = node_cols) +
  theme(legend.position = "top",
        legend.title = element_blank())

#-------------------


#------------------

# bridge symptoms
bridge_stats <- bridge(adj_cd, communities = comms)
bridge_stats
plot(bridge_stats, 
     order = "value", 
     zscore = TRUE,
     include = c("Bridge Strength", "Bridge Betweenness", "Bridge Closeness"), 
     color = TRUE, 
     text_color = TRUE,
     colpalette = pal, 
     theme = theme_minimal()) 

# gplot.layout.adj(d, layout.par)
# gplot.layout.circle(d, layout.par)
# gplot.layout.circrand(d, layout.par)
# gplot.layout.eigen(d, layout.par)
# gplot.layout.fruchtermanreingold(d, layout.par)
# gplot.layout.geodist(d, layout.par)
# gplot.layout.hall(d, layout.par)
# gplot.layout.kamadakawai(d, layout.par)
# gplot.layout.mds(d, layout.par)
# gplot.layout.princoord(d, layout.par)
# gplot.layout.random(d, layout.par)
# gplot.layout.rmds(d, layout.par)
# gplot.layout.segeo(d, layout.par)
# gplot.layout.seham(d, layout.par)
# gplot.layout.spring(d, layout.par)
# gplot.layout.springrepulse(d, layout.par)
# gplot.layout.target(d, layout.par)



