packages <- c("networktools", "dplyr", "network")
if (!all(packages %in% installed.packages())) install.packages(packages)

library(ggnetwork)
library(dplyr)
library(network)

make_adj_plot <- 
  function(adj, node_labels, communities, node_size = 10,
           node_label_col = "black",
           bridges = NULL, fade = NULL, parcors = NULL,
           layout = "circle", type = c("cd", "ci"))
{
  # ---------- focus on bridges of interest
  if (type[1] == "cd") {
    
    adj_bridge <- adj
    dimnames(adj_bridge) <- list(communities, communities) 
    for (comm in communities) {
      mask <- grepl(comm, colnames(adj_bridge))
      adj_bridge[mask, mask] <- 0
    }
    dimnames(adj_bridge) <- list(node_labels, node_labels)
    
    adj_bridge <- network::as.network(adj_bridge)
    
    adj_bridge %v% "n_labs" <- node_labels
    adj_bridge %v% "comm" <- communities
    adj_bridge_net <-
      ggnetwork(adj_bridge, layout = layout) %>%
      filter(n_labs %in% bridges)

    #---------------- main network
    adj <- network::as.network(adj)
    adj %v% "n_labs" <- node_labels
    adj %v% "comm" <- communities
    if (!is.null(parcors)) set.edge.value(adj, attrname = "weights", value = round(parcors, 2))
    adj_net <- ggnetwork(adj, layout = layout) %>%
      filter(!n_labs %in% c(bridge))
    # ------------ Plot
    # non faded
    ggplot(data = filter(adj_net, !n_labs %in% c(bridges, fade)),
           aes(x = x, y = y, xend = xend, yend = yend)) +
      # bridges
      geom_edges(data = adj_bridge_net,
                 aes(linetype = n_labs),
                 alpha = 1,
                 size = 1,
                 show.legend = FALSE) +
      geom_nodes(data = adj_bridge_net,
                 size = node_size + 1) +
      geom_nodes(data = adj_bridge_net,
                 aes(fill = comm),
                 size = node_size,
                 shape = 21) +
      geom_nodetext(data = adj_bridge_net,
                    aes(label = n_labs),
                    col = node_label_col) +
      # main network
      geom_edges(col = "gray70",
                 alpha = 0.3) +
      geom_nodes(size = node_size + 1) +
      geom_nodes(aes(fill = comm),
                 size = node_size,
                 shape = 21) +
      geom_nodetext(aes(label = n_labs), col = node_label_col) +
      # # add in fade
      geom_edges(data = filter(adj_net, n_labs %in% fade),
                 col = "gray70",
                 alpha = 0.3,
                 show.legend = FALSE) +
      geom_nodes(data = filter(adj_net, n_labs %in% fade),
                 size = node_size + 1,
                 shape = 21,
                 fill = NA) +
      geom_nodes(data = filter(adj_net, n_labs %in% fade),
                 aes(fill = comm),
                 size = node_size,
                 shape = 21,
                 alpha = 0.6) +
      geom_nodetext(data = filter(adj_net, n_labs %in% fade),
                    aes(label = n_labs),
                    alpha = 0.6, 
                    col = node_label_col) 
    
    
  } else {
    
    if (!is.null(bridges)) {
      adj_bridge <- adj
      # dimnames(adj_bridge) <- list(communities, communities) 
      # for (comm in communities) {
      #   mask <- grepl(comm, colnames(adj_bridge))
      #   adj_bridge[mask, mask] <- 0
      # }
      dimnames(adj_bridge) <- list(node_labels, node_labels)
      
      adj_bridge <- network::as.network(adj_bridge)
      
      adj_bridge %v% "n_labs" <- node_labels
      adj_bridge %v% "comm" <- communities
      adj_bridge_net <-
        ggnetwork(adj_bridge, layout = layout) %>%
        filter(n_labs %in% bridges)
    }
    
    #---------------- main network
    adj <- network::as.network(adj)
    adj %v% "n_labs" <- node_labels
    adj %v% "comm" <- communities
    if (!is.null(parcors)) set.edge.value(adj, attrname = "weights", value = round(parcors, 2))
    adj_net <- ggnetwork(adj, layout = layout) 
    if (!is.null(bridges)) adj_net <- adj_net %>% filter(!n_labs %in% c(bridge))
    # ------------ Plot
    # non faded
    ggplot(data = filter(adj_net, !n_labs %in% fade),
           aes(x = x, y = y, xend = xend, yend = yend)) +
      # bridges
      # geom_edges(data = adj_bridge_net,
      #            aes(linetype = n_labs),
      #            alpha = 1,
      #            size = 1,
      #            show.legend = FALSE) +
      # geom_nodes(data = adj_bridge_net,
      #            aes(fill = comm),
      #            size = 20,
      #            shape = 21) +
      # geom_nodetext(data = adj_bridge_net,
      #               aes(label = n_labs)) +
      # main network
      geom_edges(col = "gray70",
                 alpha = 0.5,
                 linetype = "dotted") +
      geom_nodes(size = node_size + 1) +
      geom_nodes(aes(fill = comm),
                 size = node_size,
                 shape = 21) +
      geom_nodetext(aes(label = n_labs),
                    col = node_label_col) +
      # # add in fade
      geom_edges(data = filter(adj_net, n_labs %in% fade),
                 col = "gray70",
                 alpha = 0.5,
                 linetype = "dotted",
                 show.legend = FALSE) +
      geom_nodes(data = filter(adj_net, n_labs %in% fade),
                 size = node_size+1,
                 shape = 21,
                 fill = NA) +
      geom_nodes(data = filter(adj_net, n_labs %in% fade),
                 aes(fill = comm),
                 size = node_size,
                 shape = 21,
                 alpha = 0.6) +
      geom_nodetext(data = filter(adj_net, n_labs %in% fade),
                    aes(label = n_labs),
                    alpha = 0.6,
                    col = node_label_col) 
  }
  
  
  
  }


# make_adj_plot(slct$Adj_20,
#               node_labels = node_names,
#               communities = comms,
#               bridges = c("D3", "D5"),
#               fade = c("D1", "D2", "D4", "C3", "C1"),
#               type = "cd")  +
#   ggthemes::scale_fill_few(name = expression(bold("A")^italic("CD"))) +
#   guides(fill = guide_legend(override.aes = list(size=10))) +
#   theme(legend.position = "top",
#         legend.box.background = element_blank(),
#         legend.background = element_blank(),
#         legend.title = element_text(size = 20,
#                                     margin = margin(t=0,r=50,b=0,l=0)))

