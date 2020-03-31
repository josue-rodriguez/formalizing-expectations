plot.bridge <- 
function (x, order = c("given", "alphabetical", "value"), 
          zscore = FALSE, include, color = FALSE, colpalette = "Dark2", 
          plotNA = FALSE, lab_size = 8,
          text_color = TRUE, node_size = 10,
          ...)

{
  attr(x, "class") <- NULL
  if (!plotNA) {
    if (TRUE %in% is.na(x[[1]])) {
      message("Missing values have been removed from the plot, set plotNA = TRUE to show missing values")
      x <- lapply(x, function(x) {
        x[!is.na(x)]
      })
      if (length(x[[1]]) == 0) {
        warning("No values remain after removing missing values")
      }
    }
  }
  nodes <- names(x[[1]])
  comm <- x$communities
  commcol <- vector()
  if (color) {
    pal <- colpalette
    for (i in 1:length(unique(comm))) {
      commcol[i] <- pal[i]
    }
    cols <- commcol[match(comm, unique(comm))]
  }
  else {
    cols <- rep("black", length(comm))
  }
  x$communities <- NULL
  if (zscore) {
    scalenoatt <- function(y) {
      y <- scale(y)
      attr(y, "scaled:center") <- NULL
      attr(y, "scaled:scale") <- NULL
      return(y)
    }
    x <- sapply(x, scalenoatt)
  }
  Long <- reshape2::melt(x)
  colnames(Long)[2] <- "measure"
  Long$type <- rep(NA, nrow(Long))
  Long$node <- rep(nodes, length(unique(Long$measure)))
  if (missing(include)) {
    include <- unique(Long$measure[Long$measure != "communities"])
  }
  Long <- subset(Long, Long$measure %in% include)
  if (order[1] == "given") {
    Long$node <- factor(as.character(Long$node), levels = rev(unique(as.character(Long$node))))
    g <- ggplot2::ggplot(Long, ggplot2::aes_string(x = "value", 
                                                   y = "node", group = "type", ...))
    g <- g + ggplot2::geom_path() + ggplot2::xlab("") + 
      ggplot2::ylab("") + ggplot2::geom_point(aes_string(group = "type")) +
      ggthemes::scale_fill_few() 
    g <- g + ggplot2::facet_grid("~measure", scales = "free") + 
      ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
  }
  else if (order[1] == "alphabetical") {
    Long <- Long[with(Long, order(Long$node)), ]
    Long$node <- factor(as.character(Long$node), levels = unique(as.character(Long$node)[order(Long$node)]))
    g <- ggplot2::ggplot(Long, ggplot2::aes_string(x = "value", 
                                                   y = "node", group = "type", ...))
    g <- g + ggplot2::geom_path() + ggplot2::geom_point() + 
      ggplot2::xlab("") + ggplot2::ylab("") + 
      ggplot2::facet_grid("~measure", scales = "free") + 
      ggplot2::scale_y_discrete(limits = rev(levels(Long$node))) + 
      ggplot2::theme(axis.text.y = ggplot2::element_text(colour = cols[order(nodes, 
                                                                             decreasing = T)])) +
      theme
  }
  #---------------------------------------------------- |
  else if (order[1] == "value") {
    glist <- list()
    for (i in 1:length(include)) {
      temp_Long_orig <- Long[Long$measure == include[i], 
                             ]
      temp_Long <- temp_Long_orig[with(temp_Long_orig, 
                                       order(temp_Long_orig$value)), ]
      temp_Long$node <- factor(as.character(temp_Long$node), 
                               levels = unique(as.character(temp_Long$node)[order(temp_Long$value)]))
      glist[[i]] <- ggplot2::ggplot(temp_Long, 
                                    ggplot2::aes_string(x = "value", 
                                                        y = "node", 
                                                        group = "type", 
                                                        ...)) + 
        ggplot2::geom_path() + 
        ggplot2::geom_point(fill = cols[order(temp_Long_orig$value)], shape = 21, size = node_size) +
        ggplot2::geom_text(aes_string(x = "value", y = "node", label = "node"), size = lab_size) +
        ggplot2::xlab("") +
        ggplot2::ylab("") + 
        ggplot2::facet_grid("~measure", scales = "free") 
        # theme
        if (text_color) {
          glist[[i]] <- 
            glist[[i]] + 
            ggplot2::theme(axis.text.y = ggplot2::element_text(colour = cols[order(temp_Long_orig$value)]))
        }
        
    }
    #---------------------------------------------------- |
    if (length(include) == 1) {
     return(glist[[1]])
    }
    else if (length(include) == 2) {
      gridExtra::grid.arrange(glist[[1]], glist[[2]], ncol = 2)
    }
    else if (length(include) == 3) {
      gridExtra::grid.arrange(glist[[1]], glist[[2]], glist[[3]], 
                              ncol = 3)
    }
    else if (length(include) == 4) {
      gridExtra::grid.arrange(glist[[1]], glist[[2]], glist[[3]], 
                              glist[[4]], ncol = 4)
    }
    else if (length(include) == 5) {
      gridExtra::grid.arrange(glist[[1]], glist[[2]], glist[[3]], 
                              glist[[4]], glist[[5]], ncol = 5) + ggplot2::theme_bw()
    }
  }
  if (order[1] != "value") {
    return(g)
  }
}




#=========================================
# tweak plot.select.explore
#=========================================
# 
# 
# plot.select.explore <- 
# function (x, layout = "circle", edge_colors = "classic", 
#           node_labels = NULL, node_labels_color = "black", node_groups = NULL, 
#           node_outer_size = 12, node_inner_size = 11, alpha = 0.5, 
#           txt_size = 8, edge_multiplier = 1, ...) 
# {
#   label <- NULL
#   color <- NULL
#   if (x$alternative != "exhaustive") {
#     # names(x)[1] <- "partials_non_zero"
#     x$partials_non_zero <- x$partials_positive
#     p <- ncol(x$partials_non_zero)
#     if (x$alternative == "greater" | x$alternative == 
#         "less") {
#       net <- network::network(x$Adj_20)
#     }
#     else {
#       net <- network::network(x$Adj_10)
#     }
#     if (is.null(node_labels)) {
#       network::network.vertex.names(net) <- 1:p
#     }
#     else {
#       if (length(node_labels) != p) {
#         stop("labels must be of length p (number of nodes)")
#       }
#       network::network.vertex.names(net) <- node_labels
#     }
#     network::set.edge.value(x = net, attrname = "weights", 
#                             value = x$partials_non_zero)
# 
#     network::set.edge.value(x = net, attrname = "abs_weights", 
#                             value = abs(x$partials_non_zero) * edge_multiplier)
#     
#     
#     if (edge_colors == "classic") {
#       network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                   value = ifelse(net %e% "weights" < 0, "brown3", 
#                                                  "palegreen3"))
#     }
#     else if (edge_colors == "color_blind") {
#       network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                   value = ifelse(net %e% "weights" < 0, "#009E73", 
#                                                  "#D55E00"))
#     }
#     else if (edge_colors == "vivid") {
#       network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                   value = ifelse(net %e% "weights" < 0, "darkorange1", 
#                                                  "darkorchid4"))
#     }
#     if (is.null(node_groups)) {
#       plt <- ggnet2(net = net, edge.alpha = alpha, mode = layout, 
#                     node.size = node_outer_size, node.color = "black", 
#                     edge.color = "edge_color", edge.size = "abs_weights", 
#                     label = TRUE) + geom_point(color = "white", 
#                                                size = node_inner_size, alpha = 1) + geom_text(aes(label = label), 
#                                                                                               color = node_labels_color, size = txt_size)
#       plt <- list(plt = plt)
#     }
#     else {
#       if (length(node_groups) != p) {
#         stop("labels must be of length p (number of nodes)")
#       }
#       net %v% "group" <- node_groups
#       plt <- ggnet::ggnet2(net = net, edge.alpha = alpha, mode = layout, 
#                     node.size = node_outer_size, node.color = "group", 
#                     node.alpha = 0.5, edge.color = "edge_color", 
#                     edge.size = "abs_weights", label = TRUE, 
#                     ...) + geom_point(aes(color = color), size = node_inner_size, 
#                                       alpha = 1) + geom_text(aes(label = label), color = node_labels_color, 
#                                                              size = txt_size)
#       plt <- list(plt = plt)
#     }
#     if (is.null(x$rope)) {
#       net <- network::network(x$Adj_01, directed = FALSE)
#       if (is.null(node_labels)) {
#         network::network.vertex.names(net) <- 1:p
#       }
#       else {
#         if (length(node_labels) != p) {
#           stop("labels must be of length p (number of nodes)")
#         }
#         network::network.vertex.names(net) <- node_labels
#       }
#       if (is.null(node_groups)) {
#         plt_null <- ggnet2(net = net, edge.alpha = alpha, 
#                            mode = layout, node.size = node_outer_size, 
#                            node.color = "black", label = TRUE) + 
#           geom_point(color = "white", size = node_inner_size, 
#                      alpha = 1) + geom_text(aes(label = label), 
#                                             color = node_labels_color, size = txt_size)
#         plt$plt_null <- plt_null
#       }
#       else {
#         if (length(node_groups) != p) {
#           stop("labels must be of length p (number of nodes)")
#         }
#         net %v% "group" <- node_groups
#         plt_null <- ggnet::ggnet2(net = net, edge.alpha = alpha, 
#                            mode = layout, node.alpha = 0.5, node.size = node_outer_size, 
#                            node.color = "group", label = TRUE, ...) + 
#           geom_point(aes(color = color), size = node_inner_size, 
#                      alpha = 1) + geom_text(aes(label = label), 
#                                             color = node_labels_color, size = txt_size)
#         plt$plt_null <- plt_null
#       }
#     }
#   }
#   else {
#     p <- ncol(x$neg_mat)
#     if (is.null(node_groups)) {
#       w_pos <- x$pos_mat * x$pcor_mat
#       net <- network::network(x$pos_mat, directed = FALSE)
#       if (is.null(node_labels)) {
#         network::network.vertex.names(net) <- 1:p
#       }
#       else {
#         if (length(node_labels) != p) {
#           stop("labels must be of length p (number of nodes)")
#         }
#         network::network.vertex.names(net) <- node_labels
#       }
#       network::set.edge.value(x = net, attrname = "weights", 
#                               value = w_pos)
#       network::set.edge.value(x = net, attrname = "abs_weights", 
#                               value = abs(w_pos) * edge_multiplier)
#       if (edge_colors == "classic") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "brown3", "palegreen3"))
#       }
#       else if (edge_colors == "color_blind") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "#009E73", "#D55E00"))
#       }
#       else if (edge_colors == "vivid") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "darkorange1", "darkorchid4"))
#       }
#       plt_pos <- ggnet::ggnet2(net = net, edge.alpha = alpha, 
#                         mode = layout, node.size = node_outer_size, node.color = "black", 
#                         edge.color = "edge_color", edge.size = "abs_weights", 
#                         label = TRUE) + geom_point(color = "white", 
#                                                    size = node_inner_size, alpha = 1) + geom_text(aes(label = label), 
#                                                                                                   color = node_labels_color, size = txt_size)
#       w_neg <- x$neg_mat * x$pcor_mat
#       net <- network::network(x$neg_mat, directed = FALSE)
#       if (is.null(node_labels)) {
#         network::network.vertex.names(net) <- 1:p
#       }
#       else {
#         if (length(node_labels) != p) {
#           stop("labels must be of length p (number of nodes)")
#         }
#         network::network.vertex.names(net) <- node_labels
#       }
#       network::set.edge.value(x = net, attrname = "weights", 
#                               value = w_neg)
#       network::set.edge.value(x = net, attrname = "abs_weights", 
#                               value = abs(w_neg) * edge_multiplier)
#       if (edge_colors == "classic") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "brown3", "palegreen3"))
#       }
#       else if (edge_colors == "color_blind") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "#009E73", "#D55E00"))
#       }
#       else if (edge_colors == "vivid") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "darkorange1", "darkorchid4"))
#       }
#       plt_neg <- ggnet2(net = net, edge.alpha = alpha, 
#                         mode = layout, node.size = node_outer_size, node.color = "black", 
#                         edge.color = "edge_color", edge.size = "abs_weights", 
#                         label = TRUE) + geom_point(color = "white", 
#                                                    size = node_inner_size, alpha = 1) + geom_text(aes(label = label), 
#                                                                                                   color = node_labels_color, size = txt_size)
#       net <- network::network(x$null_mat, directed = FALSE)
#       if (is.null(node_labels)) {
#         network::network.vertex.names(net) <- 1:p
#       }
#       else {
#         if (length(node_labels) != p) {
#           stop("labels must be of length p (number of nodes)")
#         }
#         network::network.vertex.names(net) <- node_labels
#       }
#       plt_null <- ggnet::ggnet2(net = net, edge.alpha = alpha, 
#                          mode = layout, node.size = node_outer_size, node.color = "white", 
#                          label = TRUE) + geom_point(color = "white", 
#                                                     size = node_inner_size, alpha = 1) + geom_text(aes(label = label), 
#                                                                                                    color = node_labels_color, size = txt_size)
#       plt <- list(plt_pos = plt_pos, plt_neg = plt_neg, 
#                   plt_null = plt_null)
#     }
#     else if (!is.null(node_groups)) {
#       if (length(node_groups) != p) {
#         stop("labels must be of length p (number of nodes)")
#       }
#       w_pos <- x$pos_mat * x$pcor_mat
#       net <- network::network(x$pos_mat, directed = FALSE)
#       net %v% "group" <- node_groups
#       network::set.edge.value(x = net, attrname = "weights", 
#                               value = w_pos)
#       network::set.edge.value(x = net, attrname = "abs_weights", 
#                               value = abs(w_pos) * edge_multiplier)
#       if (edge_colors == "classic") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "brown3", "palegreen3"))
#       }
#       else if (edge_colors == "color_blind") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "#009E73", "#D55E00"))
#       }
#       else if (edge_colors == "vivid") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "darkorange1", "darkorchid4"))
#       }
#       plt_pos <- ggnet::ggnet2(net = net, edge.alpha = alpha, 
#                         mode = layout, node.size = node_outer_size, node.color = "group", 
#                         edge.color = "edge_color", edge.size = "abs_weights", 
#                         label = TRUE, ...) + geom_point(aes(color = color), 
#                                                         size = node_outer_size, alpha = 0.5) + geom_point(aes(color = color), 
#                                                                                                           size = node_inner_size, alpha = 1) + geom_text(aes(label = label), 
#                                                                                                                                                          color = node_labels_color, size = txt_size)
#       w_neg <- x$neg_mat * x$pcor_mat
#       net <- network::network(x$neg_mat, directed = FALSE)
#       net %v% "group" <- node_groups
#       network::set.edge.value(x = net, attrname = "weights", 
#                               value = w_neg)
#       network::set.edge.value(x = net, attrname = "abs_weights", 
#                               value = abs(w_neg) * edge_multiplier)
#       if (edge_colors == "classic") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "brown3", "palegreen3"))
#       }
#       else if (edge_colors == "color_blind") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "#009E73", "#D55E00"))
#       }
#       else if (edge_colors == "vivid") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "darkorange1", "darkorchid4"))
#       }
#       plt_neg <- ggnet::ggnet2(net = net, edge.alpha = alpha, 
#                         mode = layout, node.size = node_outer_size, node.color = "group", 
#                         edge.color = "edge_color", edge.size = "abs_weights", 
#                         label = TRUE, ...) + geom_point(aes(color = color), 
#                                                         size = node_outer_size, alpha = 0.5) + geom_point(aes(color = color), 
#                                                                                                           size = node_inner_size, alpha = 1) + geom_text(aes(label = label), 
#                                                                                                                                                          color = node_labels_color, size = txt_size)
#       net <- network::network(x$null_mat, directed = FALSE)
#       net %v% "group" <- node_groups
#       if (edge_colors == "classic") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "brown3", "palegreen3"))
#       }
#       else if (edge_colors == "color_blind") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "#009E73", "#D55E00"))
#       }
#       else if (edge_colors == "vivid") {
#         network::set.edge.attribute(x = net, attrname = "edge_color", 
#                                     value = ifelse(net %e% "weights" < 0, 
#                                                    "darkorange1", "darkorchid4"))
#       }
#       plt_null <- ggnet::ggnet2(net = net, edge.alpha = alpha, 
#                          mode = layout, node.size = node_outer_size, node.color = "group", 
#                          label = TRUE, ...) + geom_point(aes(color = color), 
#                                                          size = node_outer_size, alpha = 0.5) + geom_point(aes(color = color), 
#                                                                                                            size = node_inner_size, alpha = 1) + geom_text(aes(label = label), 
#                                                                                                                                                           color = node_labels_color, size = txt_size)
#       plt <- list(plt_pos = plt_pos, plt_neg = plt_neg, 
#                   plt_null = plt_null)
#     }
#   }
#   return(plt)
# }