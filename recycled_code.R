# pems_data <- data.frame(
#   y = as.vector(t(Y)),
#   edge_number = rep(PtE$edge_number, nrow(Y)),
#   distance_on_edge = rep(PtE$distance_on_edge, nrow(Y)),
#   repl = rep(1:nrow(Y), each = ncol(Y))
# )