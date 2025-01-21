library(MetricGraph)
library(ggplot2)


gets_f_in_dist_mesh <- function(graph, f_values){
  VtE <- graph$mesh$VtE
  E <- graph$E
  nE <- graph$nE
  nV <- graph$nV
  E_ext <- data.frame(edge_number = 1:nE, vertex_start = E[,1], vertex_end = E[,2])
  
  original_vertex <- VtE[1:nV,]
  f <- f_values[1:nV]
  
  no_vertices <- cbind(VtE[(nV+1):length(f_values),], f_values[(nV+1):length(f_values)])
  
  same_vertex_list <- list()
  for (i in 1:nE) {
    same_vertex_list[[i]] <- E_ext %>% 
      filter(vertex_start == i | vertex_end == i) %>%
      mutate(lor = case_when(vertex_start == i ~ 0,vertex_end == i ~ 1)) %>% 
      dplyr::select(edge_number, lor) %>% 
      as.matrix()
  }
  
  for (i in seq_len(nrow(original_vertex))) {
    current_row <- original_vertex[i, ]
    # Find which matrix in same_vertex_list contains this row
    for (j in seq_along(same_vertex_list)) {
      if (any(apply(same_vertex_list[[j]], 1, function(x) all(x == current_row)))) {
        # Add a new column with the corresponding value from f
        same_vertex_list[[j]] <- cbind(same_vertex_list[[j]], f[i])
        break
      }
    }
  }
  
  result_matrix <- do.call(rbind, same_vertex_list)
  return(rbind(no_vertices, result_matrix))
}


edge1 <- rbind(c(0,0),c(1,0))
edge2 <- rbind(c(0,0),c(0,1))
edge3 <- rbind(c(0,1),c(-1,1))
theta <- seq(from = pi,to = 3*pi/2,length.out = 50)
edge4 <- cbind(sin(theta),1+ cos(theta))
edges <- list(edge1, edge2, edge3, edge4)

graph <- metric_graph$new(edges = edges)
aux_graph <- graph$clone()
graph$build_mesh(h = 0.01, continuous = TRUE)
edge_number <- graph$mesh$VtE
XY_graph <- graph$mesh$V

aux_graph$build_mesh(h = 0.01, continuous = FALSE)
aux_edge_number <- aux_graph$mesh$VtE
aux_XY_graph <- aux_graph$mesh$V

f <- edge_number[,1]/4# discontinuous covariate
g <- 0.5*(XY_graph[, 1]^2 - XY_graph[, 2]^2) + 0.5 # continuous covariate

aux_f <- aux_edge_number[,1]/4# discontinuous covariate
aux_g <- 0.5*(aux_XY_graph[, 1]^2 - aux_XY_graph[, 2]^2) + 0.5 # continuous covariate


aux_graph$plot_function(aux_g, vertex_size = 0, plotly = TRUE, line_color = "blue", line_width = 3) %>%  
  config(mathjax = 'cdn') %>% 
  layout(title = "Non-continuous covariate")

gg <- gets_f_in_dist_mesh(graph, g)

data <- data.frame(edge_number = gg[,1], distance_on_edge = gg[,2], f = gg[,3])
df_aux <- aux_graph$process_data(data = data, normalized = TRUE) # this is independent of the mesh, whether it is continuous or not
df <- graph$process_data(data = data, normalized = TRUE)

aux_graph$plot_function(X = aux_g, vertex_size = 0, plotly = TRUE, line_color = "blue", line_width = 3, continuous = FALSE)
graph$plot_function(X = g, vertex_size = 0, plotly = TRUE, line_color = "blue", line_width = 3, continuous = FALSE)

# They do the same thing
aux_graph$plot_function(data = "f", newdata= df, vertex_size = 0, plotly = TRUE, line_color = "blue", line_width = 3, continuous = FALSE)
graph$plot_function(data = "f", newdata= df, vertex_size = 0, plotly = TRUE, line_color = "blue", line_width = 3, continuous = FALSE)


