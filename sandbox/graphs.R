library(igraph)
g <- make_empty_graph() %>%
  add_vertices(3, color = "red") %>%
  add_vertices(2, color = "green") %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5))
g
V(g)[[]]
plot(g)

edgesMom <- numeric(10)
edgesMom[2*(1:5)- 1] <- 6
edgesMom[2*(1:5)] <- 11:15
edgesDad <- numeric(10)
edgesDad[2*(1:5)- 1] <- 9
edgesDad[2*(1:5)] <- 11:15
g <- make_empty_graph() +
  vertices(1:10) +
  vertices(11:15) +
  edges(c(edgesMom, edgesDad)) +
  vertices(16:18) +
  edges(c(11, 16, 11, 17, 11, 18, 14, 16, 14, 17, 14, 18))
plot(g)
distances(g, v = 16, to = c(1:15, 17:18))
ego(g, order = 2, nodes = 16, mindist = 1)
