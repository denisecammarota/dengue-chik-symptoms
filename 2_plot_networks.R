library(igraphdata)

## chikungunya #################

gchik <- graph_from_adjacency_matrix(red_chik, mode = 'undirected', weighted=TRUE)
karate <- gchik
V(karate)$label.cex <- diag_chik*2
#karate_groups <- cluster_optimal(karate)
coords <- layout_in_circle(karate)
#V(karate)$label <- sub("Actor ", "", V(karate)$name)
V(karate)$label.color <- 'black'
V(karate)$label.font <- 2
E(karate)$color <- adjustcolor("red", .5)

#V(karate)$shape <- "none"
plot(karate, layout = coords, edge.width = E(gchik)$weight*15,
     vertex.size = diag_chik*0)

## dengue ################

gchik <- graph_from_adjacency_matrix(red_dengue, mode = 'undirected', weighted=TRUE)
karate <- gchik
V(karate)$label.cex <- diag_dengue*2
#karate_groups <- cluster_optimal(karate)
coords <- layout_in_circle(karate)
#V(karate)$label <- sub("Actor ", "", V(karate)$name)
V(karate)$label.color <- 'black'
V(karate)$label.font <- 2
E(karate)$color <- adjustcolor("red", .5)

#V(karate)$shape <- "none"
plot(karate, layout = coords, edge.width = E(gchik)$weight*15,
     vertex.size = diag_dengue*0)
