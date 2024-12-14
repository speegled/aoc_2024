library(tidyverse)
library(igraph)

source("source_at_start.R")
mm <- make_matrix("data/day10", buffer_width = 1, buffer_char = "-1")

nrow <- nrow(mm)
ncol <- ncol(mm)
mm <- matrix(as.integer(mm), nrow = nrow)

vertices <- purrr::map_df(1:(nrow * ncol), function(i) {
  if(mm[i] == -1) {
    return(NULL)
  }    
  locs <- c(i + nrow, i - nrow, i + 1, i - 1)
  tails <- locs[which(mm[locs] == mm[i] + 1)]
  if(length(tails) == 0) {
    return(NULL)
  }
  data.frame(source = i, tail =  tails)
})


gg <- graph_from_data_frame(vertices)
sum(distances(gg, mode = "out") == 9) #first star

tot <- 0
V1 <- which(mm == 0)
V2 <- which(mm == 9)
V1 <- attr(V(gg), which = "name")[sapply(V1, function(x) which(x == attr(V(gg), which = "name")))]
V2 <- attr(V(gg), which = "name")[sapply(V2, function(x) which(x == attr(V(gg), which = "name")))]
tot <- 0
for(g1 in V1) {
  for(g2 in V2) {
      tot <- tot + length(all_shortest_paths(gg, from = g1, to = g2, mode = "out")$vpaths)
  }
  print(tot)
  }
}
tot #second star

