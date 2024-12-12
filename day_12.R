library(tidyverse)
library(igraph)
source("source_at_start.R")

mm <- make_matrix("data/day12", buffer_width = 1, buffer_char = ".")

nrow <- nrow(mm)
ncol <- ncol(mm)
graph <- purrr::map_df(1:length(mm), function(x) {
  if(mm[x] == ".") {
    return(NULL)
  }
  test <- mm[x]
  purrr::map_df(c(nrow, -nrow, 1, -1, 0), function(z) {
    if(mm[z + x] == test) {
      return(data.frame(start = x, finish = x + z, name = mm[x]))
    }
    return(NULL)
  })
})

gg <- graph_from_data_frame(graph, directed = F)

cc <- igraph::components(gg)
perimeter <- purrr::map_df(1:length(mm), function(x) {
  if(mm[x] == ".") {
    return(NULL)
  }
  data.frame(tot = sum(mm[x] != mm[x + c(nrow, -nrow, 1, -1)]), val = x)
})
sum(cc$csize)
perimeter$compoment <- cc$membership
perimeter |> 
  group_by(compoment) |> 
  summarize(price = sum(tot) * n()) |> 
  ungroup() |> 
  summarize(price = sum(price)) #first star


corners <- purrr::map_df(1:length(mm), function(x) {
  if(mm[x] == ".") {
    return(NULL)
  }
  locs <- c(1, nrow, -1, -nrow)
  locs2 <- c(nrow, -1, -nrow, 1)
  locs3 <- c(nrow - 1, -nrow - 1, nrow + 1, -nrow + 1)
  locs4 <- c(nrow, -1, 1, -nrow)
  bind_rows(
    purrr::map_df(1:4, function(zz) {
      if(mm[x + locs[zz]] != mm[x] && mm[x + locs2[zz]] != mm[x]) {
        return(data.frame(loc = x, corner = 1, type = 1))
      } else {
        return(NULL)
      }
    }),
    purrr::map_df(1:4, function(zz) {
      if(mm[x] == mm[x + locs3[zz]] && mm[x] != mm[x + locs4[zz]]) {
        return(data.frame(loc = x, corner = 1, type = 2))
      } else {
        return(NULL)
      }
    })
  )
})

left_join(perimeter, corners, by = c("val" = "loc")) |> 
  group_by(compoment, val) |>
  summarize(corner = sum(corner, na.rm = T)) |> 
  summarize(price = sum(corner, na.rm = T) * n()) |> 
  summarize(price = sum(price))

#891074 too high :-( 








