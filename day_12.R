library(tidyverse)
library(igraph)
source("source_at_start.R")

mm <- make_matrix("data/day12_test2", buffer_width = 1, buffer_char = ".")

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

corners
mm
left_join(perimeter, corners, by = c("val" = "loc")) |> 
  group_by(compoment, val) |>
  summarize(corner = sum(corner, na.rm = T)) |> 
  summarize(price = sum(corner, na.rm = T) * n()) |> 
  summarize(price = sum(price))

#891074 too high :-( 

#diagnostics
corners
mm
left_join(perimeter, corners, by = c("val" = "loc")) |> 
  group_by(compoment) |>
  summarize(corner = sum(corner, na.rm = T)) 


nn <- mm
nn[nn != "A"] <- "."
aa <- matrix(rep(" ", length(nn) + 2 * nrow(nn) + 2 * ncol(nn) + 4), nrow = nrow(nn) + 2)
for(i in 2:(nrow(aa) - 1)) {
  for(j in 2:(ncol(aa) - 1)) {
    aa[i, j] <- nn[i- 1, j - 1]
  }
}
aa

locs <- c(1, nrow(aa), -1, -nrow(aa))
locs2 <- c(nrow(aa), -1, -nrow(aa), 1)
dirs <- c("U", "R", "U", "R")
nn <- aa
edges <- purrr::map_df(1:length(nn), function(x) {
  if(nn[x] %in% c("A", " ")) {
    return(NULL)
  }
  purrr::map_df(1:4, function(zz) {
    if(nn[x + locs[zz]] == ".") {
      return(data.frame(start = x, end = x + locs[zz], dir = dirs[zz]))
    } else {
      return(NULL)
    }
  })
})

ee <- graph_from_data_frame(edges)
comps <- components(ee)
df <- data.frame(v = as.integer(attr(V(ee), which = "name")))
df$comp <- comps$membership

edges2 <- left_join(edges, df, by = c("start" = "v"))

sgs <- decompose(ee)

edges <- bind_rows(edges, 
          data.frame(start = edges$end,
                     end = edges$start,
                     dir = edges$dir))
nn
sum(sapply(1:max(df$comp), function(zz) {
  eetemp <- sgs[[zz]]
  all_circuits <- igraph::all_simple_paths(eetemp, from = V(eetemp)[1], to = neighbors(eetemp, V(eetemp)[1]) )
  no <- which.max(sapply(all_circuits, length))
  path <- data.frame(start = c(attr(all_circuits[[no]], which = "name")))
  path$end <- 1
  path$end[1:(nrow(path) - 1)] <- path$start[2:(nrow(path))]
  path$end[nrow(path)] <- path$start[1]
  path <- mutate(path, 
                 start = as.integer(start),
                 end = as.integer(end))
  temp <- left_join(path, edges)  
  sides <- temp |> 
    mutate(val = ifelse(dir == "U", 1, 0)) |> 
    pull(val) |> 
    diff() |> 
    abs() |> 
    sum()
  
  if(temp$dir[1] != temp$dir[nrow(temp)]) {
    sides <- sides + 1
  }
  return(sides)
}))

all_circuits <- igraph::all_simple_paths(ee, from = V(ee)[1], to = neighbors(ee, V(ee)[1]) )
all_circuits
no <- which.max(sapply(all_circuits, length))
path <- data.frame(start = c(attr(all_circuits[[no]], which = "name")))
path
path$end <- 1
path$end[1:(nrow(path) - 1)] <- path$start[2:(nrow(path))]
path$end[nrow(path)] <- path$start[1]
path
path <- mutate(path, 
               start = as.integer(start),
               end = as.integer(end))
path
temp <- left_join(path, edges)  
  
sides <- temp |> 
  mutate(val = ifelse(dir == "U", 1, 0)) |> 
  pull(val) |> 
  diff() |> 
  abs() |> 
  sum()
  
if(temp$dir[1] != temp$dir[nrow(temp)]) {
  sides <- sides + 1
}
sides

