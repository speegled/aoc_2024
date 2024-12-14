




all_graphs <- decompose(gg)
plot(all_graphs[[1]])
g1 <- all_graphs[[1]]
v1 <- as.integer(attr(V(g1), which = "name"))

get_names <- function(val, nrow) {
  n1 <- paste(c(val - nrow - 1, val - 1, val, val - nrow), collapse = " ")
  n2 <- paste(c(val - nrow, val, val + 1, val - nrow + 1), collapse = " ")
  n3 <- paste(c(val - 1, val - 1 + nrow, val + nrow, val), collapse = " ")
  n4 <- paste(c(val, val + nrow, val + nrow + 1, val + 1), collapse = " ")
  return(list(n1, n2, n3, n4))
}

val <- get_names(36, 10)[[1]]

get_poss_nbrs <- function(val, nrow) {
  val <- as.integer(str_split_fixed(val, " ", 4))
  n1 <- paste(val + 1, collapse = " ")
  n2 <- paste(val - 1, collapse = " ")
  n3 <- paste(val + nrow, collapse = " ")
  n4 <- paste(val - nrow, collapse = " ")
  return(list(n1, n2, n3, n4))
}

val2 <- get_poss_nbrs(val, nrow)[[1]]
val1 <- val
val2
val1
nn
is_in_graph <- function(val1, val2, nrow) {
  val1 <- as.integer(str_split_fixed(val1, " ", 4))
  val2 <- as.integer(str_split_fixed(val2, " ", 4))
  vals <- intersect(val1, val2)
  if(length(vals) != 2) {
    stop(paste("length vals not 2", val1, val2))
  }
  if(abs(diff(vals)) == 1) {
    dir <- 0
  } else{
    dir <- 1
  }
  is_edge <- nn[vals[1]] != nn[vals[2]]
  return(list(edge = is_edge,
              dir = dir))
}

is_in_graph(val1, val2, nrow)

i <- 1
#start here mm is matrix
g1 <- all_graphs[[i]]
vv <- as.integer(attr(V(g1), which = "nam"))

nn <- mm
nn[,] <- "."
nn[as.integer(vv)] <- "A"
nrow <- nrow(nn)
nrow
x <- 1
df <- distinct(purrr::map_df(1:length(vv), function(x) {
  nbs <- get_names(vv[x], nrow)
  purrr::map_df(1:4, function(zz) {
    data.frame(from = nbs[[zz]])
  })
}))

df2 <- purrr::map_df(1:nrow(df), function(x) {
  nbs <- get_poss_nbrs(df$from[x], nrow)
  purrr::map_df(1:length(nbs), function(zz) {
    yn <- is_in_graph(df$from[x], nbs[[zz]])
    if(yn$edge) {
      return(data.frame(from = df$from[x], to = nbs[[zz]], dir = yn$dir))
    }
  })
})

gg1 <- graph_from_data_frame(df2)
plot(gg1)
dd1 <- decompose(gg1)
nn
length(dd1)
dd1
#make loop
kk <- 2
sum(sapply(1:length(dd1), function(kk) {
  ggg1 <- dd1[[kk]]
  all_simple_paths(ggg1)
  n1 <- V(ggg1)[1]
  n2 <- neighbors(ggg1, n1)[1]
  all_circuits <- all_simple_paths(ggg1, from = n1, to = n2)
  no <- which.max(sapply(all_circuits, length))
  all_circuits[[2]]
  
  path <- data.frame(start = c(attr(all_circuits[[no]], which = "name")))
  path$end <- 1
  path$end[1:(nrow(path) - 1)] <- path$start[2:(nrow(path))]
  path$end[nrow(path)] <- path$start[1]
  path <- rename(path, from = start, to = end)
  path <- bind_rows(path, as_tibble(path)[1,])
  path
  sides <- left_join(path, df2) |> 
    pull(dir) |> 
    diff() |> 
    abs() |> 
    sum()
  
  sides
}))


nn





oo <- 2
sapply(1:length(all_graphs), function(oo) {
  g1 <- all_graphs[[oo]]
  coords <- as.integer(attr(V(g1), which = "name"))
  nn <- mm
  nn[,] <- "."
  nn[coords] <- "A"
  aa <- matrix(rep(" ", length(nn) + 2 * nrow(nn) + 2 * ncol(nn) + 4), nrow = nrow(nn) + 2)
  for(i in 2:(nrow(aa) - 1)) {
    for(j in 2:(ncol(aa) - 1)) {
      aa[i, j] <- nn[i- 1, j - 1]
    }
  }
  
  locs <- c(1, nrow(aa), -1, -nrow(aa))
  locs2 <- c(nrow(aa), -1, -nrow(aa), 1)
  dirs <- c("U", "R", "U", "R")
  nn <- aa
  edges
  edges <- purrr::map_df(1:length(nn), function(x) {
    if(nn[x] %in% c("A", " ")) {
      return(NULL)
    }
    if(all(nn[x + locs] %in% c(".", " "))) {
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
  edges
  nn
})
mm
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