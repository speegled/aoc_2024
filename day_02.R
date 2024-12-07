library(tidyverse)
dd <- readLines("data/day02")

x <- dd[2]
sum(sapply(dd, function(x) {
  temp <- str_split(x, pattern= " ", simplify = T) |> 
    as.integer() |> 
    diff()
  all(temp <= 3 & temp >= 1) || all(temp >= -3 & temp <= -1)
}))

sum(sapply(dd, function(x) {
  temp <- str_split(x, pattern= " ", simplify = T) |> 
    as.integer()
  any(sapply(1:length(temp), function(z) {
    temp <- temp[-z] |> 
      diff()
    all(temp <= 3 & temp >= 1) || all(temp >= -3 & temp <= -1)
  })) 
}))

