library(tidyverse)
dd <- read.csv("data/day01", sep = " ", header = F) |> select(V1, V4)
sum(abs(sort(dd$V1) - sort(dd$V4)))
sum(sapply(dd$V1, function(x) {
  x * sum(x == dd$V4)
}))