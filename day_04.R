library(tidyverse)
dd <- readLines("data/day04")
str_length(dd[1])
mm <- matrix(".", nrow = 140 + 2 * 3, ncol = 140 + 2 * 3)
dd <- dd |> 
  str_split(pattern = "")

for(i in 4:143) {
  for(j in 4:143) {
    mm[i,j] <- dd[[i - 3]][j - 3] 
  }
}  
mm

check_xmas <- function(i, j) {
  tot <- 0
  if(mm[i,j] != "X") {
    return(0)
  }
  if(mm[i + 1,j] == "M" && mm[i + 2,j] == "A" && mm[i + 3, j] == "S") {
    tot <- tot + 1
  }
  if(mm[i - 1,j] == "M" && mm[i - 2,j] == "A" && mm[i - 3, j] == "S") {
    tot <- tot + 1
  }
  if(mm[i,j + 1] == "M" && mm[i,j + 2] == "A" && mm[i, j + 3] == "S") {
    tot <- tot + 1
  }
  if(mm[i,j - 1] == "M" && mm[i,j - 2] == "A" && mm[i, j - 3] == "S") {
    tot <- tot + 1
  }
  if(mm[i + 1,j + 1] == "M" && mm[i + 2,j + 2] == "A" && mm[i + 3, j + 3] == "S") {
    tot <- tot + 1
  }
  if(mm[i + 1,j - 1] == "M" && mm[i + 2,j - 2] == "A" && mm[i + 3, j - 3] == "S") {
    tot <- tot + 1
  }
  if(mm[i - 1,j + 1] == "M" && mm[i - 2,j + 2] == "A" && mm[i - 3, j + 3] == "S") {
    tot <- tot + 1
  }
  if(mm[i - 1,j - 1] == "M" && mm[i - 2,j - 2] == "A" && mm[i - 3, j- 3] == "S") {
    tot <- tot + 1
  }
  return(tot)
}

gt <- 0
for(i in 1:nrow(mm)) {
  for(j in 1:ncol(mm)) {
    gt <- gt + check_xmas(i, j)
  }
}
gt
i <- 10
j <- 10
check_xmas2 <- function(i, j) {
  if(mm[i, j] != "A") {
    return(0)
  }
  str1 <- paste0(mm[i - 1, j - 1], mm[i + 1, j + 1], collapse = "")
  str2 <- paste0(mm[i - 1, j + 1], mm[i + 1, j - 1], collapse = "")
  if( (str_equal(str1, "MS") || str_equal(str1, "SM")) && (str_equal(str2, "MS") || str_equal(str2, "SM"))    ) {
    return(1)
  }
  return(0)
}
gt <- 0
for(i in 1:nrow(mm)) {
  for(j in 1:ncol(mm)) {
    gt <- gt + check_xmas2(i, j)
  }
}
gt

