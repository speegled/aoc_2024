library(tidyverse)

#broke this trying to make it faster :-( 

dd <- data.frame(x = readLines("data/day07"))
dd <- dd |> 
  separate_wider_delim(x, names = c("result", "problem"), delim = ": ")
dd <- dd |> 
  mutate(numspaces = str_count(problem, " ")) 

library(future.apply)
#library(future)
plan("multisession", workers = 12)
i <- 2
fff <- sapply(1:nrow(dd), function(i) {
  x <- dd$problem[i]
  ss <- dd$numspaces[i]
  rr <- as.numeric(dd$result[i])
  ops <- c("a", "b", "c") #use c("a", "b") for the first star; c("-", "+" , "") for second
  patterns <- combinat::hcube(x = rep(length(ops), ss))
  keep <- FALSE
  for(z in 1:nrow(patterns)) {
    pp <- as.integer(patterns[z,])
    for(ll in 1:length(pp)) {
      x <- sub(" ", ops[pp[ll]], x)
    }
    if(parse_string(x, target = rr) == rr) {
      keep <- TRUE
      break
    }
    if(z %% 1000 == 0) {
      print(z)
    }
  }
  if(keep) {
    return(rr)
  } else{
    return(0)
  }
})
#, future.globals = c("parse_string", "dd"), future.packages = c("stringr"))

fff
print(sum(fff), digits = 18)
parse_string <- function(str, target) {
  nn <- str_count(str, "[abc]")
  val <- as.numeric(str_extract(str, "[0-9]+"))
  str <- str_remove(str, "[0-9]+")
  i <- 1
  while(i <= nn && val <= target + 1) {
    op <- str_extract(str, "[abc]")
    str <- str_remove(str, "[abc]")
    val2 <- as.numeric(str_extract(str, "[0-9]+"))
    str <- str_remove(str, "[0-9]+")
    if(op == "a") {
      val <- val * val2
    }
    if(op == "b") {
      val <- val + val2
    }
    if(op == "c") {
      val <- as.numeric(paste0(val, val2))
    }
    i <- i + 1
  }
  return(val)
}



