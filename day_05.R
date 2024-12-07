library(tidyverse)
dd <- data.frame(x = readLines("data/day05"))
aa <- dd |> filter(str_detect(x, "\\|")) |> 
  separate_wider_delim(x, names = c("first", "second"), delim = "|")

dd <- dd |> 
  filter(str_detect(x, ","))

aa <- aa |> 
  rowwise() |> 
  mutate(str = paste(first, second, sep = "x"))
tot <- 0
for(i in 1:192 ) {
  ss <- dd[i,]
  ss <- as.vector(str_split(ss, pattern = ",", simplify = T))
  good <- TRUE
  for(j in 1:(length(ss) - 1)) {
    for(k in (j + 1):length(ss)) {
      tt <- paste(ss[j], ss[k], sep = "x")
      tt %in% aa$str
      if(!(tt %in% aa$str)) {
        good <- FALSE
      }
    }
  }
  if(good) {
    val <- as.integer(ss[(1 + length(ss)) / 2])
    tot <- tot + val
  }
}

is_good <- function(ss) {
  good <- TRUE
  for(j in 1:(length(ss) - 1)) {
    for(k in (j + 1):length(ss)) {
      tt <- paste(ss[j], ss[k], sep = "x")
      tt %in% aa$str
      if(!(tt %in% aa$str)) {
        good <- FALSE
      }
    }
  }
  return(good)
}

tot <- 0
i <- 1
for(i in 1:nrow(dd)) {
  ss <- dd$x[i]
  ss <- as.vector(str_split(ss, pattern = ",", simplify = T))
  if(!is_good(ss)) {
    count <- 0
    while(!is_good(ss) && count < 1000) {
      for(j in 1:(length(ss) - 1)) {
        for(k in (j + 1):length(ss)) {
          tt <- paste(ss[j], ss[k], sep = "x")
          if(!(tt %in% aa$str)) {
            temp <- ss[j]
            ss[j] <- ss[k]
            ss[k] <- temp
          }
        }
      }
      count <- count + 1
    }
    if(count == 1000) {
      message("whoops")
      break
    }
    tot <- tot + as.integer(ss[(1 + length(ss)) / 2])
  }
}
tot



