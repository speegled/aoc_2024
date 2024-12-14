library(tidyverse)

library(memoise)
inp <- c(572556, 22, 0, 528, 4679021, 1, 10725, 2790)
get_stones_small <- function(val, small = 25) {
  x <- val
  for(i in 1:small) {
    y <- rep(-1, times = (2 * length(x)))
    y[seq(2, 2 * length(x), by = 2)] <- x
    for(i in seq(2, 2 * length(x), by = 2)) {
      if(y[i] == 0) {
        y[i] <- 1
      } else if (str_length(as.character(y[i])) %% 2 == 0) {
        z <- as.character(y[i])
        y[i - 1] <- as.integer(str_sub(z, start = 1, end = str_length(z)/2))
        y[i] <- as.integer(str_sub(z, start = str_length(z)/2 + 1, end = str_length(z)))
      } else {
        y[i] <- y[i] * 2024
      }
    }
    x <- y[y!= -1]
    #print(length(x))
  }
  #print(length(x))
  return(length(x))
}
tot <- 0
for(zzz in inp) {
  tot <- tot + get_stones_small(zzz)
}
tot #first star


get_stones <- function(val) {
  x <- val
  for(i in 1:15) {
    y <- rep(-1, times = (2 * length(x)))
    y[seq(2, 2 * length(x), by = 2)] <- x
    for(i in seq(2, 2 * length(x), by = 2)) {
      if(y[i] == 0) {
        y[i] <- 1
      } else if (str_length(as.character(y[i])) %% 2 == 0) {
        z <- as.character(y[i])
        y[i - 1] <- as.integer(str_sub(z, start = 1, end = str_length(z)/2))
        y[i] <- as.integer(str_sub(z, start = str_length(z)/2 + 1, end = str_length(z)))
      } else {
        y[i] <- y[i] * 2024
      }
    }
    x <- y[y!= -1]
    #print(length(x))
  }
  #print(length(x))
  df <- data.frame(table(x))
  df$Freq <- as.numeric(df$Freq)
  df$x <- as.numeric(as.character(df$x))
  return(df)
}

mem_get_stones <- memoise(f = get_stones)

inp <- c(572556, 22, 0, 528, 4679021, 1, 10725, 2790)
tot <- 0
for(zzz in inp) {
  x <- mem_get_stones(zzz)
  for(kk in 1:4) {
    df1 <- mem_get_stones(x$x[1])
    df1$Freq <- df1$Freq * x$Freq[1]
    for(j in 2:nrow(x)) {
      df2 <- mem_get_stones(x$x[j])
      df2$Freq <- df2$Freq * x$Freq[j]
      df1 <- full_join(df1, df2, by = "x") |> 
        rowwise() |> 
        mutate(Freq = sum(c(Freq.x, Freq.y), na.rm = T), .keep = "unused") |> 
        ungroup() 
      if(any(is.na(df1$Freq))) {
        print("disaster")
        break
      }
    }
    x <- df1
  }
  print(sum(x$Freq))
  tot <- tot + sum(x$Freq)
}
print(tot, digits = 16) #second star



