library(tidyverse)

dd <- readLines("data/day08")
ncol <- str_length(dd[1])
nrow <- length(dd)
buffer <- 0
buffer_char <- "E"
mm <- matrix(".", nrow = nrow + 2 * buffer, ncol = ncol + 2 * buffer)
dd <- dd |> 
  str_split(pattern = "")
str(dd)

if(buffer > 0) {
  buffer_rows <- c(1:buffer, nrow + buffer + 1:buffer)
  buffer_cols <- c(1:buffer, ncol + buffer + 1:buffer)
} else{
  buffer_rows <- NULL
  buffer_cols <- NULL
}
for(i in 1:(nrow + 2 * buffer)) {
  for(j in 1:(ncol + 2 * buffer)) {
    if(i %in% buffer_rows || j %in% buffer_cols) {
      mm[i, j] <- buffer_char
    } else{
      mm[i,j] <- dd[[i - buffer]][j - buffer] 
    }
  }
} 
goodindex <- function(p1, p2, mm) {
  if(p1 %in% 1:nrow(mm) && p2 %in% 1:ncol(mm)) {
    return(TRUE)
  } 
  return(FALSE)
}
ant <- setdiff(unique(as.vector(mm)), ".")

zz <- mm
zz[,] <- " "

for(cc in ant) {
  locs <- which(mm == cc, arr.ind = T)
  for(i in 1:(nrow(locs) - 1)) {
    for(j in (i + 1):nrow(locs)) {
      d1 <- locs[i, 1] - locs[j, 1]
      d2 <- locs[i, 2] - locs[j, 2]
      p1 <- locs[j, 1] + 2 * d1
      p2 <- locs[j, 2] + 2 * d2
      if(goodindex(p1, p2, zz)) {
        zz[p1, p2] <- paste(zz[p1, p2], cc, collapse = " ")
      }
      p1 <- locs[i, 1] - 2 * d1
      p2 <- locs[i, 2] - 2 * d2
      if(goodindex(p1, p2, zz)) {
        zz[p1, p2] <- paste(zz[p1, p2], cc, collapse = " ")
      }
      
    }
  }
}

sum(zz != " ")

for(cc in ant) {
  locs <- which(mm == cc, arr.ind = T)
  for(i in 1:(nrow(locs) - 1)) {
    for(j in (i + 1):nrow(locs)) {
      d1 <- locs[i, 1] - locs[j, 1]
      d2 <- locs[i, 2] - locs[j, 2]
      gcd <- pracma::gcd(d1, d2)
      if(gcd != 1) {
        print(gcd) #humph!
      }
      d1 <- d1/gcd
      d2 <- d2/gcd
      jj <- 0
      p1 <- locs[j, 1] + jj * d1
      p2 <- locs[j, 2] + jj * d2
      while(goodindex(p1, p2, zz)) {
        jj <- jj + 1
        zz[p1, p2] <- paste(zz[p1, p2], cc, collapse = " ")
        p1 <- locs[j, 1] + jj * d1
        p2 <- locs[j, 2] + jj * d2
      }
      jj <- 0
      p1 <- locs[j, 1] + jj * d1
      p2 <- locs[j, 2] + jj * d2
      while(goodindex(p1, p2, zz)) {
        jj <- jj - 1
        zz[p1, p2] <- paste(zz[p1, p2], cc, collapse = " ")
        p1 <- locs[j, 1] + jj * d1
        p2 <- locs[j, 2] + jj * d2
      }
    }
  }
}
sum(zz != " ")