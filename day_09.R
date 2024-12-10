library(tidyverse)
dd <- read_lines("data/day_09")
dd <- dd |> 
  str_split(pattern = "") |> 
  unlist() |> 
  as.integer()

# dd <- "2333133121414131402" |> 
#   str_split(pattern = "") |> 
#   unlist() |> 
#   as.integer()

aa <- integer(sum(dd))
cc <- cumsum(dd)
max(cc)
cur <- cc[1]
for(i in 1:length(cc)) {
  npos <- cc[i]
  if(npos < cur) {
    print("dd")
  } else{
    if(i %% 2 == 1) {
      fill <- (i - 1) / 2
    } else {
      fill <- -1
    }
    for(j in cur:(npos)) {
      aa[j] <- fill
    }
  }
  cur <- npos + 1
}

is.unsorted(aa[aa!= -1])
oldaa <- aa
empties <- which(aa == -1)
tots <- sum(aa!= -1)
ee <- empties[empties <= tots]
movers <- which(aa!= -1)
movers <- rev(movers)
for(j in 1:length(ee)) {
  aa[ee[j]] <- aa[movers[j]]
}

for(j in (tots + 1):length(aa)) {
  aa[j] <- -1
}
nn <- aa[aa > -1]
print(sum(nn * 0:(length(nn) - 1)), digits = 14)

canmove <- function(filenumber, aa) {
  curpos <- which(aa == filenumber)
  tt <- aa[1:(min(curpos) - 1)]
  tt <- paste0(ifelse(tt == -1, 1, 0), collapse = "")
  ll <- length(curpos)
  patt <- paste0(rep("1", times = ll), collapse = "")
  if(str_detect(tt, patt)) {
    return( str_locate(tt, patt)[1] )
  } else{
    return(-1)
  }
}

movefile <- function(filenumber, newstart, aa) {
  oldpos <- which(aa == filenumber)
  aa[oldpos] <- -1
  aa[oldpos + newstart - min(oldpos)] <- filenumber
  return(aa)
}

aa <- oldaa
for(i in max(aa):1) {
  cm <- canmove(i, aa)
  if(cm != -1) {
    aa <- movefile(i, cm, aa)
  }
  if(i %% 200 == 0) {
    print(i)
  }
}

nn <- aa
nn[aa == -1] <- 0
print(sum(nn * 0:(length(nn) - 1)), digits = 14)




