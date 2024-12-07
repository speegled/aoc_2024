library(tidyverse)

dd <- readLines("data/day06")
ncol <- str_length(dd[1])
nrow <- length(dd)
buffer <- 1
buffer_char <- "E"
mm <- matrix(".", nrow = nrow + 2 * buffer, ncol = ncol + 2 * buffer)
dd <- dd |> 
  str_split(pattern = "")
str(dd)

if(buffer > 0) {
  buffer_rows <- c(1:buffer, nrow + buffer + 1:buffer)
  buffer_cols <- c(1:buffer, ncol + buffer + 1:buffer)
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

moveint <- function(char) {
  if(char == "^") {
    dir <- -1
    newchar <- ">"
  }
  if(char == ">") {
    dir <- ncol(mm)
    newchar <- "D"
  }
  if(char == "D") {
    dir <- 1
    newchar <- "<"
  }
  if(char == "<") {
    dir <- -nrow(mm)
    newchar <- "^"
  }
  return(list(dir = dir, newchar = newchar))
}

cur_pos <- which(mm == "^")
while(1) {
  char <- mm[cur_pos]
  new_pos <- moveint(char)
  dir <- cur_pos + new_pos$dir
  if(mm[dir] == "E") {
    break
  } else if(mm[dir] == "#") {
    mm[cur_pos] <- new_pos$newchar
  } else {
    mm[dir] <- char
    cur_pos <- dir
  }
}
sum(!(mm %in% c("E", ".", "#")))

#reload mm before running below

guardmovement <- function(mm, i, j) {
  if(mm[i,j] == ".") {
    mm[i,j] <- "#"
  } else {
    return(FALSE)
  }
  count <- 1
  cur_pos <- which(mm == "^")
  while(count < 16900) {
    char <- mm[cur_pos]
    new_pos <- moveint(char)
    dir <- cur_pos + new_pos$dir
    if(mm[dir] == "E") {
      return(FALSE)
    } else if(mm[dir] == "#") {
      mm[cur_pos] <- new_pos$newchar
    } else if(mm[dir] == mm[cur_pos]) {
      return(TRUE)
    } else {
      if(mm[dir] == ".") {
        count <- 1
      }
      mm[dir] <- char
      cur_pos <- dir
    }
    count <- count + 1
  }
  if(count == 16900) {
    warning("weird loop")
    return(TRUE)
  }
  warning("did not finish")
  return(FALSE)
}

tot <- 0
for(i in 1:nrow(mm)) {
  print(i)
  for(j in 1:ncol(mm)) {
    tot <- tot + guardmovement(mm, i, j)
  }
}


