
dd <- readLines("data/day10")
buffer_width <- 3
buffer_char <- "10"
make_matrix <- function(filename, buffer_width = 0, buffer_char = ".") {
  dd <- readLines(filename)
  
  ncol <- str_length(dd[1])
  nrow <- length(dd)
  if(missing(buffer_width)) {
    buffer_width <- 0
  }
  if(missing(buffer_char)) {
    buffer_char <- "."
  }

  mm <- matrix(".", nrow = nrow + 2 * buffer_width, ncol = ncol + 2 * buffer_width)
  dd <- dd |> 
    str_split(pattern = "")
  
  if(buffer_width > 0) {
    buffer_rows <- c(1:buffer_width, nrow + buffer_width + 1:buffer_width)
    buffer_cols <- c(1:buffer_width, ncol + buffer_width + 1:buffer_width)
  } else{
    buffer_rows <- NULL
    buffer_cols <- NULL
  }
  for(i in 1:(nrow + 2 * buffer_width)) {
    for(j in 1:(ncol + 2 * buffer_width)) {
      if(i %in% buffer_rows || j %in% buffer_cols) {
        mm[i, j] <- buffer_char
      } else{
        mm[i,j] <- dd[[i - buffer_width]][j - buffer_width] 
      }
    }
  } 
  return(mm)
}
