library(tidyverse)
dd <- readLines("data/day03")
aa <- str_extract_all(dd, "mul\\([0-9]{1,3},[0-9]{1,3}\\)")

aa
aa[[1]] |> 
  paste(collapse = " ") |> 
  str_replace_all("mul\\(", " ") |> 
  str_replace_all(",", "*") |> 
  str_replace_all("\\)", "+") |> 
  paste("0", collapse = " ") %>% 
  parse(text = .) |> 
  eval()

sapply(aa, function(x) {
  x |> 
    paste(collapse = " ") |> 
    str_replace_all("mul\\(", " ") |> 
    str_replace_all(",", "*") |> 
    str_replace_all("\\)", "+") |> 
    paste("0", collapse = " ") %>% 
    parse(text = .) |> 
    eval()
}) |> 
  sum()

aa <- str_extract_all(dd, "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)")
aa
aa <- paste(purrr::map_chr(aa, function(x) {
  paste(x, collapse = " ")
}), collapse = " ")



str_remove(aa, "don't\\(\\).*(?=do\\(\\))")
bb <- data.frame(x = strsplit(aa, split = "don't\\(\\)") |> 
                   unlist())
bb |> 
  rowwise() |> 
  mutate( y = str_extract(x, "do\\(\\).*")) |>
  mutate(y = ifelse(is.na(y), "mul(1, 0)", y)) |> 
  mutate(x = str_remove_all(x, "do\\(\\)|don't\\(\\)"),
         y = str_remove_all(y, "do\\(\\)|don't\\(\\)")) |>
  mutate(score_y = str_replace_all(y, "mul\\(", " ") |> 
           str_replace_all(",", "*") |> 
           str_replace_all("\\)", "+") |> 
           paste("0", collapse = " ") %>% 
           parse(text = .) |> 
           eval()) |> 
  mutate(score_x = str_replace_all(x, "mul\\(", " ") |> 
           str_replace_all(",", "*") |> 
           str_replace_all("\\)", "+") |> 
           paste("0", collapse = " ") %>% 
           parse(text = .) |> 
           eval()) |>
  ungroup() |> 
  summarize(end = sum(score_y) + first(score_x))
  ungroup() |> 
  slice_min(str_length(x), n = 12)
  


