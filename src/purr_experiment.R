library(purrr)
library(repurrrsive)
library(listviewer)

jsonedit(got_chars, mode = "view")
str(got_chars, list.len = 3)
str(got_chars[[1]], list.len = 8)

map(c(9, 16, 25), sqrt)
map(got_chars, "playedBy")
got_chars %>% map(19)
got_chars[[1]] %>% names() %>% length()

sc_name <- function(a_list, a_name) {
  result <- vector(mode = "list", length = length(a_list))
  
  for (i1 in seq_along(a_list)) {
    result[[i1]] <- a_list[[i1]][[a_name]]
  }
  
  return(result)
}

sc_name(got_chars, "playedBy")

got_chars %>% length()

a_list <- got_chars
