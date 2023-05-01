read_structured_file <- function(.path) {
  ret <- yaml:::read_yaml(.path)
  
  return(ret)
}

extract_info <- function() {
  
}

create_directory <- function() {
  .path <- "research/articles/"
  rand <- stringi::stri_rand_strings(1, 10)
  dir.create(paste0(.path, rand))
  
  return(paste0(.path, rand, "/"))
}

pipeline <- function(path_in) {
  res <- read_structured_file(path_in)
  path_out <- create_directory()
  file.copy(from = path_in, 
            to = path_out)
  file.remove(path_in)
  file.copy(from = "research/template", 
            to = path_out)
  file.rename(from = paste0(path_out, "template"), 
              to = paste0(path_out, "index.qmd"))
}
