read_structured_file <- function(.path) {
  ret <- yaml:::read_yaml(.path)
  
  return(ret)
}

process_authors <- function(tbl) {
  ret <- tbl[[1]] |>
    tidyr::unite(col = "name", c("family", "given"), sep = ", ") |>
    dplyr::summarise(authors = paste(name, collapse = "; ")) |>
    dplyr::select(authors) |>
    dplyr::mutate(authors = stringr::str_to_title(authors), 
                  authors = stringr::str_remove_all(authors, 
                                                    stringr::fixed(".")))
  
  return(ret$authors)
}

extract_info <- function(struct) {
  ret <- rcrossref::cr_works(dois = struct$DOI) |>
    purrr::pluck("data") |>
    dplyr::mutate(authors = process_authors(author))
  
  return(list(
    doi = struct$DOI, 
    dag = struct$DAG, 
    journal = ret$container.title, 
    issue = ret$issue, 
    volume = ret$volume, 
    member = ret$member, 
    pages = ret$page, 
    date = ret$published.print, 
    title = ret$title, 
    authors = ret$authors
  )
  )
}

create_directory <- function() {
  .path <- "research/articles/"
  rand <- stringi::stri_rand_strings(1, 10)
  dir.create(paste0(.path, rand))
  
  return(paste0(.path, rand, "/"))
}

null_transformer <- function(text, envir) {
  out <- glue::identity_transformer(text, envir)
  if (is.null(out)) {
    return("NULL")
  }
  
  return(out)
}

template_index <- function(info, path) {
  template <- glue::glue(
    '---
title: "{{info$title}}"
date: {{info$date}}
doi: {{info$doi}}
pub-info:
  reference: >-
    {{info$authors}}. "{{info$title}}", <em> {{info$journal}} </em> {{info$volume}}, no. {{info$issue}} ({{info$date}}): {{info$pages}}.
    
    DOI: <a href="https://doi.org/{{info$doi}}"><code>{{info$doi}}</code></a>
  links:
    - name: Article
      url: https://doi.org/{{info$doi}}
      icon: fa-solid fa-scroll
---

## Important links

- [Article](https://doi.org/{{info$doi}})

## DAG

```{r}
dag <- dagitty::dagitty(\'{{info$dag}}\')
tidy_dag <- ggdag::tidy_dagitty(dag)

ggdag::ggdag_status(tidy_dag) +
  ggdag::theme_dag()
```

The **adjustment sets** are:

```{r}
dagitty::adjustmentSets(dag)
```

    ', 
    .open = "{{", 
    .close = "}}", 
    .transformer = null_transformer
  )
  
  cat(template, 
      file = paste0(path, "index.qmd"))
}

pipeline <- function(path_in = "research/input") {
  .list_files <- list.files(path_in, 
                            full.names = TRUE)
  
  for (el in .list_files) {
    res <- read_structured_file(el)
    path_out <- create_directory()
    file.copy(from = el, 
              to = path_out)
    file.remove(el)
    .filename <- el |>
      strsplit("/")
    .filename <- .filename[[1]][length(.filename[[1]])]
    file.rename(from = paste0(path_out, .filename), 
                to = paste0(path_out, "dat"))
    info_paper <- extract_info(res)
    template_index(info = info_paper, 
                   path = path_out)
  } # End loop over input files
}
