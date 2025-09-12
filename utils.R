# Read a SPARQL query from a `.rq` file and return it as a multi-line string.
load_query_from_file <- function(path, remove_comments = FALSE) {
  query <- readLines(path)
  if (remove_comments) {
    query <- query[!grepl("^\\s*#", query)]
  }
  paste(query, collapse = "\n")
}

# Extract the list of PREFIXes from a SPARQL query file, and return it as
# a tibble object.
load_prefixes_from_file <- function(path) {
  prefix_regexp <- "^\\s*PREFIX\\s*"
  grep(prefix_regexp, readLines(path), value = TRUE) |>
    stringr::str_trim() |>
    stringr::str_remove(prefix_regexp) |>
    stringr::str_split_fixed("\\s+", 2) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(c("short", "long")) |>
    dplyr::mutate(long = stringr::str_remove_all(long, "^<|>$")) |>
    dplyr::mutate(short = stringr::str_remove_all(short, ":"))
}



library(tidyverse)

subtractive_mix <- function(hex_colors) {
  if (length(hex_colors) == 0) {
    return("#D3D3D3")
  }
  if (length(hex_colors) == 1) {
    return(hex_colors[1])
  }

  # Convert hex to RGB matrix (0–255)
  rgb_matrix <- col2rgb(hex_colors)

  # Subtractive mixing: take the minimum (i.e. most absorbed) of each channel
  mixed_rgb <- apply(rgb_matrix, 1, min)

  # Convert back to hex
  rgb(mixed_rgb[1], mixed_rgb[2], mixed_rgb[3], maxColorValue = 255)
}



test <- c("#FF0000", "#7ad3f7")
subtractive_mix(test)

x <- "#7A0000"