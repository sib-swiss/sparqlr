# Import `rlang` so that lintr in VS code does not complain about `.data`.
library(rlang)

# Return a string with the elapsed time formatted as "hh:mm:ss".
elapsed_time <- function(start_time, end_time) {
  elapsed_seconds <- round(as.numeric(end_time - start_time, units = "secs"))
  h <- elapsed_seconds %/% 3600
  m <- (elapsed_seconds %% 3600) %/% 60
  s <- elapsed_seconds %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}


#' Convert a 2-column tibble into a multi-line SPARQL PREFIX string.
#'
#' @param prefixes A tibble with columns "short" and "long", containing
#'   respectively the short and long forms of SPARQL prefixes.
#'
#' @keywords internal
as_sparql_prefix <- function(prefixes) {
  if (nrow(prefixes) == 0) {
    return("")
  }
  paste0("PREFIX ", prefixes$short, ": <", prefixes$long, ">") |>
    paste(collapse = "\n")
}


# Verify that the `x` list object has the expected structure of a standard
# SPARQL query response in JSON format.
is_valid_rdf_term <- function(x) {
  is.list(x) && !is.null(x$type) && !is.null(x$value)
}


# Convert a list of n-triples to a 3-column tibble: subject, predicate, object.
n_triples_to_tibble <- function(n_triples) {
  split_ntriple(n_triples) |>
    tibble::as_tibble(stringsAsFactors = FALSE, .name_repair = "minimal") |>
    dplyr::rename(subject = 1, predicate = 2, object = 3)
}


# Read a SPARQL query from a `.rq` file and return it as a multi-line string.
load_query_from_file <- function(path, remove_comments = FALSE) {
  query <- readLines(path)
  if (remove_comments) {
    query <- query[!grepl("^\\s*#", query)]
  }
  paste(query, collapse = "\n")
}


#' Extract the list of PREFIXes from a SPARQL query file, and return it as
#' a tibble object.
#'
#' @param path path to SPARQL file.
#'
load_prefixes_from_file <- function(path) {
  prefix_regexp <- "^\\s*PREFIX\\s*"
  grep(prefix_regexp, readLines(path), value = TRUE) |>
    stringr::str_trim() |>
    stringr::str_remove(prefix_regexp) |>
    stringr::str_split_fixed("\\s+", 2) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(c("short", "long")) |>
    dplyr::mutate(long = stringr::str_remove_all(.data$long, "^<|>$")) |>
    dplyr::mutate(short = stringr::str_remove_all(.data$short, ":")) |>
    dplyr::arrange(.data$short) |>
    dplyr::distinct()
}


subtractive_mix <- function(hex_colors) {
  if (length(hex_colors) == 0) {
    return("#D3D3D3")
  }
  if (length(hex_colors) == 1) {
    return(hex_colors[1])
  }

  # Convert hex to RGB matrix (0–255)
  rgb_matrix <- grDevices::col2rgb(hex_colors)

  # Subtractive mixing: take the minimum (i.e. most absorbed) of each channel
  mixed_rgb <- apply(rgb_matrix, 1, min)

  # Convert back to hex
  grDevices::rgb(mixed_rgb[1], mixed_rgb[2], mixed_rgb[3], maxColorValue = 255)
}
