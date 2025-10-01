# Test setup.
test_prefixes <- c(
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
)

as_tibble <- function(prefix_list) {
  purrr::map_dfr(
    stringr::str_split(prefix_list, " "),
    ~ tibble::tibble(
      short = stringr::str_remove(.x[2], ":"),
      long = substr(.x[3], 2, nchar(.x[3]) - 1)
    )
  )
}

# Running tests.
test_that(
  "function returns correct PREFIX string for single prefix",
  {
    prefixes <- as_tibble(test_prefixes[1])
    expect_equal(as_sparql_prefix(prefixes), test_prefixes[1])
  }
)

test_that(
  "function returns correct PREFIX string for multiple prefixes",
  {
    prefixes <- as_tibble(test_prefixes)
    expect_equal(
      as_sparql_prefix(prefixes),
      paste(test_prefixes, collapse = "\n")
    )
  }
)

test_that(
  "as_sparql_prefix returns empty string for empty tibble",
  {
    prefixes <- tibble::tibble(short = character(), long = character())
    expect_equal(as_sparql_prefix(prefixes), "")
  }
)
