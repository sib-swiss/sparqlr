
REGEXP_IRI <- "<[^>]*>"
REGEXP_IRI_TRIPLE <- paste0(
  "^", REGEXP_IRI, "\\s*", REGEXP_IRI, "\\s*", REGEXP_IRI, "$"
)
REGEXP_LITERAL_UNQUOTED <- "(?<=\").*(?=\")"
REGEXP_LITERAL <- "\".*\""
REGEXP_DATA_TYPE <- paste0("\\^\\^", REGEXP_IRI)

# Regexp to match a literal with an optional data type.
# Matches:
# * "10"^^<http://www.w3.org/2001/XMLSchema#decimal>   ->  10
# * "10"
# * "This should just \"work\""
REGEXP_LITERAL_WITH_DATA_TYPE <- paste0(
  REGEXP_LITERAL, "(", REGEXP_DATA_TYPE, ")?"
)

REGEXP_LITERAL_TRIPLE <- paste0(
  "^", REGEXP_IRI, "\\s*", REGEXP_IRI, "\\s*",
  REGEXP_LITERAL_WITH_DATA_TYPE, "$"
)
REGEXP_BLANK_NODE <- "_:[A-Za-z0-9]+"

get_iri_from_ntriple <- function(x) {
  split_values <- stringr::str_extract_all(x, REGEXP_IRI)[[1]]
  if (length(split_values) != 3) {
    stop("N-triple string '", x, "' does not contain 3 IRI values")
  }
  split_values
}

get_iri_and_literal_from_ntriple <- function(x) {
  split_values <- stringr::str_extract_all(
    x, paste(REGEXP_IRI, REGEXP_LITERAL_WITH_DATA_TYPE, sep = "|")
  ) |>
    unlist()
  if (length(split_values) != 3) {
    stop("N-triple string '", x, "' does not contain 2 IRIs + literal values")
  }
  split_values
}

# Split an N-triple into its 3 constituents: either 3 IRIs or 2 IRS + 1 literal.
split_ntriple <- function(x) {
  stringr::str_split_fixed(x, "(?<=>)\\s*", 3)
}
# For unit testing
# Test strings:
#   x = "<foo> <bar> <foobar>"
#   x = "<foo><bar><foobar>"
#   x = "<a> <b> \"foo <bar> <bar>\"@en"
#   x = "<a> <b> \"foo <bar> <bar>\"^^integer"