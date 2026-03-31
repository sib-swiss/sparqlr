# Import `rlang` so that lintr in VS code does not complain about `.data`.
library(rlang)

# MIME (Multipurpose Internet Mail Extensions) type to use for making SPARQL
# requests with a given query form.
MIME_TYPES_BY_QUERY_FORM <- list(
  SELECT    = "application/sparql-results+json",
  CONSTRUCT = "application/n-triples",
  DESCRIBE  = "application/n-triples",
  ASK       = "unimplemented"
)


# Run a SPARQL query on the specified endpoint.
sparql_query <- function(
  endpoint,
  query,
  query_form = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK"),
  request_method = c("POST", "GET"),
  http_extra_params = list()
) {
  # Validate input.
  query_form <- match.arg(query_form)
  request_method <- match.arg(request_method)

  # Build the list of parameters to pass to the HTTP request.
  http_params <- http_extra_params
  http_params$query <- query

  # Run the HTTP request on the SPARQL endpoint.
  http_request(
    endpoint,
    http_params,
    mime_type = MIME_TYPES_BY_QUERY_FORM[[query_form]],
    request_method = request_method
  )
}

# Run an HTTP request on a SPARQL endpoint.
http_request <- function(
  endpoint,
  http_params,
  mime_type,
  request_method = c("POST", "GET")
) {
  # Validate input.
  request_method <- match.arg(request_method)

  # Build and run the HTTP request.
  response <- httr2::request(endpoint) |>
    httr2::req_headers(Accept = mime_type) |>
    ifelse(
      request_method == "POST",
      httr2::req_body_form,
      httr2::req_url_query
    )(!!!http_params) |>
    httr2::req_perform()

  # Return the HTTP response if it completed successfully.
  if (httr2::resp_status(response) == 200) {
    return(response)
  }

  # Otherwise an error is raised.
  response_summary <- paste(
    utils::capture.output(print(response)), collapse = "\n"
  )
  response_header <- paste(
    utils::capture.output(print(response$header)), collapse = "\n"
  )
  rlang::abort(
    paste0(
      "Error in HTTP request.'\n",
      " -> endpoint       : ", endpoint, "\n",
      " -> http_params    : ", http_params, "\n",
      " -> MIME type      : ", mime_type, "\n",
      " -> request method : ", request_method, "\n",
      " -> response       : ", response_summary, "\n",
      " -> response header: ", response_header, "\n"
    )
  )
}


#' Convert an individual RDF term to an R string.
#'
#' @param value    A list with `type` and `value` fields, or `NULL`.
#' @param na_value Value returned when `value` is NULL or an empty string.
#'
#' @keywords internal
rdf_term_to_string <- function(value, na_value = NA) {
  # This function assumes that the input `value` has the correct structure:
  # a list with "type" and "value" fields.
  if (is.null(value) || value$value == "") {
    return(na_value)
  }
  switch(
    value$type,
    "uri" = paste0("<", value$value, ">"),
    "literal" = value$value,
    "bnode" = paste0("_:", sub("^_:", "", value$value)),
    rlang::abort(paste("Unknown RDF type in JSON response:", value$type))
  )
}


#' Parse an HTTP response with content type "application/sparql-results+json".
#'
#' @param response SPARQL SELECT HTTP query response.
#'
#' @keywords internal
parse_select_response <- function(response) {

  # The HTTP response for a SPARQL SELECT query is expected to be in JSON
  # format. This attempts to parse the JSON string into a list.
  # Fallback on HTML if that fails.
  parsed_response <- tryCatch(
    httr2::resp_body_json(response),
    error = httr2::resp_body_html
  )

  # Verify that the parsed response has the expected structure.
  if (
    !(is.list(parsed_response) &&
        all(c("head", "results") %in% names(parsed_response)) &&
        "bindings" %in% names(parsed_response$results))
  ) {
    rlang::abort(
      paste0(
        "Error parsing response from SPARQL SELECT query: '",
        parsed_response,
        "' is not a valid SPARQL SELECT query result."
      )
    )
  }

  parsed_response
}


#' Parse an HTTP response with content type "application/n-triples".
#'
#' @param response SPARQL CONSTRUCT HTTP query response.
#'
#' @keywords internal
parse_construct_response <- function(response) {

  # The HTTP response for a SPARQL CONSTRUCT query is expected to be an
  # n-triple string. This attempts to parse the response body to a string.
  # Fallback on HTML if that fails.
  parsed_response <- tryCatch(
    httr2::resp_body_string(response),
    error = httr2::resp_body_html
  )

  # Split the response to a list of n-triples.
  strsplit(parsed_response, ".\n") |>
    unlist() |>
    purrr::map_chr(trimws)
}


#' Convert a SPARQL SELECT query result into a tibble.
#'
#' @description
#' Converts the result of a parsed SPARQL SELECT query (a list object) into an
#' R tibble.
#'
#' @param query_result  Object (list) to parse.
#' @param na_value      Value with which to replace empty/missing fields.
#'
#' @keywords internal
query_result_to_tibble <- function(query_result, na_value = NA) {
  # Parse the nested list: for each record (row) returned by the request, check
  # whether some fields (columns) are missing, which indicates a "NA" value.
  variable_names <- unlist(query_result$head)
  lapply(
    variable_names,
    function(column_name) {
      sapply(
        query_result$results$bindings,
        function(x) rdf_term_to_string(x[[column_name]], na_value)
      )
    }
  ) |>
    # Convert the parsed list to an R tibble.
    rlang::set_names(variable_names) |>
    tibble::as_tibble() |>
    # Call the built-in type conversion of R.
    utils::type.convert(na.strings = c(""), as.is = TRUE)
}

# Verify that a PREFIX tibble (or data frame) has the correct structure.
is_valid_prefix_tibble <- function(t) {
  required_columns <- c("long", "short")

  if (!inherits(t, "data.frame") || !all(names(t) %in% required_columns)) {
    rlang::abort(
      paste0(
        "Input must be a tibble or data frame with columns: ",
        paste(required_columns, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}


#' Create a function to replace IRI styles in strings.
#'
#' @description Returns a function to replace IRI styles in strings.
#'
#' @param iri_style          One of "long", "short", "mdlink", or "html".
#' @param prefixes           Tibble with "short" and "long" prefix columns.
#' @param replace_in_literal If TRUE, then replacement is also done within
#'                           literal strings, not just in IRIs.
#'
#' @return A function that replaces IRIs according to the specified style.
#'
#' @keywords internal
iri_replacement_function <- function(
  iri_style,
  prefixes,
  replace_in_literal
) {
  # "long" forms are not replaced, so we return an identity function.
  if (iri_style == "long") {
    return(\(x) x)
  }

  short_forms <- unlist(prefixes$short)
  long_forms <- unlist(prefixes$long)

  # List of pattern to match for a regexp.
  pattern <- if (replace_in_literal) {
    # This could possible be written as a single regexp.
    c(
      paste0("<(", long_forms, ")(\\S+)>"),
      paste0("(?:(?<=^)|(?<=\\s))(", long_forms, ")(\\S+)(?:(?=$)|(?=\\s))")
    )
  } else {
    paste0("^<(", long_forms, ")(\\S+)>$")
  }

  # List of replacement values for the patterns defined above. Each pattern
  # must have a matching replacement, which is why `replacement` is duplicated
  # when `replace_in_literal=TRUE`.
  replacement <- switch(
    iri_style,
    "short" = paste0(short_forms, ":\\2"),
    "mdlink" = paste0("[", short_forms, ":\\2](\\1\\2)"),
    "html" = paste0('<a href="', long_forms, '\\2">', short_forms, ":\\2</a>"),
    rlang::abort(paste("Unsupported iri_style:", iri_style))
  )
  if (replace_in_literal) replacement <- c(replacement, replacement)

  # Return string replacement function.
  \(x) stringr::str_replace_all(x, stats::setNames(replacement, pattern))
}


#' Convert the style of IRIs from "long" or "short" into another form.
#'
#' @description Convert the style of IRIs in all columns of a tibble.
#'
#' @param t                  Tibble whose IRIs are to be modified.
#' @param prefixes           Tibble with "short" and "long" versions of the
#'                           prefixes for which the IRI style should be
#'                           modified.
#' @param iri_style          One of "short", "mdlink", "html" or "long".
#' @param replace_in_literal If TRUE, then replacement is also done within
#'                           literal strings, not just in IRIs.
#'
#' @return An copy of the input tibble `t` where the IRI style was modified.
#'
#' @export
convert_iri_style <- function(
  t,
  prefixes,
  iri_style = c("short", "long", "mdlink", "html"),
  replace_in_literal = FALSE
) {
  # Validate user input.
  iri_style <- match.arg(iri_style, several.ok = TRUE)
  if (is.null(prefixes) || nrow(prefixes) == 0) return(t)

  # The input is assumed to already be in "long" form, so if "long" is
  # requested, there is nothing to change.
  if (all(iri_style == "long")) return(t)

  # Sort prefix tibble by decreasing length order, so that longer prefixes
  # are matched (and replaced) before shorter ones.
  prefixes <- dplyr::arrange(prefixes, dplyr::desc(nchar(.data$long)))

  if (length(iri_style) == 1) {
    # Case 1: a single IRI style was passed and will be applied to all columns
    # of the input tibble. This version is kept here as it might be slightly
    # more performant than using `purrr::map2()`
    t |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(rlang::is_character),
          function(x) {
            iri_replacement_function(iri_style, prefixes, replace_in_literal)(x)
          }
        )
      )
  } else {
    # Case 2: multiple IRI styles were passed by the user, and each gets
    # applied to a column of the input tibble.
    purrr::map2(
      t,
      # If needed, recycle the provided IRI styles over the number of columns
      # of the tibble to modify.
      rep(iri_style, length.out = length(t)),
      # Function to replace one IRI style with another.
      function(x, new_style) {
        iri_replacement_function(new_style, prefixes, replace_in_literal)(x)
      }
    ) |>
      tibble::as_tibble()
  }
}

#' Run a SPARQL SELECT query.
#'
#' @description Run a SPARQL query, either SELECT, CONSTRUCT or DESCRIBE and
#' return the results as a tibble. Returned column names are the same as SPARQL
#' variables. Detection of column types relies on R built-in methods, not RDF
#' data types.
#'
#' In the HTTP request, the "application/sparql-results+json" MIME type is
#' used, which is supported by most SPARQL endpoints.
#'
#' @param endpoint          URL of SPARQL endpoint.
#' @param query             SPARQL query as a string.
#' @param prefixes          Optional data frame whose first two columns are
#'                          taken for short and long versions of base IRIs.
#' @param request_method    HTTP method to use to submit the request.
#' @param http_extra_params Additional parameter/value pair to pass to the HTTP
#'                          request. E.g. some endpoints accept a "timeout"
#'                          argument, which could be passed via this argument.
#' @param verbose           If `TRUE`, print query execution time.
#'
#' @return A tibble with the query results or NULL if the query returns nothing.
#' @export
sparql_select <- function(
  endpoint,
  query,
  prefixes = NULL,
  request_method = c("POST", "GET"),
  http_extra_params = list(),
  verbose = FALSE
) {
  # Validate user input.
  request_method <- match.arg(request_method)

  # Run the HTTP request on the SPARQL endpoint.
  start_time <- Sys.time()
  http_response <- sparql_query(
    endpoint,
    query,
    query_form = "SELECT",
    request_method = request_method,
    http_extra_params = http_extra_params
  )
  if (verbose) message(
    paste("Query time:", elapsed_time(start_time, end_time = Sys.time()))
  )

  # Try to parse the response as JSON. If the query returned no results, exit
  # function.
  query_result <- parse_select_response(http_response)
  if (length(query_result$results$bindings) == 0) {
    return(NULL)
  }

  # Convert the HTTP query response into a tibble. Adapt IRI style to the
  # format requested by the user.
  query_result |>
    query_result_to_tibble() |>
    convert_iri_style(prefixes, iri_style = "short") |>
    dplyr::arrange(dplyr::across(dplyr::everything()))
}


#' Run a SPARQL CONSTRUCT query.
#'
#' @description
#' Executes a SPARQL CONSTRUCT query and returns the results as a list
#' containing two tibbles: one for edges (triples with IRIs) and one for nodes
#' with properties.
#' IRIs can be formatted in different styles using the `iri_style` argument.
#'
#' @param endpoint          URL of SPARQL endpoint.
#' @param query             SPARQL query as a string.
#' @param prefixes          Optional data frame whose first two columns are
#'                          taken for short and long versions of base IRIs.
#' @param request_method    HTTP method to use to submit the request.
#' @param http_extra_params Additional parameter/value pair to pass to the HTTP
#'                          request. E.g. some endpoints accept a "timeout"
#'                          argument, which could be passed via this argument.
#' @param verbose           If `TRUE`, print query execution time.
#'
#' @return             A list with two tibbles: `edges` and `nodes`.
#'
#' @export
#' @importFrom rlang .data
sparql_construct <- function(
  endpoint,
  query,
  prefixes = NULL,
  request_method = c("POST", "GET"),
  http_extra_params = list(),
  verbose = FALSE
) {
  # Validate user input.
  request_method <- match.arg(request_method)

  # Run the HTTP request on the SPARQL endpoint.
  start_time <- Sys.time()
  http_response <- sparql_query(
    endpoint,
    query,
    query_form = "CONSTRUCT",
    request_method = request_method,
    http_extra_params = http_extra_params
  )
  if (verbose) message(
    paste("Query time:", elapsed_time(start_time, end_time = Sys.time()))
  )

  # Try to parse the response as n-triple strings. If the query returned no
  # results, exit function.
  query_result <- parse_construct_response(http_response)
  if (length(query_result) == 0) {
    return(NULL)
  }

  # Create a data frame with all regular nodes of the graph: "from" and "to"
  # are the nodes, and "edge" is the predicate name (i.e. name of the edge).
  edges <- query_result[grepl(REGEXP_IRI_TRIPLE, query_result, perl = TRUE)] |>
    lapply(get_iri_from_ntriple) |>
    do.call(what = rbind) |>
    dplyr::as_tibble(stringsAsFactors = FALSE) |>
    dplyr::select("from" = 1,  "to" = 3, "edge" = 2) |>
    convert_iri_style(prefixes, iri_style = "short") |>
    dplyr::arrange(dplyr::across(dplyr::everything()))

  # Create a data frame with all nodes that have at least one "property"
  # associated with them. The column with node names must be named "id" in
  # order to be easily compatible with visNetwork.
  nodes_with_properties <- query_result[grepl(
    REGEXP_LITERAL_TRIPLE, query_result, perl = TRUE
  )] |>
    lapply(get_iri_and_literal_from_ntriple) |>
    do.call(what = rbind) |>
    dplyr::as_tibble(stringsAsFactors = FALSE) |>
    dplyr::rename(id = 1, edge = 2, property = 3) |>
    dplyr::mutate(
      property = stringr::str_extract(
        .data$property,
        REGEXP_LITERAL_UNQUOTED
      )
    ) |>
    convert_iri_style(prefixes, iri_style = "short") |>
    dplyr::arrange(dplyr::across(dplyr::everything())) |>
    dplyr::group_by(.data$id, .data$edge) |>
    dplyr::summarise(
      property = paste(.data$property, collapse = " | "),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = "edge",
      values_from = "property",
      values_fill = NA
    )

  # Return a list with 2 data frames:
  list(
    edges = edges,
    nodes = dplyr::full_join(
      tibble::tibble(id = union(edges$from, edges$to)),  # List of all nodes.
      nodes_with_properties, by = "id"
    ) |>
      dplyr::arrange(.data$id)
  )
}

#' Run a SPARQL DESCRIBE query.
#'
#' @description
#' Executes a SPARQL DESCRIBE query and returns the results tibble with 3
#' columns: subject, predicate, object.
#'
#' IRIs can be formatted in different styles using the `iri_style` argument.
#'
#' @param endpoint          URL of SPARQL endpoint.
#' @param query             SPARQL query as a string.
#' @param prefixes          Optional data frame whose first two columns are
#'                          taken for short and long versions of base IRIs.
#' @param request_method    HTTP method to use to submit the request.
#' @param http_extra_params Additional parameter/value pair to pass to the HTTP
#'                          request. E.g. some endpoints accept a "timeout"
#'                          argument, which could be passed via this argument.
#' @param verbose           If `TRUE`, print query execution time.
#'
#' @return             A list with two tibbles: `edges` and `nodes`.
#'
#' @export
sparql_describe <- function(
  endpoint,
  query,
  prefixes = NULL,
  request_method = c("POST", "GET"),
  http_extra_params = list(),
  verbose = FALSE
) {
  # Validate user input.
  request_method <- match.arg(request_method)

  # Run the HTTP request on the SPARQL endpoint.
  start_time <- Sys.time()
  http_response <- sparql_query(
    endpoint,
    query,
    query_form = "DESCRIBE",
    request_method = request_method,
    http_extra_params = http_extra_params
  )
  if (verbose) message(
    paste("Query time:", elapsed_time(start_time, end_time = Sys.time()))
  )

  # Try to parse the response as n-triple strings. If the query returned no
  # results, exit function.
  query_result <- parse_construct_response(http_response)
  if (length(query_result) == 0) {
    return(NULL)
  }

  # Convert the list of n-triples into a tibble with
  # subject / predicate / object columns.
  n_triples_to_tibble(n_triples = query_result) |>
    convert_iri_style(prefixes, iri_style = "short")
}

sparql_update <- function() {
  stop("not yet implemented")
}


sparql_ask <- function() {
  stop("not yet implemented")

  # This should support:
  # httr2::req_headers(Accept = "application/sparql-results+json")
}


sparql_count <- function() {
  stop("not yet implemented")

}
