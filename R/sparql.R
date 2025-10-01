MIME_TYPE_SPARQL_JSON <- "application/sparql-results+json"
MIME_TYPE_N_TRIPLE <- "application/n-triples"
DEFAULT_PREFIXES <- tibble::tribble(
  ~short, ~long,
  "rdf",  "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  "rdfs", "http://www.w3.org/2000/01/rdf-schema#"
)


# Run an HTTP request on a SPARQL endpoint.
run_http_request <- function(
  endpoint,
  http_params,
  mime_type,
  use_post = FALSE
) {
  # Build and run the HTTP request.
  add_params <- if (use_post) httr2::req_body_form else httr2::req_url_query
  response <- httr2::request(endpoint) |>
    httr2::req_headers(Accept = mime_type) |>
    add_params(!!!http_params) |>
    httr2::req_perform()

  # Return the HTTP response if it completed successfully.
  if (httr2::resp_status(response) == 200) {
    return(response)
  }

  # Otherwise an error is raised.
  response_summary <- paste(capture.output(print(response)), collapse = "\n")
  response_header <- paste(
    capture.output(print(response$header)), collapse = "\n"
  )
  rlang::abort(
    paste0(
      "Error in HTTP request.'\n",
      " -> endpoint    : ", endpoint, "\n",
      " -> http_params : ", http_params, "\n",
      " -> MIME type   : ", mime_type, "\n",
      " -> request type: ", ifelse(use_post, "POST", "GET"), "\n",
      " -> response    : ", response_summary, "\n",
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
    "bnode" = paste0("_:", value$value),
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
    type.convert(na.strings = c(""), as.is = TRUE)
}


#' Change the style of IRIs from "long" into another form.
#'
#' @description Modify the style of IRIs in all columns of a tibble.
#'
#' @param t         Tibble whose IRIs are to be modified.
#' @param prefixes  Tibble with "short" and "long" versions of the prefixes
#'                  for which the IRI style should be modified.
#' @param iri_style One of "short", "mdlink", "html" or "long".
#'
#' @return An copy of the input tibble `t` where the IRI style was modified.
#'
#' @export
modify_iri_style <- function(t, prefixes, iri_style = "short") {

  # The input is assumed to already be in "long" form, so if "long" is
  # requested, there is nothing to change.
  if (iri_style == "long") {
    return(t)
  }

  # Change IRIs from "long" to other forms.
  short_forms <- unlist(prefixes[, 1])
  long_forms <- unlist(prefixes[, 2])
  pattern <- paste0("<(", long_forms, ")(\\S+)>")
  replacement <- switch(
    iri_style,
    "short" = paste0(short_forms, ":$2"),
    "mdlink" = paste0("[", short_forms, ":$2]($1$2)"),
    "html" = paste0('<a href="', long_forms, '$2">', short_forms, ":$2</a>"),
    rlang::abort(paste("Unsupported iri_style:", iri_style))
  )

  # Note: applying the regexp using dplyr is much faster than trying to apply
  # it when converting individual RDF terms.
  t |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(rlang::is_character),
        function(x) {
          stringr::str_replace_all(
            unlist(x),
            pattern = pattern,
            replacement = replacement
          )
        }
      )
    )
}


#' Run a SPARQL SELECT query
#'
#' @description Run a SPARQL query, either SELECT, CONSTRUCT or DESCRIBE and
#' return the results as a tibble. Returned column names are the same as SPARQL
#' variables. Detection of column types relies on R built-in methods, not RDF
#' data types.
#'
#' In the HTTP request, the "application/sparql-results+json" MIME type is
#' used, which is supported by most SPARQL endpoints.
#'
#' @param endpoint     URL of SPARQL endpoint.
#' @param query        SPARQL query as a string.
#' @param prefixes     Optional data frame whose first two columns are taken
#'                     for short and long versions of base IRIs.
#' @param http_params  Additional parameter/value pair to pass with the HTTP
#'                     request.
#'                     Example: some endpoints accept a "timeout" argument.
#'                     This could be passed via this argument.
#' @param use_post     Boolean to switch http protocol (default to GET). The
#'                     main benefit of POST is to allow for larger input query.
#' @param add_prefixes Boolean to add PREFIX declarations passed to the
#'                     `prefixes` argument to the SPARQL query.
#' @param iri_style    One of "long", "short" , "html" or "mdlink" (markdown
#'                     link), to encode returned IRIs.
#' @param na_value     Value with which to replace empty fields.
#' @param echo         Boolean value to echo the SPARQL query before execution.
#'
#' @return             A tibble with the query results or NULL if the query
#'                     returns nothing.
#' @seealso            SPARQL_ask() SPARQL_update()
#'
#' @export
sparql_select <- function(
  endpoint,
  query,
  prefixes     = DEFAULT_PREFIXES,
  http_params  = list(),
  use_post     = FALSE,
  add_prefixes = FALSE,
  iri_style    = "short",
  na_value     = NA,
  echo         = FALSE
) {
  # Prepend PREFIXes to the SPARQL query.
  query <- add_prefixes_to_query(
    query,
    prefixes = ifelse(add_prefixes, prefixes, NULL)
  )
  if (echo) cat(query)
  http_params$query <- query

  # Run the HTTP request on the SPARQL endpoint.
  start_time <- Sys.time()
  response <- run_http_request(
    endpoint,
    http_params,
    mime_type = MIME_TYPE_SPARQL_JSON,
    use_post = use_post
  )
  message(paste("Query time:", elapsed_time(start_time, end_time = Sys.time())))

  # Try to parse the response as JSON. If the query returned no results, exit
  # function.
  query_result <- parse_select_response(response)
  if (length(query_result$results$bindings) == 0) {
    return(NULL)
  }

  # Convert the HTTP query response into a tibble. Adapt IRI style to the
  # format requested by the user.
  query_result |>
    query_result_to_tibble() |>
    modify_iri_style(prefixes, iri_style = iri_style)
}


#' Run a SPARQL CONSTRUCT query
#'
#' @description
#' Executes a SPARQL CONSTRUCT query and returns the results as a list
#' containing two tibbles: one for edges (triples with IRIs) and one for nodes
#' with properties.
#' IRIs can be formatted in different styles using the `iri_style` argument.
#'
#' @param endpoint     URL of the SPARQL endpoint.
#' @param query        SPARQL CONSTRUCT query as a string.
#' @param prefixes     Optional data frame whose first two columns are short
#'                     and long versions of base IRIs.
#' @param add_prefixes Boolean to add PREFIX declarations from `prefixes` to
#'                     the query.
#' @param http_params  Additional parameter/value pairs for the HTTP request.
#' @param use_post     Boolean to use POST instead of GET for the HTTP request.
#' @param iri_style    One of "long", "short", "html", or "mdlink" to encode
#'                     returned IRIs.
#' @param echo         Print the SPARQL query that is executed in the terminal.
#'
#' @return             A list with two tibbles: `edges` and `nodes`.
#'
#' @export
sparql_construct <- function(
  endpoint,
  query,
  prefixes = DEFAULT_PREFIXES,
  add_prefixes = FALSE,
  http_params  = list(),
  use_post = FALSE,
  iri_style = "long",
  echo = FALSE
) {
  # Prepend PREFIXes to the SPARQL query.
  query <- add_prefixes_to_query(
    query,
    prefixes = ifelse(add_prefixes, prefixes, NULL)
  )
  if (echo) cat(query)
  http_params$query <- query

  # Run the HTTP request on the SPARQL endpoint.
  start_time <- Sys.time()
  response <- run_http_request(
    endpoint,
    http_params,
    mime_type = MIME_TYPE_N_TRIPLE,
    use_post = use_post
  )
  message(paste("Query time:", elapsed_time(start_time, end_time = Sys.time())))

  # Try to parse the response as n-triple strings. If the query returned no
  # results, exit function.
  query_result <- parse_construct_response(response)
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
    dplyr::arrange(dplyr::across(dplyr::everything())) |>
    modify_iri_style(prefixes, iri_style = iri_style)

  # Create a data frame with all nodes that have at least one "property"
  # associated with them. The column with node names must be named "id" in
  # order to be easily compatible with visNetwork.
  nodes_with_properties <- query_result[grepl(
    REGEXP_LITERAL_TRIPLE, query_result, perl = TRUE
  )] |>
    lapply(get_iri_and_lit_from_ntriple) |>
    do.call(what = rbind) |>
    dplyr::as_tibble(stringsAsFactors = FALSE) |>
    dplyr::rename(id = 1, edge = 2, property = 3) |>
    dplyr::mutate(
      property = stringr::str_extract(property, REGEXP_LITERAL_UNQUOTED)
    ) |>
    modify_iri_style(prefixes, iri_style = iri_style) |>
    dplyr::group_by(id, edge) |>
    dplyr::summarise(
      property = paste(property, collapse = " | "),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = edge,
      values_from = property,
      values_fill = NA
    )

  # Return a list with 2 data frames:
  list(
    edges = edges,
    nodes = dplyr::full_join(
      tibble::tibble(id = union(edges$from, edges$to)),  # List of all nodes.
      nodes_with_properties, by = "id"
    ) |>
      dplyr::arrange(id)
  )
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


#' Run a SPARQL DESCRIBE query
#'
#' @description
#' Executes a SPARQL DESCRIBE query and returns the results tibble with 3
#' columns: subject, predicate, object.
#'
#' IRIs can be formatted in different styles using the `iri_style` argument.
#'
#' @param endpoint     URL of the SPARQL endpoint.
#' @param query        SPARQL CONSTRUCT query as a string.
#' @param prefixes     Optional data frame whose first two columns are short
#'                     and long versions of base IRIs.
#' @param add_prefixes Boolean to add PREFIX declarations from `prefixes` to
#'                     the query.
#' @param http_params  Additional parameter/value pairs for the HTTP request.
#' @param use_post     Boolean to use POST instead of GET for the HTTP request.
#' @param iri_style    One of "long", "short", "html", or "mdlink" to encode
#'                     returned IRIs.
#' @param echo         Print the SPARQL query that is executed in the terminal.
#'
#' @return             A list with two tibbles: `edges` and `nodes`.
#'
#' @export
sparql_describe <- function(
  endpoint,
  query,
  prefixes = DEFAULT_PREFIXES,
  add_prefixes = FALSE,
  http_params  = list(),
  use_post = FALSE,
  iri_style = "long",
  echo = FALSE
) {
  # Prepend PREFIXes to the SPARQL query.
  query <- add_prefixes_to_query(
    query,
    prefixes = ifelse(add_prefixes, prefixes, NULL)
  )
  if (echo) cat(query)
  http_params$query <- query

  # Run the HTTP request on the SPARQL endpoint.
  start_time <- Sys.time()
  response <- run_http_request(
    endpoint,
    http_params,
    mime_type = MIME_TYPE_N_TRIPLE,
    use_post = use_post
  )
  message(paste("Query time:", elapsed_time(start_time, end_time = Sys.time())))

  # Try to parse the response as n-triple strings. If the query returned no
  # results, exit function.
  query_result <- parse_construct_response(response)
  if (length(query_result) == 0) {
    return(NULL)
  }

  # Convert the list of n-triples into a tibble with
  # subject / predicate / object columns.
  n_triples_to_tibble(n_triples = query_result)
}

# Convert a list of n-triples to a 3-column tibble: subject, predicate, object.
n_triples_to_tibble <- function(n_triples) {
  stringr::str_split(n_triples, "\\s+", simplify = TRUE) |>
    tibble::as_tibble(stringsAsFactors = FALSE, .name_repair = "minimal") |>
    dplyr::rename(subject = 1, predicate = 2, object = 3)
}
