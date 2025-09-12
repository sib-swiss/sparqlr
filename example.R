# Load SPARQL functions.
source("sparql.R")
source("../use-case.git/utils.R")

# ******************************************************************************
# DESCRIBE query

endpoint <- "https://sparql.uniprot.org"
query <- "
PREFIX up: <http://purl.uniprot.org/core/>
DESCRIBE up:enzymeClass
"
prefixes <- DEFAULT_PREFIXES
add_prefixes <- FALSE
http_params  <- list()
use_post <- FALSE


# Demo query result.

test_ntriple <- "
<lang:Python> <ex:name> \"Python\" .
<lang:Python> <ex:creator> \"Guido van Rossum\" .
<lang:Python> <ex:releaseYear> \"1991\"^^<http://www.w3.org/2001/XMLSchema#gYear> .
<lang:Python> <ex:influencedBy> <lang:ABC> .
<lang:Python> <ex:influences> <lang:Rust> .
<lang:R> <ex:name> \"R\" .
<lang:R> <ex:creator> \"unknown\" .
<lang:Rust> <ex:name> \"Rust\" .
<lang:Rust> <ex:creator> \"Graydon Hoare\" .
<lang:Rust> <ex:releaseYear> \"2010\"^^<http://www.w3.org/2001/XMLSchema#gYear> .
<lang:Rust> <ex:influencedBy> <lang:Python> .
<lang:Rust> <ex:influencedBy> <lang:CPlusPlus> .
"

test_ntriple <- "
<lang:Python> <ex:name> \"Python\" .
<lang:Python> <ex:creator> \"Guido van Rossum\" .
<lang:Python> <ex:fasterThan> <lang:R> .
<lang:Python> <ex:slowerThan> <lang:Rust> .
<lang:R> <ex:name> \"R\" .
<lang:R> <ex:creator> \"unknown\" .
<lang:Rust> <ex:name> \"Rust\" .
<lang:Rust> <ex:creator> \"Graydon Hoare\" .
<lang:Rust> <ex:hypeLevel> <hype:max> .
<lang:Rust> <ex:fasterThan> <lang:Python> .
<lang:Rust> <ex:hasMascot> <mascot:Ferris> .
<lang:Rust> <ex:mascotName> \"Ferris\" .
<lang:Rust> <ex:mascotName> \"Ferris the crab\" .
<lang:Go> <ex:mascotName> \"The Go Gopher\" .
"
query_result <- strsplit(test_ntriple, ".\n")[[1]] |>
  lapply(trimws) |>
  unlist()

desc_output <- list(
  edges = edges,
  nodes = dplyr::full_join(
    tibble::tibble(id = union(edges$from, edges$to)),  # List of all nodes.
    nodes_with_properties, by = "id"
  ) |>
    dplyr::arrange(id)
)


sparql_describe(endpoint = endpoint, query = query)
desc_output <- sparql_describe(endpoint = endpoint, query = query)

library(igraph)
library(visNetwork)
g <- graph_from_data_frame(
  desc_output$nodes,
  vertices = desc_output$properties
)
visNetwork(
  nodes = desc_output$nodes |> dplyr::mutate(color = "red", label = id),
  edges = desc_output$edges |> dplyr::mutate(label = edge, color = "red")
)




# ******************************************************************************
# Construct
library(igraph)
library(visNetwork)

source("sparql.R")
source("../use-case.git/utils.R")

# Set endpoints and paths to SPARQL queries.
endpoint_wikidata <- "https://query.wikidata.org/sparql"
endpoint_idsm <- "https://idsm.elixir-czech.cz/sparql/endpoint/idsm"
endpoint_uniprot <- "https://sparql.uniprot.org/sparql"
endpoint_oma <- "https://sparql.omabrowser.org/sparql"
query_wikidata <- "example_queries/construct_1_wikidata.rq"
query_idsm <- "example_queries/construct_2_idsm.rq"
query_uniprot <- "example_queries/construct_3_uniprot.rq"
query_oma <- "example_queries/construct_4_oma.rq"

query_1 <- load_query_from_file(query_wikidata)
prefixes_1 <- load_prefixes_from_file(query_wikidata)
query_2 <- load_query_from_file(query_idsm)
prefixes_2 <- load_prefixes_from_file(query_idsm) |>
  tibble::add_row(
    short = "cmp", long = "http://rdf.ncbi.nlm.nih.gov/pubchem/compound/"
  ) |>
  tibble::add_row(short = "ex", long = "http://example.") |>
  tibble::add_row(
    short = "CHEBI", long = "http://purl.obolibrary.org/obo/CHEBI_"
  )
query_3 <- load_query_from_file(query_uniprot)
prefixes_3 <- load_prefixes_from_file(query_uniprot)
query_4 <- load_query_from_file(query_oma)
prefixes_4 <- load_prefixes_from_file(query_oma)


constr_1 <- sparql_construct(
  endpoint = endpoint_wikidata,
  query = query_1,
  prefixes = prefixes_1,
  iri_style = "short"
)
constr_2 <- sparql_construct(
  endpoint = endpoint_idsm,
  query = query_2,
  prefixes = prefixes_2,
  iri_style = "short"
)
constr_3 <- sparql_construct(
  endpoint = endpoint_uniprot,
  query = query_3,
  prefixes = prefixes_3,
  iri_style = "short",
  use_post = TRUE,
)
constr_4 <- sparql_construct(
  endpoint = endpoint_uniprot,
  query = query_4,
  prefixes = prefixes_4,
  iri_style = "short",
  use_post = TRUE,
)


graph_1 <- igraph::graph_from_data_frame(
  constr_1$edges,
  vertices = constr_1$nodes
)
graph_2 <- igraph::graph_from_data_frame(
  constr_2$edges,
  vertices = constr_2$nodes
)


V(graph_1)$name


edges <- dplyr::bind_rows(
  constr_1$edges |> dplyr::mutate(label = edge, color = "red"),
  constr_2$edges |> dplyr::mutate(label = edge, color = "green"),
  constr_3$edges |> dplyr::mutate(label = edge, color = "blue")
) |>
  distinct()
nodes <- dplyr::bind_rows(
  constr_1$nodes |> dplyr::mutate(color = "#FF0000"),
  constr_2$nodes |> dplyr::mutate(color = "#7ad3f7"),
  constr_3$nodes |> dplyr::mutate(color = "#9cf363")
) |>
  dplyr::group_by(id) |>
  dplyr::reframe(
    color = subtractive_mix(color),
    dplyr::across(everything(), ~ first(.x)),  # keep first value of all columns
    .groups = "drop"
  )
  
  # |>
  # dplyr::mutate(
  #   title = `<p/direct/P232>`
  # )

visNetwork::visNetwork(nodes = nodes, edges = edges) |>
  visPhysics(stabilization = FALSE)


# Color nodes by data type.
visNetwork::visNetwork(
  edges = constr_1$edges |> dplyr::mutate(label = property, color = "red"),
  nodes = constr_1$nodes |> dplyr::mutate(
    color = "red",
    label = ifelse(
      is.na(`<http://example.orglabel>`),
      id,
      `<http://example.orglabel>`
    ),
    title = paste0(
        "EC: ", `<http://www.wikidata.org/prop/direct/P231>`,
        "/n CAS: ",`<http://www.wikidata.org/prop/direct/P232>`
    )
  )
)

names(constr$nodes)




# ******************************************************************************

