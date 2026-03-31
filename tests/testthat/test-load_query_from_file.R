QUERY_WITH_COMMENTS <- "test_query_multiple_prefixes.rq"
QUERY_WITHOUT_COMMENTS <- "test_query_single_prefix.rq"
QUERY_ONLY_COMMENTS <- "test_query_only_comments.rq"


test_that("function loads a simple query file without comments", {
  file_path <- test_path("fixtures", QUERY_WITHOUT_COMMENTS)
  query <- load_query_from_file(file_path)

  # Verify that the returned value is a single string.
  expect_type(query, "character")
  expect_length(query, 1)

  # Verify the content includes expected elements.
  expected_query <- paste(
    "PREFIX wdt: <http://www.wikidata.org/prop/direct/>",
    "SELECT ?name",
    "WHERE {",
    "  ?name wdt:P106 wd:Q36834 .",
    "}",
    sep = "\n"
  )
  expect_equal(query, expected_query)
})


test_that("function preserves comments by default", {
  file_path <- test_path("fixtures", QUERY_WITH_COMMENTS)
  query <- load_query_from_file(file_path)

  # Verify the content includes expected elements.
  expected_query <- paste(
    "# A test SPARQL query with multiple prefixes.",
    "# * Prefixes are not in alphabetical order, to test that they get sorted",
    "#   properly.",
    "# * Some have extra whitespace, to test they get parsed properly.",
    "",
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX owl: <http://www.w3.org/2002/07/owl#>",
    "PREFIX   wd:   <http://www.wikidata.org/entity/>",
    "PREFIX wdt:    <http://www.wikidata.org/prop/direct/>",
    "",
    "# This is a comment in the middle",
    "SELECT ?subject ?predicate ?object",
    "WHERE {",
    "  # Another comment inside the query",
    "  ?subject ?predicate ?object .",
    "}",
    "# Final comment",
    "LIMIT 10",
    sep = "\n"
  )
  expect_equal(query, expected_query)
})


test_that("function removes comments when instructed", {
  file_path <- test_path("fixtures", QUERY_WITH_COMMENTS)
  query <- load_query_from_file(file_path, remove_comments = TRUE)

  # Verify the content includes expected elements.
  expected_query <- paste(
    "",
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX owl: <http://www.w3.org/2002/07/owl#>",
    "PREFIX   wd:   <http://www.wikidata.org/entity/>",
    "PREFIX wdt:    <http://www.wikidata.org/prop/direct/>",
    "",
    "SELECT ?subject ?predicate ?object",
    "WHERE {",
    "  ?subject ?predicate ?object .",
    "}",
    "LIMIT 10",
    sep = "\n"
  )
  expect_equal(query, expected_query)
})


test_that("function handles files with only comments", {
  file_path <- test_path("fixtures", QUERY_ONLY_COMMENTS)
  query <- load_query_from_file(file_path)

  # Verify the content includes expected elements.
  expected_query <- paste(
    "# This file contains only comments.",
    "# and blank lines.",
    "",
    "# No actual query content.",
    sep = "\n"
  )
  expect_equal(query, expected_query)
})


test_that("function returns empty string when file has only comments", {
  file_path <- test_path("fixtures", QUERY_ONLY_COMMENTS)
  query <- load_query_from_file(file_path, remove_comments = TRUE)
  expect_equal(query, "")
})
