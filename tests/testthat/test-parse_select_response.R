
test_that(
  "successfully parses a valid SPARQL SELECT response",
  {
    test_response <- readRDS("sparql_select_response.rds")
    parsed <- parse_select_response(test_response)

    expect_equal(parsed$results$bindings[[1]]$message$type, "literal")
    expect_equal(parsed$results$bindings[[1]]$message$value, "Hello world")
  }
)
