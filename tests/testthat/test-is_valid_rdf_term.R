test_that("is_valid_rdf_term returns TRUE for valid RDF terms", {
  term <- list()
  term$type <- "IRI"
  term$value <- "foo"
  expect_true(is_valid_rdf_term(term))
})

test_that("is_valid_rdf_term returns FALSE for invalid RDF terms", {
  invalid_term <- list()
  invalid_term$type <- "IRI"
  expect_false(is_valid_rdf_term(invalid_term))
})

test_that("is_valid_rdf_term handles NA and NULL", {
  expect_false(is_valid_rdf_term(NA))
  expect_false(is_valid_rdf_term(NULL))
})
