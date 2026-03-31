LONG_IRIS <- c(
  "<http://www.wikidata.org/entity/Q178266>",
  "<http://www.wikidata.org/entity/Q2397>",
  "<http://purl.uniprot.org/core/U21>",
  "<http://purl.uniprot.org/core/U27>",
  "<http://www.w3.org/W15>",
  "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
  "<http://www.w3.org/2000/01/rdf-schema#S758>"
)
SHORT_IRIS <- c(
  "wde:Q178266",
  "wde:Q2397",
  "up:U21",
  "up:U27",
  "w3:W15",
  "rdf:type",
  "rdfs:S758"
)
MARKDOWN_IRIS <- c(
  "[wde:Q178266](http://www.wikidata.org/entity/Q178266)",
  "[wde:Q2397](http://www.wikidata.org/entity/Q2397)",
  "[up:U21](http://purl.uniprot.org/core/U21)",
  "[up:U27](http://purl.uniprot.org/core/U27)",
  "[w3:W15](http://www.w3.org/W15)",
  "[rdf:type](http://www.w3.org/1999/02/22-rdf-syntax-ns#type)",
  "[rdfs:S758](http://www.w3.org/2000/01/rdf-schema#S758)"
)

HTML_IRIS <- c(
  '<a href="http://www.wikidata.org/entity/Q178266">wde:Q178266</a>',
  '<a href="http://www.wikidata.org/entity/Q2397">wde:Q2397</a>',
  '<a href="http://purl.uniprot.org/core/U21">up:U21</a>',
  '<a href="http://purl.uniprot.org/core/U27">up:U27</a>',
  '<a href="http://www.w3.org/W15">w3:W15</a>',
  '<a href="http://www.w3.org/1999/02/22-rdf-syntax-ns#type">rdf:type</a>',
  '<a href="http://www.w3.org/2000/01/rdf-schema#S758">rdfs:S758</a>'
)

LONG_LITERALS <- c(
  "This is http://www.wikidata.org/entity/Q178266",
  "Another http://www.wikidata.org/entity/Q2397 string",
  "http://purl.uniprot.org/core/U21",
  "http://purl.uniprot.org/core/U27 with a suffix",
  "This <http://www.w3.org/2000/01/rdf-schema#S758>",
  "A w3 example: <http://www.w3.org/W15>",
  "Here is an http://www.w3.org/1999/02/22-rdf-syntax-ns#type example"
)
SHORT_LITERALS <- c(
  "This is wde:Q178266",
  "Another wde:Q2397 string",
  "up:U21",
  "up:U27 with a suffix",
  "This rdfs:S758",
  "A w3 example: w3:W15",
  "Here is an rdf:type example"
)
MARKDOWN_LITERALS <- c(
  "This is [wde:Q178266](http://www.wikidata.org/entity/Q178266)",
  "Another [wde:Q2397](http://www.wikidata.org/entity/Q2397) string",
  "[up:U21](http://purl.uniprot.org/core/U21)",
  "[up:U27](http://purl.uniprot.org/core/U27) with a suffix",
  "This [rdfs:S758](http://www.w3.org/2000/01/rdf-schema#S758)",
  "A w3 example: [w3:W15](http://www.w3.org/W15)",
  "Here is an [rdf:type](http://www.w3.org/1999/02/22-rdf-syntax-ns#type) example"
)

HTML_LITERALS <- c(
  'This is <a href="http://www.wikidata.org/entity/Q178266">wde:Q178266</a>',
  'Another <a href="http://www.wikidata.org/entity/Q2397">wde:Q2397</a> string',
  '<a href="http://purl.uniprot.org/core/U21">up:U21</a>',
  '<a href="http://purl.uniprot.org/core/U27">up:U27</a> with a suffix',
  'This <a href="http://www.w3.org/2000/01/rdf-schema#S758">rdfs:S758</a>',
  'A w3 example: <a href="http://www.w3.org/W15">w3:W15</a>',
  'Here is an <a href="http://www.w3.org/1999/02/22-rdf-syntax-ns#type">rdf:type</a> example'
)

TEST_INPUT <- tibble::tibble(
  iri_1 = LONG_IRIS,
  iri_2 = LONG_IRIS,
  iri_3 = LONG_IRIS,
  literals = LONG_LITERALS,
)

PREFIXES <- tibble::tibble(
  short = c("up", "w3", "rdf", "rdfs", "wde"),
  long  = c(
    "http://purl.uniprot.org/core/",
    "http://www.w3.org/",
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "http://www.w3.org/2000/01/rdf-schema#",
    "http://www.wikidata.org/entity/"
  )
)


# Helper function for tests.
check_iri_conversion <- function(iri_style, expected_iris, expected_literals) {

  # Test default conversion, which excludes literals.
  result <- sparqlr::modify_iri_style(TEST_INPUT, PREFIXES, iri_style = iri_style)
  testthat::expect_identical(result$iri_1, expected_iris)
  testthat::expect_identical(result$iri_2, expected_iris)
  testthat::expect_identical(result$iri_3, expected_iris)
  testthat::expect_identical(result$literals, LONG_LITERALS)

  # Test that form conversion also works on literals.
  result <- sparqlr::modify_iri_style(
    TEST_INPUT,
    PREFIXES,
    iri_style = iri_style,
    replace_in_literal = TRUE
  )
  testthat::expect_identical(result$iri_1, expected_iris)
  testthat::expect_identical(result$iri_2, expected_iris)
  testthat::expect_identical(result$iri_3, expected_iris)
  testthat::expect_identical(result$literals, expected_literals)

}

test_that("Long IRIs are correctly converted to short form", {
  check_iri_conversion("short", SHORT_IRIS, SHORT_LITERALS)
})

test_that("Long IRIs are correctly converted to markdown link form", {
  check_iri_conversion("mdlink", MARKDOWN_IRIS, MARKDOWN_LITERALS)
})

test_that("Long IRIs are correctly converted to html form", {
  check_iri_conversion("html", HTML_IRIS, HTML_LITERALS)
})

test_that("long IRIs remain unchanged when converted to long form", {
  check_iri_conversion("long", LONG_IRIS, LONG_LITERALS)
})

test_that(
  "Conversion is done on a per-column basis when multiple values are\
  passed for 'iri_style'.",
  {
    # Multiple styles should be applied on a per-column basis.
    result <- modify_iri_style(
      TEST_INPUT,
      PREFIXES,
      iri_style = c("short", "mdlink", "html", "short"),
      replace_in_literal = TRUE
    )
    expect_identical(result$iri_1, SHORT_IRIS)
    expect_identical(result$iri_2, MARKDOWN_IRIS)
    expect_identical(result$iri_3, HTML_IRIS)
    expect_identical(result$literals, SHORT_LITERALS)

    # Values in iri_style should be re-cycled.
    result <- modify_iri_style(
      TEST_INPUT,
      PREFIXES,
      iri_style = c("short", "mdlink"),
      replace_in_literal = TRUE
    )
    expect_identical(result$iri_1, SHORT_IRIS)
    expect_identical(result$iri_2, MARKDOWN_IRIS)
    expect_identical(result$iri_3, SHORT_IRIS)
    expect_identical(result$literals, MARKDOWN_LITERALS)
  }
)

test_that(
  "modify_iri_style returns unchanged tibble when prefixes is empty",
  {
    result <- modify_iri_style(
      TEST_INPUT,
      prefixes = tibble::tibble(short = character(), long = character()),
      iri_style = "short"
    )
    expect_equal(result, TEST_INPUT)
  }
)


test_that("modify_iri_style prioritizes longer prefixes", {

  # Longer prefixes should be matched first to avoid partial replacements.
  prefixes <- tibble::tibble(
    short = c("ex", "example"),
    long = c("http://example.org/", "http://example.org/ns#")
  )
  input_tibble <- tibble::tibble(
    term = "<http://example.org/ns#term>"
  )
  result <- modify_iri_style(input_tibble, prefixes, iri_style = "short")

  # Should match the longer prefix first.
  expect_equal(result$term, "example:term")
})
