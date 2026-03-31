QUERY_MULTIPLE_PREFIXES <- "test_query_multiple_prefixes.rq"
QUERY_NO_PREFIXES <- "test_query_no_prefixes.rq"
QUERY_SINGLE_PREFIX <- "test_query_single_prefix.rq"
QUERY_PREFIXES_NOT_GROUPED <- "test_query_prefixes_not_grouped.rq"
QUERY_DUPLICATED_PREFIXES <- "test_query_duplicated_prefixes.rq"

EXPECTED_PREFIXES_SHORT <- c("owl", "rdf", "rdfs", "wd", "wdt")
EXPECTED_PREFIXES_LONG <- c(
  "http://www.w3.org/2002/07/owl#",
  "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  "http://www.w3.org/2000/01/rdf-schema#",
  "http://www.wikidata.org/entity/",
  "http://www.wikidata.org/prop/direct/"
)


test_that("function extracts multiple prefixes correctly", {
  file_path <- test_path("fixtures", QUERY_MULTIPLE_PREFIXES)
  prefixes <- load_prefixes_from_file(file_path)

  # Verify that the returned value is a tibble with the correct structure.
  expect_s3_class(prefixes, "tbl_df")
  expect_equal(colnames(prefixes), c("short", "long"))
  expect_equal(nrow(prefixes), 5)

  # Verify that the returned tibble has the expected content.
  expect_equal(prefixes$short, EXPECTED_PREFIXES_SHORT)
  expect_equal(prefixes$long, EXPECTED_PREFIXES_LONG)
})


test_that("function returns empty tibble when query has no prefixes", {
  file_path <- test_path("fixtures", QUERY_NO_PREFIXES)
  prefixes <- load_prefixes_from_file(file_path)

  # Verify that the returned value is a tibble with 0 rows.
  expect_s3_class(prefixes, "tbl_df")
  expect_equal(colnames(prefixes), c("short", "long"))
  expect_equal(nrow(prefixes), 0)
})


test_that("function handles single prefix", {
  file_path <- test_path("fixtures", QUERY_SINGLE_PREFIX)
  prefixes <- load_prefixes_from_file(file_path)

  # Verify that the returned value is a tibble with 1 single rows.
  expect_s3_class(prefixes, "tbl_df")
  expect_equal(nrow(prefixes), 1)
  expect_equal(prefixes$short[1], "wdt")
  expect_equal(prefixes$long[1], "http://www.wikidata.org/prop/direct/")
})


test_that("function extracts multiple prefixes correctly", {
  file_path <- test_path("fixtures", QUERY_PREFIXES_NOT_GROUPED)
  prefixes <- load_prefixes_from_file(file_path)

  # Verify that the returned value is a tibble with the correct structure.
  expect_s3_class(prefixes, "tbl_df")
  expect_equal(colnames(prefixes), c("short", "long"))
  expect_equal(nrow(prefixes), 5)

  # Verify that the returned tibble has the expected content.
  expect_equal(prefixes$short, EXPECTED_PREFIXES_SHORT)
  expect_equal(prefixes$long, EXPECTED_PREFIXES_LONG)
})


test_that("function loads file with duplicated prefixes correctly", {
  file_path <- test_path("fixtures", QUERY_DUPLICATED_PREFIXES)
  prefixes <- load_prefixes_from_file(file_path)

  # Verify that the returned value is a tibble with the correct structure.
  expect_s3_class(prefixes, "tbl_df")
  expect_equal(colnames(prefixes), c("short", "long"))
  expect_equal(nrow(prefixes), 5)

  # Verify that the returned tibble has the expected content.
  expect_equal(prefixes$short, EXPECTED_PREFIXES_SHORT)
  expect_equal(prefixes$long, EXPECTED_PREFIXES_LONG)
})
