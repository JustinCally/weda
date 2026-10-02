mock_catalog <- function(statement, params) {
  schema <- params[[1]]
  if (grepl("SHOW search_path", statement)) return(data.frame(search_path = '"$user", public'))
  if (grepl("relkind IN \\('r', 'p'\\)", statement)) {
    return(data.frame(name = c("raw_camtrap_records", "raw_project_information", "vba_name_conversions", "shiny_hide_table")))
  }
  if (grepl("pg_get_viewdef", statement)) {
    return(data.frame(
      name = c("processed_presence_absence", "curated_camtrap_records", "curated_project_information"),
      kind = c("m", "m", "v"),
      definition = c(" SELECT r.\"SiteID\" FROM camtrap.curated_camtrap_records r JOIN camtrap.vba_name_conversions v USING (common_name);",
                     " SELECT * FROM camtrap.raw_camtrap_records;",
                     " SELECT * FROM \"camtrap\".raw_project_information;")))
  }
  if (grepl("pg_depend", statement)) {
    return(data.frame(dependent = "processed_presence_absence", source = "curated_camtrap_records"))
  }
  if (grepl("pg_indexes", statement)) {
    return(data.frame(tablename = "curated_camtrap_records", indexname = "crr_id",
                      indexdef = "CREATE UNIQUE INDEX crr_id ON camtrap.curated_camtrap_records USING btree (\"camtrap_record_database_ID\")"))
  }
  if (grepl("pg_get_functiondef", statement)) {
    return(data.frame(name = "refresh_curated_camtrap_records_recent",
                      definition = "CREATE OR REPLACE FUNCTION camtrap.refresh_curated_camtrap_records_recent()\n RETURNS void\n LANGUAGE sql\nAS $function$ REFRESH MATERIALIZED VIEW CONCURRENTLY camtrap.curated_camtrap_records $function$\n"))
  }
  # Objects already in the target: only the raw tables
  if (schema == "camtrap_dev" && grepl("FROM pg_class", statement)) {
    return(data.frame(name = c("raw_camtrap_records", "raw_project_information")))
  }
  data.frame(name = character(0))
}

test_that("mirror_schema_structure generates renamed, ordered SQL without touching data", {
  executed <- character(0)
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, params = NULL, ...) mock_catalog(statement, params),
    dbExecute = function(conn, statement, ...) { executed <<- c(executed, statement); 0 },
    .package = "DBI"
  )
  sql_file <- tempfile(fileext = ".sql")

  sql <- suppressMessages(mirror_schema_structure("mock-con", sql_file = sql_file))

  # Dry run: only the session search_path is changed and restored
  expect_equal(executed, c("SET search_path TO pg_catalog", 'SET search_path TO "$user", public'))
  expect_true(file.exists(sql_file))

  # Existing raw tables are skipped; missing ones created (VBA list copied, hide table empty)
  expect_false(any(grepl("CREATE TABLE camtrap_dev.raw_camtrap_records", sql)))
  expect_true("INSERT INTO camtrap_dev.vba_name_conversions SELECT * FROM camtrap.vba_name_conversions;" %in% sql)
  expect_false(any(grepl("INSERT INTO camtrap_dev.shiny_hide_table", sql)))

  # Views are renamed and dependencies come first
  idx <- function(pattern) which(grepl(pattern, sql))
  expect_lt(idx("MATERIALIZED VIEW camtrap_dev.curated_camtrap_records"),
            idx("MATERIALIZED VIEW camtrap_dev.processed_presence_absence"))
  expect_lt(idx("MATERIALIZED VIEW camtrap_dev.curated_camtrap_records"), idx("INDEX IF NOT EXISTS crr_id"))
  expect_lt(idx("INDEX IF NOT EXISTS crr_id"), idx("FUNCTION camtrap_dev.refresh_"))

  # Nothing except the table copies still points at production
  definitions <- sql[!grepl("^(CREATE TABLE|INSERT INTO)", sql)]
  expect_false(any(grepl("\\bcamtrap\\b", definitions, perl = TRUE)))
  expect_true(any(grepl("JOIN camtrap_dev.vba_name_conversions", sql)))
  expect_true(any(grepl("FROM camtrap_dev.raw_project_information", sql)))
})

test_that("mirror_schema_structure only writes to the database when apply = TRUE", {
  in_transaction <- FALSE
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, params = NULL, ...) mock_catalog(statement, params),
    dbExecute = function(conn, statement, ...) 0,
    dbWithTransaction = function(conn, code, ...) { in_transaction <<- TRUE; code },
    .package = "DBI"
  )
  suppressMessages(mirror_schema_structure("mock-con", sql_file = NULL))
  expect_false(in_transaction)
  suppressMessages(mirror_schema_structure("mock-con", sql_file = NULL, apply = TRUE))
  expect_true(in_transaction)

  expect_error(mirror_schema_structure("mock-con", to = "camtrap"), "different")
  expect_error(mirror_schema_structure("mock-con", to = "x; DROP"), "Invalid schema")
})

test_that("order_by_dependency handles chains and cycles", {
  deps <- data.frame(dependent = c("c", "b"), source = c("b", "a"))
  expect_equal(order_by_dependency(c("c", "b", "a", "d"), deps), c("a", "d", "b", "c"))
  expect_error(order_by_dependency(c("a", "b"), data.frame(dependent = c("a", "b"), source = c("b", "a"))), "Circular")
})
