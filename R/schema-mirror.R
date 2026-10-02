#' Mirror a schema's structure (without data) into another schema
#'
#' @description Recreates the views, materialized views, indexes and functions of
#' one schema (e.g. `camtrap`) in another (e.g. `camtrap_dev`) so the development
#' schema behaves like production without copying production data. Definitions are
#' read from the PostgreSQL catalog, so they match what is actually on the server.
#'
#' Base tables missing from the target are created empty (`LIKE ... INCLUDING ALL`),
#' except tables in `copy_data_tables` (reference data such as the VBA name list),
#' which are also copied. Objects that already exist in the target are skipped.
#' Materialized views are built from the target schema's own tables.
#'
#' By default nothing is changed on the database: the SQL is written to `sql_file`
#' for review. Set `apply = TRUE` to run it in a single transaction.
#'
#' @param con database connection
#' @param from schema to copy the structure from
#' @param to schema to create the structure in
#' @param sql_file file to write the generated SQL to (NULL to skip)
#' @param apply logical, run the SQL on the database (default FALSE)
#' @param copy_data_tables base tables whose data should also be copied when created
#'
#' @return generated SQL statements (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
#' username = "psql_user"))
#' # Review the SQL first
#' mirror_schema_structure(con, from = "camtrap", to = "camtrap_dev")
#' # Then apply it
#' mirror_schema_structure(con, from = "camtrap", to = "camtrap_dev", apply = TRUE)
#' }
mirror_schema_structure <- function(con,
                                    from = "camtrap",
                                    to = "camtrap_dev",
                                    sql_file = paste0(to, "_setup.sql"),
                                    apply = FALSE,
                                    copy_data_tables = "vba_name_conversions") {

  for (s in c(from, to)) {
    if (!grepl("^[a-z_][a-z0-9_]*$", s)) stop("Invalid schema name: ", s)
  }
  if (from == to) stop("'from' and 'to' must be different schemas")

  # Without the source schema on the search_path, pg_get_viewdef() schema-qualifies
  # every relation, so the definitions can be safely renamed to the target schema
  old_path <- DBI::dbGetQuery(con, "SHOW search_path")[[1]]
  DBI::dbExecute(con, "SET search_path TO pg_catalog")
  on.exit(DBI::dbExecute(con, paste("SET search_path TO", old_path)), add = TRUE)

  q <- function(sql, schema) DBI::dbGetQuery(con, sql, params = list(schema))

  existing <- q("SELECT c.relname AS name FROM pg_class c
                 JOIN pg_namespace n ON n.oid = c.relnamespace
                 WHERE n.nspname = $1", to)$name
  existing_fns <- q("SELECT p.proname AS name FROM pg_proc p
                     JOIN pg_namespace n ON n.oid = p.pronamespace
                     WHERE n.nspname = $1", to)$name

  tables <- q("SELECT c.relname AS name FROM pg_class c
               JOIN pg_namespace n ON n.oid = c.relnamespace
               WHERE n.nspname = $1 AND c.relkind IN ('r', 'p')", from)$name
  views <- q("SELECT c.relname AS name, c.relkind AS kind, pg_get_viewdef(c.oid) AS definition
              FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
              WHERE n.nspname = $1 AND c.relkind IN ('v', 'm')", from)
  deps <- q("SELECT DISTINCT dep.relname AS dependent, src.relname AS source
             FROM pg_depend d
             JOIN pg_rewrite r ON d.objid = r.oid
             JOIN pg_class dep ON r.ev_class = dep.oid
             JOIN pg_class src ON d.refobjid = src.oid
             JOIN pg_namespace n ON src.relnamespace = n.oid
             WHERE n.nspname = $1 AND dep.oid <> src.oid", from)
  indexes <- q("SELECT tablename, indexname, indexdef FROM pg_indexes WHERE schemaname = $1", from)
  functions <- q("SELECT p.proname AS name, pg_get_functiondef(p.oid) AS definition
                  FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
                  WHERE n.nspname = $1 AND p.prokind = 'f'", from)

  rename <- function(x) gsub(paste0('(?<![\\w"])"?', from, '"?\\.'), paste0(to, "."), x, perl = TRUE)
  skipped <- character(0)

  sql <- c(paste0("CREATE SCHEMA IF NOT EXISTS ", to, ";"),
           # Any unqualified names resolve to the target schema, never the source
           paste0("SET LOCAL search_path TO ", to, ", public;"))

  # Base tables (structure only, except reference data)
  for (tbl in tables) {
    if (tbl %in% existing) { skipped <- c(skipped, tbl); next }
    sql <- c(sql, paste0("CREATE TABLE ", to, ".", tbl, " (LIKE ", from, ".", tbl, " INCLUDING ALL);"))
    if (tbl %in% copy_data_tables) {
      sql <- c(sql, paste0("INSERT INTO ", to, ".", tbl, " SELECT * FROM ", from, ".", tbl, ";"))
    }
  }

  # Views and materialized views, dependencies first
  created_views <- character(0)
  for (v in order_by_dependency(views$name, deps)) {
    if (v %in% existing) { skipped <- c(skipped, v); next }
    row <- views[views$name == v, ]
    def <- sub(";\\s*$", "", trimws(rename(row$definition)))
    sql <- c(sql, if (row$kind == "m") {
      paste0("CREATE MATERIALIZED VIEW ", to, ".", v, " AS\n", def, "\nWITH DATA;")
    } else {
      paste0("CREATE VIEW ", to, ".", v, " AS\n", def, ";")
    })
    created_views <- c(created_views, v)
  }

  # Indexes on new materialized views (needed for REFRESH ... CONCURRENTLY)
  mv_indexes <- indexes[indexes$tablename %in% intersect(created_views, views$name[views$kind == "m"]), ]
  for (i in seq_len(nrow(mv_indexes))) {
    sql <- c(sql, paste0(sub("^CREATE (UNIQUE )?INDEX ", "CREATE \\1INDEX IF NOT EXISTS ",
                             rename(mv_indexes$indexdef[i])), ";"))
  }

  # Functions (e.g. refresh_*), last so the objects they use exist
  for (i in seq_len(nrow(functions))) {
    if (functions$name[i] %in% existing_fns) { skipped <- c(skipped, paste0(functions$name[i], "()")); next }
    sql <- c(sql, paste0(trimws(rename(functions$definition[i])), ";"))
  }

  # Flag renamed definitions still mentioning the source schema (e.g. inside
  # quoted strings); table creation/copy statements read from it by design
  copies_source <- grepl("^(CREATE TABLE|INSERT INTO) ", sql)
  leftover <- !copies_source & grepl(paste0("\\b", from, "\\b"), sql, perl = TRUE)
  if (any(leftover)) {
    warning(sum(leftover), " statement(s) still reference '", from,
            "' after renaming; review them in the SQL file before applying")
  }

  if (length(skipped) > 0) {
    message("Already in ", to, " (skipped): ", paste(skipped, collapse = ", "))
  }

  if (!is.null(sql_file)) {
    writeLines(c(paste0("-- Generated by weda::mirror_schema_structure() on ", Sys.time()),
                 paste0("-- Copies the structure of '", from, "' into '", to, "'"),
                 "BEGIN;", sql, "COMMIT;"),
               sql_file, sep = "\n\n")
    message("SQL written to ", sql_file, " (", length(sql), " statements)")
  }

  if (apply) {
    DBI::dbWithTransaction(con, for (stmt in sql) DBI::dbExecute(con, stmt))
    message("Applied ", length(sql), " statements to ", to)
  } else {
    message("Nothing has been changed on the database. Review the SQL, then re-run with apply = TRUE")
  }

  invisible(sql)
}

#' Order views so each comes after the views it depends on
#'
#' @param names view names
#' @param deps data.frame with `dependent` and `source` columns
#'
#' @noRd
#'
#' @return ordered view names
order_by_dependency <- function(names, deps) {
  deps <- deps[deps$dependent %in% names & deps$source %in% names, ]
  ordered <- character(0)
  remaining <- names
  while (length(remaining) > 0) {
    ready <- remaining[!remaining %in% deps$dependent[!deps$source %in% ordered]]
    if (length(ready) == 0) stop("Circular view dependencies: ", paste(remaining, collapse = ", "))
    ordered <- c(ordered, sort(ready))
    remaining <- setdiff(remaining, ready)
  }
  ordered
}
