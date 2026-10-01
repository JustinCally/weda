skip_if_not_installed("RSQLite")

# In-memory SQLite database with an attached 'camtrap' schema standing in for
# the project table on the server
local_project_db <- function(env = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = env)
  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS camtrap")
  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "raw_project_information"),
                    data.frame(ProjectShortName = c("gg_survey_2023", "gg_survey_2023", "fox_monitoring"),
                               ProjectName = c("Greater Glider Survey 2023", "Greater Glider Survey 2023", "Fox Monitoring Program")))
  con
}

project <- function(short, full) data.frame(ProjectShortName = short, ProjectName = full)

test_that("project names are checked against existing projects", {
  con <- local_project_db()

  expect_true(suppressMessages(check_project_names(project("new_project", "A New Project"), con)))
  expect_true(suppressMessages(check_project_names(project("gg_survey_2023", "Greater Glider Survey 2023"), con)))

  # Short name exists with a different full name, and vice versa
  expect_false(suppressMessages(check_project_names(project("gg_survey_2023", "Greater Glider Survey"), con)))
  expect_false(suppressMessages(check_project_names(project("fox_2024", "Fox Monitoring Program"), con)))

  msgs <- cli::cli_fmt(check_project_names(project("gg_survey_2023", "Greater Glider Survey"), con))
  msgs <- gsub("\\s+", " ", paste(msgs, collapse = " "))
  expect_match(msgs, "already exists with ProjectName 'Greater Glider Survey 2023'", fixed = TRUE)
})

test_that("project check is skipped without a usable connection", {
  expect_true(suppressMessages(check_project_names(project("a", "b"), con = NULL)))

  con <- local_project_db()
  msgs <- cli::cli_fmt(res <- check_project_names(project("a", "b"), con, schema = "camtrap_dev"))
  expect_true(res)
  expect_true(any(grepl("Could not check project names", msgs)))
})
