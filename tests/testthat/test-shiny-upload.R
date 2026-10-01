test_that("Step 9 retry only uploads tables that failed previously", {
  uploads <- character(0)
  fail_operation <- TRUE

  testthat::local_mocked_bindings(
    prepare_camtrap_upload = function(agent_list) list(project_information = data.frame(ProjectShortName = "TEST")),
    upload_camtrap_data = function(con, data_list, uploadername, tables_to_upload, ...) {
      if (tables_to_upload == "raw_camtrap_operation" && fail_operation) {
        fail_operation <<- FALSE
        stop("server closed the connection unexpectedly")
      }
      uploads <<- c(uploads, tables_to_upload)
    }
  )
  testthat::local_mocked_bindings(
    dbIsValid = function(dbObj, ...) TRUE,
    dbGetQuery = function(conn, statement, ...) data.frame(x = 1),
    .package = "DBI"
  )

  shiny::testServer(dataUploadServer, args = list(con = "mock-con"), {
    dqlist(list(result = list(camtrap_records = "agent")))
    session$flushReact()

    # First attempt: records succeed, operation fails, session keeps running
    run_upload("Test Person")
    expect_equal(uploads, "raw_camtrap_records")
    expect_equal(uploaded_tables(), "raw_camtrap_records")

    # Retry: records are not uploaded a second time
    run_upload("Test Person")
    expect_equal(uploads, c("raw_camtrap_records", "raw_camtrap_operation", "raw_project_information"))
  })
})

test_that("Step 9 does not upload when the database is unreachable", {
  uploads <- character(0)
  testthat::local_mocked_bindings(
    prepare_camtrap_upload = function(agent_list) list(),
    upload_camtrap_data = function(...) uploads <<- c(uploads, "called")
  )

  shiny::testServer(dataUploadServer, args = list(con = NULL), {
    dqlist(list(result = list(camtrap_records = "agent")))
    session$flushReact()
    run_upload("Test Person")
    expect_length(uploads, 0)
  })
})
