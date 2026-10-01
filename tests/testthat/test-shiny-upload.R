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

test_that("Step 9 uploads to the schema selected in the dev toggle", {
  schemas <- character(0)
  testthat::local_mocked_bindings(
    prepare_camtrap_upload = function(agent_list) list(),
    upload_camtrap_data = function(con, data_list, uploadername, tables_to_upload, schema, ...) schemas <<- c(schemas, schema)
  )
  testthat::local_mocked_bindings(
    dbIsValid = function(dbObj, ...) TRUE,
    dbGetQuery = function(conn, statement, ...) data.frame(x = 1),
    .package = "DBI"
  )

  shiny::testServer(dataUploadServer, args = list(con = "mock-con"), {
    expect_null(output$devbanner$html)
    session$setInputs(target_schema = "camtrap_dev")
    expect_match(output$devbanner$html, "camtrap_dev")

    dqlist(list(result = list(camtrap_records = "agent")))
    session$flushReact()
    run_upload("Test Person", schema = input$target_schema)
    expect_equal(unique(schemas), "camtrap_dev")
  })
})

test_that("upload_camtrap_data writes and refreshes only in the target schema", {
  sql <- character(0)
  written <- list()
  testthat::local_mocked_bindings(
    dbWriteTable = function(conn, name, value, ...) written[[length(written) + 1]] <<- name,
    dbExecute = function(conn, statement, ...) sql <<- c(sql, statement),
    .package = "DBI"
  )
  data_list <- list(camtrap_records = data.frame(a = 1),
                    camtrap_operation = data.frame(a = 1),
                    project_information = data.frame(a = 1))

  suppressMessages(upload_camtrap_data(con = "mock-con", data_list = data_list,
                                       uploadername = "Test Person", schema = "camtrap_dev"))

  expect_true(all(vapply(written, function(x) x@name[["schema"]], character(1)) == "camtrap_dev"))
  expect_length(sql, 5)
  expect_true(all(startsWith(sql, "SELECT camtrap_dev.refresh_")))
  expect_error(upload_camtrap_data(con = "mock-con", data_list = data_list,
                                   uploadername = "x", schema = "camtrap; DROP TABLE x"),
               "Invalid schema")
})
