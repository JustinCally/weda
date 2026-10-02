dq_fixture <- function(multiples = NA, ...) {
  ops <- read_camtrap_csv_operation(readr::read_csv(system.file("dummydata/operationdata.csv", package = "weda"),
                                                    show_col_types = FALSE))
  recs <- data.frame(
    SiteID = as.character(ops$SiteID), SubStation = ops$SubStation, Iteration = 1L,
    scientific_name = "Macropus giganteus", common_name = "Eastern Grey Kangaroo",
    DateTimeOriginal = as.POSIXct(ops$DateDeploy + 1), Date = ops$DateDeploy + 1, Time = "00:00:00",
    delta.time.secs = 0, delta.time.mins = 0, delta.time.hours = 0, delta.time.days = 0,
    Directory = "dir", FileName = paste0("img", seq_len(nrow(ops)), ".JPG"), n_images = 1,
    HierarchicalSubject = NA, metadata_Multiples = multiples, metadata_Distance = NA,
    metadata_Individuals = NA, metadata_Behaviour = NA, metadata_Species = "Macropus giganteus"
  )
  proj <- data.frame(ProjectName = "Test Project", ProjectShortName = "test_project", DistanceSampling = FALSE,
                     TerrestrialArboreal = "Terrestrial", AllSpeciesTagged = TRUE, DistanceForAllSpecies = FALSE,
                     ProjectDescription = "Test", ProjectLeader = "Test Person")
  suppressMessages(camera_trap_dq(recs, ops, proj, ...))
}

test_that("every data quality step has a brief naming its column", {
  dq <- dq_fixture()
  for (agent in dq) {
    v <- agent$validation_set
    expect_false(any(is.na(v$brief) | v$brief == ""))
    # Multi-column steps get one brief per column, prefixed with the column name
    prefixed <- startsWith(v$brief, "'")
    first_col <- vapply(v$column, function(x) x[[1]][1], character(1))
    expect_true(all(startsWith(v$brief[prefixed], paste0("'", first_col[prefixed], "'"))))
  }
})

test_that("blank multiples pass and non-integer multiples stop", {
  dq <- dq_fixture()
  expect_true(all(vapply(dq, function(a) !any(a$validation_set$stop, na.rm = TRUE), logical(1))))

  v <- dq_fixture(multiples = c("3+", "2", "2.5", NA, "1", "4"))$camtrap_records$validation_set
  stopped <- v[v$stop %in% TRUE, ]
  expect_equal(stopped$label, "metadata_Multiples must be a whole number (or left blank)")
  expect_equal(stopped$n_failed, 2)
})

test_that("the app's Step 8 messages don't include pointblank's per-step log", {
  # progress = TRUE is what an interactive session (e.g. RStudio) gets by default
  logged <- cli::cli_fmt(dq_fixture(progress = TRUE))
  quiet <- cli::cli_fmt(dq_fixture(progress = FALSE))
  expect_true(any(grepl("Interrogation Started", logged)))
  expect_false(any(grepl("Interrogation|Step [0-9]+: OK", quiet)))
})
