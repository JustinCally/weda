skip_if_not_installed("RSQLite")

# Three projects: species detected at some cameras, a project where the species
# was never recorded, and a site with substations
local_camtrap_db <- function(no_substation = NA, env = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = env)
  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS camtrap")

  operation <- data.frame(
    ProjectShortName = c("p1", "p1", "p1", "p2", "p2", "p3", "p3"),
    SiteID = c("A", "B", "C", "D", "D", "E", "F"),
    SubStation = c(no_substation, no_substation, no_substation, "1", "2", no_substation, no_substation),
    Iteration = 1L
  )
  records <- data.frame(
    ProjectShortName = c("p1", "p1", "p1", "p2", "p2", "p3"),
    SiteID = c("A", "A", "B", "D", "D", "E"),
    SubStation = c(no_substation, no_substation, no_substation, "1", "2", no_substation),
    Iteration = 1L,
    scientific_name = c("Wallabia bicolor", "Vulpes vulpes", "Notamacropus irma", "Wallabia bicolor", "Vulpes vulpes", "Vulpes vulpes"),
    common_name = c("Black Wallaby", "Red Fox", "Black-tailed Wallaby", "Black Wallaby", "Red Fox", "Red Fox")
  )
  # The precomputed presence-absence table is defined by this function for all
  # species. SQLite's paste() returns NULL for a missing SubStation (Postgres'
  # CONCAT_WS doesn't), so build it with a placeholder SubStation, then restore NA
  placeholder <- function(x) ifelse(is.na(x), "<none>", x)
  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "curated_camtrap_operation"),
                    dplyr::mutate(operation, SubStation = placeholder(SubStation)))
  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "curated_camtrap_records"),
                    dplyr::mutate(records, SubStation = placeholder(SubStation)))
  pa <- processed_SubStation_presence_absence(con, return_data = TRUE) %>%
    dplyr::mutate(SubStation = dplyr::na_if(SubStation, "<none>"))
  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "processed_site_substation_presence_absence"), pa)

  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "curated_camtrap_operation"), operation, overwrite = TRUE)
  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "curated_camtrap_records"), records, overwrite = TRUE)
  list(con = con, operation = operation)
}

test_that("fast presence matches processed_SubStation_presence_absence() for one species", {
  # The old query keys cameras with paste(SiteID, SubStation), which SQLite (unlike
  # Postgres' CONCAT_WS) turns into NULL for a missing SubStation, so compare
  # using cameras that all have a SubStation; missing ones are tested below
  db <- local_camtrap_db(no_substation = "0")
  keys <- c("ProjectShortName", "SiteID", "SubStation", "Iteration")

  for (sp in c("Black Wallaby", "Red Fox", "Black-tailed Wallaby")) {
    # How the map previously coloured markers
    old <- db$operation %>%
      dplyr::left_join(processed_SubStation_presence_absence(db$con, return_data = TRUE, species = sp) %>%
                         dplyr::mutate(Iteration = as.integer(Iteration)),
                       by = keys) %>%
      dplyr::arrange(ProjectShortName, SiteID, SubStation)

    new <- add_species_presence(db$operation, species_detections(db$con, sp)) %>%
      dplyr::arrange(ProjectShortName, SiteID, SubStation)

    expect_equal(new$Presence, old$Presence, label = paste("Presence for", sp))
  }
})

test_that("presence is 1 if any selected species was detected", {
  db <- local_camtrap_db()
  res <- add_species_presence(db$operation, species_detections(db$con, c("Black Wallaby", "Black-tailed Wallaby")))
  res <- res[order(res$ProjectShortName, res$SiteID, res$SubStation), ]

  # p1: A (Black Wallaby), B (Black-tailed), C neither; p2: D1 detected, D2 not; p3: never recorded
  expect_equal(res$Presence, c(1, 1, 0, 1, 0, NA, NA))
  expect_equal(res$species_detected[1:2], c("Black Wallaby", "Black-tailed Wallaby"))
})

test_that("species never recorded gives NA presence everywhere", {
  db <- local_camtrap_db()
  res <- add_species_presence(db$operation, species_detections(db$con, "Koala"))
  expect_true(all(is.na(res$Presence)))
})
