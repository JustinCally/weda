raw_camtrap_records <- camtrapR::recordTable(inDir  = system.file("dummydata/images", package = "weda"),
                                   IDfrom = "metadata",
                                   cameraID = "directory",
                                   stationCol = "SiteID",
                                   camerasIndependent = TRUE,
                                   timeZone = Sys.timezone(location = TRUE),
                                   metadataSpeciesTag = "Species",
                                   removeDuplicateRecords = FALSE,
                                   returnFileNamesMissingTags = TRUE) %>%
  dplyr::rename(SubStation = Camera) %>%
  dplyr::mutate(SubStation = dplyr::case_when(SiteID == SubStation ~ NA_character_,
                                TRUE ~ SubStation),
         Iteration = 1L)

test_that("vba check works", {
  expect_warning({
    standardise_species_names(raw_camtrap_records,
                              format = "scientific",
                              speciesCol = "Species",
                              return_data = FALSE)
  })
})
