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

  converted_data <- standardise_species_names(raw_camtrap_records %>%
                                              dplyr::mutate(Species = dplyr::case_when(Species == "Rusa unicolor" ~ "Cervus unicolor",
                                                                               TRUE ~ Species)),
                                                  format = "scientific",
                                                  speciesCol = "Species",
                                                  return_data = TRUE)

  expect_true(c("scientific_name") %in% colnames(converted_data))
  expect_true(c("common_name") %in% colnames(converted_data))

})

test_that("glider tags are remapped to the correct VBA taxa", {
  common <- suppressWarnings(standardise_species_names(
    data.frame(Species = c("Greater Glider", "Feathertail Glider", "Sugar Glider")),
    format = "common", speciesCol = "Species"))
  ids <- weda::vba_name_conversions$taxon_id[match(common$scientific_name, weda::vba_name_conversions$scientific_name)]
  # Sugar Glider is a control: names that aren't overridden pass through unchanged
  expect_equal(common$common_name, c("Southern Greater Glider", "Feather-tailed glider species", "Sugar Glider"))
  expect_equal(ids[1:2], c(11133, 903793))

  sci <- suppressWarnings(standardise_species_names(
    data.frame(Species = c("fam. Pseudocheiridae gen. Petauroides", "fam. Acrobatidae gen. Acrobates")),
    format = "scientific", speciesCol = "Species"))
  expect_equal(sci$common_name, c("Southern Greater Glider", "Feather-tailed glider species"))
})
