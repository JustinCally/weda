## Generates obfuscated dummy transect data for use in examples and vignettes.
##
## The original survey data contained real observer names, business names,
## Proofsafe record IDs, and GPS coordinates that could identify a specific
## survey site. This script creates synthetic replacements that preserve the
## column structure and plausible values while removing identifiable information:
##
##   - Names replaced with generic placeholders
##   - Business name replaced with generic agency name
##   - Proofsafe section/data IDs replaced with small sequential dummy values
##   - Site and file names replaced with generic codes
##   - Date shifted to a generic example date
##   - GPS coordinates shifted by ~1.24 deg lon / 0.28 deg lat to Gippsland
##   - Project name/short name changed to non-identifiable example values

library(tibble)
library(readr)
library(sf)

out_dir <- "inst/dummydata/transectdata"

## ---- region_gg_records.csv --------------------------------------------------
## Two detections: one heard Southern Boobook and one seen Southern Greater
## Glider, both recorded by the same single-observer team on one transect.

records <- tribble(
  ~Business_Id_B, ~Business_Name_B, ~Author_Id_F, ~Author_Name_F,
  ~Form_Id_F, ~Form_Name_F, ~File_Id_F, ~File_Name_F,
  ~Section_Id_F, ~Data_Section_Id_F, ~Parent_Data_Section_Id_F,
  ~Section_Id_921_H1, ~Data_Section_Id_921_H1,
  ~SiteID_H1, ~Transect_H1, ~Date_H1, ~Start_time_H1,
  ~`Observer_H1...18`, ~ObserverOther_H1, ~`Observer_H1...20`,
  ~GPS_H1, ~StatusBurn_H1, ~Access_H1, ~TransectNotes_H1, ~Visibility_H1,
  ~Temp_C_H1, ~MoonPhase_H1, ~Nightlight_H1, ~CloudCover_H1,
  ~Wind_H1, ~Precipitation_H1, ~FlowerIndex_H1,
  ~Section_Id_922_H2, ~Data_Section_Id_922_H2,
  ~`P1 Vis_rank_H2`, ~`P2 Vis_rank_H2`, ~`P3 Vis_rank_H2`,
  ~`P4 Vis_rank_H2`, ~`P5 Vis_rank_H2`, ~`P6 Vis_rank_H2`,
  ~End_time_H2, ~Notes_H2,
  ~Section_Id_923_I3, ~Data_Section_Id_923_I3,
  ~Animal_I3, ~AnimObsTime_I3, ~SeenHeard_I3,
  ~Species_I3, ~Animal_sp_other_I3,
  ~`L or R of trans_I3`, ~`Waypoint no._I3`,
  ~AnimalHeight_I3, ~`Distance to animal_I3`, ~`Bearing to A._I3`,
  ~Dist_F_Transect_I3, ~`Tree species_I3`, ~Tree_sp_other_I3,
  ~SeenX2_I3, ~Comments_I3,

  # Row 1 — Southern Boobook (heard)
  1, "Department of Environment", 1, "ARI Contractor",
  1, NA, 1001, "Example Road S1 N1",
  923, 1001, 0,
  921, 1000,
  "S1", 1, "2023-04-15", "20:00:00",
  "Other", "Jane Smith- Alex Jones", 1,
  "Other", "Control", "Access via main road", NA, "Poor: thick understorey",
  15, 50, "Medium", 40,
  "Light breeze (6-11 km/h)", "No rain", "No trees in flower",
  922, 1000,
  "51-75%", "51-75%", "0-25%", "0-25%", "0-25%", "51-75%",
  "21:30:00", NA,
  923, 1001,
  "01", "20:45:00", "Heard",
  "Southern Boobook", NA,
  "Left", "01",
  NA, 35, 236,
  190, NA, NA,
  "No", "Transect bearing = 335.",

  # Row 2 — Southern Greater Glider (seen)
  1, "Department of Environment", 1, "ARI Contractor",
  1, NA, 1001, "Example Road S1 N1",
  923, 1002, 0,
  921, 1000,
  "S1", 1, "2023-04-15", "20:00:00",
  "Other", "Jane Smith- Alex Jones", 1,
  "Other", "Control", "Access via main road", NA, "Poor: thick understorey",
  15, 50, "Medium", 40,
  "Light breeze (6-11 km/h)", "No rain", "No trees in flower",
  922, 1000,
  "51-75%", "51-75%", "0-25%", "0-25%", "0-25%", "51-75%",
  "21:30:00", NA,
  923, 1002,
  "03", "22:20:00", "Seen",
  "Southern Greater Glider", NA,
  "Right", "03",
  28, 47.11, 52,
  430, "Peppermint sp.", NA,
  NA, "Transect bearing = 356."
)

write_csv(records, file.path(out_dir, "region_gg_records.csv"), na = "NA")

## ---- region_gg_transects.geojson --------------------------------------------
## Single north-south transect line with the same geometry as the original but
## shifted ~1.24 deg east and ~0.28 deg south to central Gippsland (Victoria).
## SiteID updated to match records ("S1").

transects_sf <- st_sf(
  SiteID   = "S1",
  Transect = 1,
  geometry = st_sfc(
    st_linestring(matrix(
      c(146.800436, -37.900942,
        146.799991, -37.896513),
      ncol = 2, byrow = TRUE
    )),
    crs = 4283
  )
)

st_write(transects_sf,
         file.path(out_dir, "region_gg_transects.geojson"),
         driver = "GeoJSON",
         delete_dsn = TRUE)

## ---- region_gg_project.csv --------------------------------------------------

project <- tribble(
  ~ProjectName, ~ProjectShortName,
  ~DistanceSampling, ~TerrestrialArboreal,
  ~AllSpeciesTagged, ~DistanceForAllSpecies,
  ~DiurnalNocturnal, ~ProjectDescription, ~ProjectLeader,

  "Greater Glider Example Survey", "gg_example",
  TRUE, "Arboreal",
  TRUE, FALSE,
  "Nocturnal", "Example project for vignette demonstration", "First Last"
)

write_csv(project, file.path(out_dir, "region_gg_project.csv"), na = "NA")

message("Dummy transect data written to ", out_dir)
