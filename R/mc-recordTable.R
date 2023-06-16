# Taken from Juergen Niedballa
# https://groups.google.com/g/camtrapr/c/iSJjCe88NFM
recordTable.inc <- function(inDir,
                            IDfrom,
                            cameraID,
                            camerasIndependent,
                            includeStation,
                            exclude,
                            minDeltaTime = 0,
                            deltaTimeComparedTo,
                            timeZone,
                            stationCol,
                            writecsv = FALSE,
                            outDir,
                            metadataHierarchyDelimitor = "|",
                            metadataSpeciesTag,
                            additionalMetadataTags,
                            removeDuplicateRecords = TRUE,
                            returnFileNamesMissingTags = FALSE,
                            eventSummaryColumn,
                            eventSummaryFunction,
                            video
)
{

  wd0 <- getwd()
  on.exit(setwd(wd0))

  if(!hasArg(stationCol)) stationCol <- "Station"
  if(!is.character(stationCol)) stop("stationCol must be of class 'character'")
  camtrapR:::checkForSpacesInColumnNames(stationCol = stationCol)

  speciesCol <- "Species"

  if(!is.character(IDfrom)) stop("IDfrom must be of class 'character'")
  IDfrom <- match.arg(IDfrom, choices = c("metadata", "directory"))

  if(IDfrom == "metadata"){
    metadataHierarchyDelimitor <- match.arg(metadataHierarchyDelimitor, choices = c("|", ":"))
    if(!hasArg(metadataSpeciesTag))       stop("'metadataSpeciesTag' must be defined if IDfrom = 'metadata'")
    if(!is.character(metadataSpeciesTag)) stop("metadataSpeciesTag must be of class 'character'")
    if(length(metadataSpeciesTag) != 1)   stop("metadataSpeciesTag must be of length 1")
  }

  multiple_tag_separator <- "_&_"

  # check input
  if(!hasArg(timeZone)) {
    message("timeZone is not specified. Assuming UTC")
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()", call. = FALSE)
  }
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)

  if(hasArg(cameraID)){
    if(!is.character(cameraID))         stop("cameraID must be of class 'character'", call. = FALSE)
    cameraID <- match.arg(cameraID, choices = c("filename", "directory"))
    if(!hasArg(camerasIndependent))     stop("camerasIndependent is not defined. It must be defined if cameraID is defined", call. = FALSE)
    if(!is.logical(camerasIndependent)) stop("camerasIndependent must be of class 'logical'", call. = FALSE)
  } else {
    camerasIndependent <- FALSE
  }

  cameraCol <- "Camera"


  if(hasArg(outDir)){
    if(!is.character(outDir))         stop("outDir must be of class 'character'", call. = FALSE)
    if(isFALSE(file.exists(outDir)))  stop("outDir does not exist", call. = FALSE)
  }

  if(hasArg(exclude)){
    if(!is.character(exclude)) stop("exclude must be of class 'character'", call. = FALSE)
  }

  if(!is.logical(removeDuplicateRecords))     stop("'removeDuplicateRecords' must be logical (TRUE / FALSE)", call. = FALSE)
  if(!is.logical(returnFileNamesMissingTags)) stop("'returnFileNamesMissingTags' must be logical (TRUE / FALSE)", call. = FALSE)


  metadata.tagname <- "HierarchicalSubject"    # for extracting metadata assigned in tagging software

  if(hasArg(additionalMetadataTags)){
    if(!is.character(additionalMetadataTags)) stop("additionalMetadataTags must be of class 'character'", call. = FALSE)
    if(any(grep(pattern = " ", x = additionalMetadataTags, fixed = TRUE))) stop("In argument additionalMetadataTags, spaces are not allowed")
    if("HierarchicalSubject" %in% additionalMetadataTags & IDfrom == "metadata")  {
      message("'HierarchicalSubject' may not be in 'additionalMetadataTags' if IDfrom = 'metadata'. It will be ignored because the function returns it anyway.", call. = FALSE)
      additionalMetadataTags <- additionalMetadataTags[-grep(pattern = "HierarchicalSubject", x = additionalMetadataTags)]  # remove HierarchicalSubject from additionalMetadataTags
    }
  }

  minDeltaTime <- as.integer(minDeltaTime)
  if(!is.integer(minDeltaTime)) stop("'minDeltaTime' must be an integer", call. = FALSE)

  if(minDeltaTime != 0){
    if(isFALSE(removeDuplicateRecords)){
      warning("minDeltaTime is > 0. Therefore, removeDuplicateRecords was set to TRUE (otherwise there may be records taken at the same time)", call. = FALSE, immediate. = TRUE)
      removeDuplicateRecords <- TRUE
    }

    deltaTimeComparedTo < match.arg(deltaTimeComparedTo, choices = c("lastRecord", "lastIndependentRecord"))

    if(!hasArg(deltaTimeComparedTo)) {
      stop(paste("minDeltaTime is not 0. deltaTimeComparedTo must be defined"), call. = FALSE)
    }
  } else {
    if(hasArg(deltaTimeComparedTo)) {
      warning(paste("minDeltaTime is 0. deltaTimeComparedTo = '", deltaTimeComparedTo, "' will have no effect", sep = ""), call. = FALSE, immediate. = TRUE)
    } else {
      deltaTimeComparedTo <- "lastRecord"
    }
  }

  if(!is.logical(writecsv))  stop("writecsv must be logical (TRUE or FALSE)", call. = FALSE)
  if(!is.character(inDir))   stop("inDir must be of class 'character'", call. = FALSE)
  if(length(inDir) != 1)     stop("inDir may only consist of 1 element only", call. = FALSE)
  if(!dir.exists(inDir))     stop("Could not find inDir:\n", inDir, call. = FALSE)

  if(hasArg(eventSummaryColumn)) {
    if(!is.character(eventSummaryColumn))     stop("eventSummaryColumn must be of class 'character'", call. = FALSE)
    if(!is.character(eventSummaryFunction))   stop("eventSummaryFunction must be of class 'character'", call. = FALSE)
  }

  # find image directories
  dirs       <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)

  if(hasArg(includeStation))  {
    stations_to_include <- which(dirs_short %in% includeStation)

    dirs <- dirs[stations_to_include]
    dirs_short <- dirs_short[stations_to_include]
  }

  if(length(dirs) == 0) stop("inDir contains no station directories", call. = FALSE)
  max_nchar_station <- max(nchar(dirs_short))


  # process video argument (if present)
  if(hasArg(video)){
    video_out <- processVideoArgument(IDfrom = IDfrom,
                                      video  = video)
    digiKam_data <- video_out$digiKam_data
    file_formats <- video_out$file_formats
    # if(isFALSE("jpg" %in% file_formats)) file_formats <- c(file_formats, "jpg")
  } else {
    file_formats <- "jpg"   # jpg, as the default, if video not requested
  }

  # empty list for metadata output
  record.table.list <- list()

  # create command line calls
  command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal',
                        ifelse(hasArg(video),
                               paste(" -", video$dateTimeTag, sep = ""),
                               ""),    # if video requested, video date time tag
                        ' -HierarchicalSubject',
                        ifelse(hasArg(additionalMetadataTags), paste(" -",additionalMetadataTags,  collapse = "", sep = ""), ""),
                        paste(" -ext", file_formats, collapse = " ", sep = " "),    # requested file extensions
                        ' "', dirs, '"', sep = "")

  # construct column names for metadata table
  colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal")
  if(hasArg(video)) colnames.tmp <- c(colnames.tmp, video$dateTimeTag)
  colnames.tmp <- c(colnames.tmp, "HierarchicalSubject")
  if(hasArg(additionalMetadataTags)) colnames.tmp <- c(colnames.tmp, additionalMetadataTags [nchar(additionalMetadataTags) >= 2])
  # only add as new column if entry of additionalMetadataTag has more than 2 characters (to allow users to specify exiftool commands, e.g. -L)


  for(i in 1:length(dirs)){   # loop through station directories

    # execute exiftool
    metadata.tmp <- camtrapR:::runExiftool(command.tmp = command.tmp[i], colnames.tmp = colnames.tmp)

    if(class(metadata.tmp) == "NULL"){            # omit station if no images found

      length.tmp <- length(list.files(dirs[i], pattern = paste(".", file_formats, "$", collapse = "|", sep = ""),
                                      ignore.case = TRUE, recursive = TRUE))
      message(paste(formatC(dirs_short[i], width = max_nchar_station, flag = "-"),  ":  ",
                    formatC(length.tmp, width = 5), " files      Skipping", sep = ""))
      warning(paste(dirs_short[i],  ":  contains no files of interest and was omitted"), call. = FALSE,  immediate. = FALSE)
    } else {

      # if video files extracted, add DateTimeOriginal and HierarchicalSubject
      if(hasArg(video)){
        metadata.tmp <- addVideoDateTimeOriginal(metadata.tmp = metadata.tmp, video = video)

        # add HierachicalSubject for video files
        if(!is.null(digiKam_data)){
          digiKamVideoMetadata <- digiKamVideoHierarchicalSubject(stationDir = dirs[i],
                                                                  digiKamTablesList = digiKam_data,    # output of accessDigiKamDatabase
                                                                  videoFormat = file_formats[!grepl(file_formats, pattern = "jpg")])
          # add HierarchialSubject for video files (match by filename and path)
          metadata.tmp <- addVideoHierarchicalSubject (metadata.tmp = metadata.tmp,
                                                       video = video,
                                                       digiKamVideoMetadata = digiKamVideoMetadata,
                                                       digiKamTablesList = digiKam_data,
                                                       videoFormat = file_formats[!grepl(file_formats, pattern = "jpg")])
        }
      }

      # check presence / consistency of DateTimeOriginal column, go to next station or remove records if necessary
      metadata.tmp <- camtrapR:::checkDateTimeOriginal (intable    = metadata.tmp,
                                                        dirs_short = dirs_short,
                                                        i          = i)
      if(is.null(metadata.tmp)) next


      # now split HierarchicalSubject tags and add as columns to table
      metadata.tmp <- camtrapR:::addMetadataAsColumns (intable                    = metadata.tmp,
                                                       metadata.tagname           = metadata.tagname,
                                                       metadataHierarchyDelimitor = metadataHierarchyDelimitor,
                                                       multiple_tag_separator     = multiple_tag_separator)

      # add species names to metadata table (from folders or metadata, otherwise NA)
      metadata.tmp <- camtrapR:::assignSpeciesID(intable                = metadata.tmp,
                                                 IDfrom                 = IDfrom,
                                                 metadataSpeciesTag     = metadataSpeciesTag,
                                                 speciesCol             = speciesCol,
                                                 dirs_short             = dirs_short,
                                                 i_tmp                  = i,
                                                 multiple_tag_separator = multiple_tag_separator,
                                                 returnFileNamesMissingTags = returnFileNamesMissingTags)

      # if images in station contain no metadata species tags, skip that station
      if(!is.data.frame(metadata.tmp)){
        if(metadata.tmp == "found no species tag") {
          warning(paste(dirs_short[i], ":   metadataSpeciesTag '", metadataSpeciesTag, "' not found in image metadata tag 'HierarchicalSubject'. Skipping", sep = ""), call. = FALSE, immediate. = TRUE)
        } else {
          warning(paste(dirs_short[i], ":   error in species tag extraction. Skipping. Please report this bug", sep = ""), call. = FALSE, immediate. = TRUE)
        }
        next
      }

      # remove empty metadata columns (if HierarchicalSubject is all empty or if additionalMetadataTags were not found)
      empty_cols <- which(apply(metadata.tmp, MARGIN = 2, FUN = function(X){all(X == "-")}))
      if(length(empty_cols) >= 1){
        metadata.tmp <-  metadata.tmp[, -empty_cols]
      }

      # add station and camera id to metadata table
      arg.list0 <- list(intable = metadata.tmp, dirs_short = dirs_short, stationCol = stationCol, hasStationFolders = TRUE, cameraCol = cameraCol, i = i, IDfrom = IDfrom)  # assumes station directories

      if(!hasArg(cameraID)) metadata.tmp <- do.call(camtrapR:::addStationCameraID, arg.list0)
      if( hasArg(cameraID)) metadata.tmp <- do.call(camtrapR:::addStationCameraID, c(arg.list0, cameraID = cameraID))

      # remove species in argument "excluded"
      if(hasArg (exclude)){
        if(any(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude))) {  # if there is anything to remove
          metadata.tmp <- metadata.tmp[-which(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude)),]
        }
      }

      if(nrow(metadata.tmp) >= 1){   # if anything left after excluding species, do

        # convert character vector extracted from images to time object and format for outfilename
        metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata.tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = timeZone))

        # sort by station, (camera), species and time
        if(camerasIndependent == TRUE) {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,speciesCol], metadata.tmp[,cameraCol], metadata.tmp$DateTimeOriginal),]
        } else {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,speciesCol], metadata.tmp$DateTimeOriginal),]
        }



        #remove duplicate records of same species taken in same second at the same station (by the same camera, if relevant)
        metadata.tmp2 <- camtrapR:::removeDuplicatesOfRecords(metadata.tmp          = metadata.tmp,
                                                              removeDuplicateRecords = removeDuplicateRecords,
                                                              camerasIndependent     = camerasIndependent,
                                                              stationCol             = stationCol,
                                                              speciesCol             = speciesCol,
                                                              cameraCol              = cameraCol,
                                                              current                = i,
                                                              total                  = length(dirs),
                                                              max_nchar_station      = max_nchar_station)


        # assess independence between records and calculate time differences
        args.assessTemporalIndependence <- list(intable             = metadata.tmp2,
                                                deltaTimeComparedTo = deltaTimeComparedTo,
                                                columnOfInterest    = speciesCol,
                                                cameraCol           = cameraCol,
                                                camerasIndependent  = camerasIndependent,
                                                minDeltaTime        = minDeltaTime,
                                                stationCol          = stationCol)

        if(hasArg(eventSummaryColumn)) {
          args.assessTemporalIndependence <- c(args.assessTemporalIndependence,
                                               list(eventSummaryColumn   = eventSummaryColumn,
                                                    eventSummaryFunction = eventSummaryFunction))
        }

        d1 <- do.call(camtrapR:::assessTemporalIndependence, args = args.assessTemporalIndependence)

        record.table.list[[i]] <- d1

        suppressWarnings(rm(d1))
      }  # end      if(nrow(metadata.tmp) >= 1){} else {...}   # i.e. not all species were excluded
    }    # end      if(nrow(metadata.tmp) == 0){} else {...}   # i.e. directory i contained images
  }      # end      for(i in 1:length(dirs)){   # loop through station directories


  # combine all data frames from list into one data frame
  record.table <- as.data.frame(data.table::rbindlist(record.table.list, fill = TRUE, use.names = TRUE))

  if(nrow(record.table) == 0){
    stop(paste("something went wrong. I looked through all those", length(dirs)  ,"folders and now your table is empty. Were date/time information unreadable? Could species tags not be extracted (if IDfrom = 'metadata')? Or did you exclude too many species?"), call. = FALSE)
  }

  # rearrange table, add date and time as separate columns. add additional column names as needed.

  record.table2  <-  data.frame(record.table[,c(stationCol, speciesCol, "DateTimeOriginal")],
                                Date = as.Date (record.table$DateTimeOriginal, format = "%Y/%M/%d", tz = timeZone),
                                Time = strftime(record.table$DateTimeOriginal, format = "%H:%M:%S", tz = timeZone),
                                record.table[,c("delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days",
                                                "Directory", "FileName")])

  metadata_columns <- which(!colnames(record.table) %in% colnames(record.table2))

  # add metadata columns
  if(length(metadata_columns) >= 1){
    record.table3 <- cbind(record.table2, record.table[,metadata_columns])
    colnames(record.table3)[(ncol(record.table2) + 1) : ncol(record.table3)] <- colnames(record.table)[metadata_columns]
  } else {record.table3 <- record.table2}


  # add camera column (if present)
  if(hasArg(cameraID)){
    record.table3 <- data.frame(record.table3[,stationCol],
                                record.table[,cameraCol],
                                record.table3[,-which(colnames(record.table3) %in% c(stationCol, cameraCol))])
    colnames(record.table3)[1] <- stationCol
    colnames(record.table3)[2] <- cameraCol
  }

  rownames(record.table3) <- NULL


  # warning if additionalMetadataTags were not found
  if(hasArg(additionalMetadataTags)){
    #whichAdditionalMetadataTagsFound <- which(gsub(additionalMetadataTags, pattern = ":", replacement = ".") %in% colnames(record.table3))   # replace : in additionalMetadataTags (if specifying tag groups) with . as found in column names
    whichAdditionalMetadataTagsFound <- which(additionalMetadataTags %in% colnames(record.table3))   # replace : in additionalMetadataTags (if specifying tag groups) with . as found in column names
    if(length(whichAdditionalMetadataTagsFound) < length(additionalMetadataTags)){
      if(length(whichAdditionalMetadataTagsFound) == 0) {  # if none of the additionalMetadataTags was found
        warning("metadata tag(s)  not found in image metadata:  ", paste(additionalMetadataTags, collapse = ", "), call. = FALSE)
      } else {                                                            # if only some of the additionalMetadataTags was found
        warning("metadata tag(s)  not found in image metadata:  ", paste(additionalMetadataTags[-whichAdditionalMetadataTagsFound], collapse = ", "), call. = FALSE)
      }
    }
  }

  # make column "HierarchicalSubject" the last column
  col_to_move <- which(colnames(record.table3) %in% metadata.tagname)
  if(length(col_to_move) >= 1){
    record.table3 <- cbind(record.table3, record.table3[,col_to_move])
    record.table3 <- record.table3[,-col_to_move]
    colnames(record.table3)[ncol(record.table3)] <- metadata.tagname
  }

  # convert to data.frame, in order to get all the column names right (: becomes .)
  # NOTE: doesn't work unless data.frame is called. : in column names means they must be adressed as e.g. record.table3$`IPTC:Keywords`
  record.table3 <- data.frame(record.table3, stringsAsFactors = FALSE, check.names = TRUE)

  # save table
  if(writecsv == TRUE){
    outtable_filename <- paste("record_table_", minDeltaTime, "min_deltaT_", Sys.Date(), ".csv", sep = "")
    message("saving csv to  ", file.path(inDir, outtable_filename))
    if(hasArg(outDir) == FALSE){
      setwd(inDir)
    } else {
      setwd(outDir)
    }
    write.csv(record.table3, file = outtable_filename)
  }
  return(record.table3)
}


#' Camera Trap Record Table for subset sites and using multiple cores
#'
#' @param inDir character. Directory containing station directories. It must
#' either contain images in species subdirectories (e.g.
#' inDir/StationA/SpeciesA) or images with species metadata tags (without
#' species directories, e.g. inDir/StationA).
#' @param intermediateDir character. Directory path to store intermediate rds objects,
#' which are one rds (intermediate file) per station. This directory should not have stations,
#' data or files that are not intermediate files.
#' @param ... Additional arguments passed to \code{\link[camtrapR:recordTable]{recordTable}}
#' @param cores Number of cores to use
#' @param includeStations character. (OPTIONAL) Subset of stations to extract data for within a directory.
#' Can be useful if only wanting to extract data for a subset of sites within a directory. If no
#' argument is given function will default to all stations. Otherwise, provide a character vector of
#' station names (matching the subdirectory names in inDir).
#' @param overwrite Re-extract data for sites where an intermediate file already exists. The default (FALSE),
#' ensures data is not re-extracted for sites that already have intermediate files. Allowing you to
#' pick up where you left off for extractions that take a long time.
#'
#' @return data.frame (same as \code{\link[camtrapR:recordTable]{recordTable}})
#' @export
#'
#' @examples
#' int.dir <- tempdir()
#' raw_camtrap_records <- mcrecordTable(inDir  = system.file("dummydata/images", package = "weda"),
#'                                     IDfrom = "metadata",
#'                                     cameraID = "directory",
#'                                     stationCol = "SiteID",
#'                                     camerasIndependent = TRUE,
#'                                     timeZone = Sys.timezone(location = TRUE),
#'                                     metadataSpeciesTag = "Species",
#'                                     removeDuplicateRecords = FALSE,
#'                                     returnFileNamesMissingTags = TRUE,
#'                                     includeStations = c("832", "2602"),
#'                                     intermediateDir = int.dir,
#'                                     overwrite = FALSE, cores = 1)
mcrecordTable <- function(inDir, intermediateDir, includeStations, overwrite = FALSE, ..., cores = 1) {

  stations <- list.dirs(inDir, full.names = F, recursive = F)

  if(hasArg(includeStations)) {
    stations <- stations[stations %in% includeStations]
  }

  if(overwrite) {
    completed_stations <- NULL
  } else {
  completed_stations <- gsub(x = list.files(intermediateDir, recursive = F, full.names = F),
                             pattern =  ".rds",
                             replacement =  "")
}
  stationstoprocess <- setdiff(stations, completed_stations)

  # split.stations <- split(stations, sort(1:length(stations) %% cores))

  data <- bettermc::mclapply(stationstoprocess, FUN = function(x) {
    sitetab <- recordTable.inc(inDir  = inDir,
                               includeStation = x,
                               ...)

    saveRDS(sitetab, file.path(intermediateDir, paste0(x, ".rds")))

    return(sitetab)
  }, mc.cores = cores, mc.allow.fatal = TRUE, mc.allow.error = TRUE, mc.preschedule = FALSE)

  # return(data)
  files_read <- list.files(intermediateDir, full.names = T)
  sites_rec_table <- lapply(files_read, readRDS)
  bound_rec_table <- dplyr::bind_rows(sites_rec_table)

}
