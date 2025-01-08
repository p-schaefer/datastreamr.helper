#' DataStream Schema Validator
#'
#' Validate supplied data frame against DataStream Schema: https://datastream.org/schema
#'
#' Schema checks related to pattern matching are currently unavailable. Function will save data frame to a temporary file internally.
#'
#' @param x a data frame or file path to a CSV to be uploaded to DataStream
#' @param .chunk_size an integer indicating the number of rows to read into R at a time
#'
#' @returns a data frame of error messages from schema validation
#' @export
#'
#' @examples
#' dt <- tibble::tribble(
#' ~DatasetName, ~MonitoringLocationID, ~MonitoringLocationName, ~MonitoringLocationLatitude, ~MonitoringLocationLongitude, ~MonitoringLocationHorizontalCoordinateReferenceSystem, ~MonitoringLocationHorizontalAccuracyMeasure, ~MonitoringLocationHorizontalAccuracyUnit, ~MonitoringLocationVerticalMeasure, ~MonitoringLocationVerticalUnit, ~MonitoringLocationType,   ~ActivityType, ~ActivityMediaName, ~ActivityStartDate, ~ActivityStartTime, ~ActivityEndDate, ~ActivityEndTime, ~ActivityDepthHeightMeasure, ~ActivityDepthHeightUnit, ~SampleCollectionEquipmentName,  ~CharacteristicName, ~MethodSpeciation, ~ResultSampleFraction, ~ResultValue, ~ResultUnit, ~ResultValueType, ~ResultDetectionCondition, ~ResultDetectionQuantitationLimitMeasure, ~ResultDetectionQuantitationLimitUnit, ~ResultDetectionQuantitationLimitType, ~ResultStatusID, ~ResultComment, ~ResultAnalyticalMethodID, ~ResultAnalyticalMethodContext, ~ResultAnalyticalMethodName, ~AnalysisStartDate, ~AnalysisStartTime, ~AnalysisStartTimeZone, ~LaboratoryName, ~LaboratorySampleID,
#' "test",             "test-314",               "test-314",                  43.5895361,                  -79.9411775,                                                "NAD83",                                           NA,                                        NA,                                 NA,                              NA,          "River/Stream", "Field Msr/Obs",    "Surface Water",       "2005-07-12",                 NA,               NA,               NA,                          NA,                       NA,                 "Probe/Sensor", "Temperature, water",                NA,                    NA,           18,     "deg C",         "Actual",                        NA,                                       NA,                                    NA,                                    NA,              NA,             NA,                        NA,                             NA,                          NA,                 NA,                 NA,                     NA,              NA,                  NA
#' )
#'
#' DS_validator(dt)
#'
#' dt_bad <- dt
#' dt_bad$CharacteristicName <- "Something Wrong"
#'
#' dt_new <- rbind(dt,dt_bad)
#'
#' DS_validator(dt_new)
#'

DS_validator <- function(x,.chunk_size=9999L){

  x_path<-""
  if (inherits(x,"data.frame")) {
    x_path <- tempfile(fileext = ".csv")
    readr::write_csv(x,x_path)
    x <- x_path
  }

  if (!file.exists(x)) stop("File 'x' doesn't exist, or the file path is specified incorrectly")

  sc_sub <- .load_schema()

  callback <- readr::DataFrameCallback$new(function(x, pos) {
    dt_list <- .format_data(x,sc_sub)
    out <- purrr::map_dfr(dt_list,
                          .id = "Row",
                          .progress=list(name=paste("Validating rows", pos, "to", pos+length(dt_list)-1)),
                          function(x)
                            purrr::map_dfr(sc_sub,
                                           function(y) {
                                             out <- y$validate(x,verbose=T,greedy =T)
                                             out <- attr(out,"errors")

                                             field <- out$instancePath
                                             field1 <- out$params$missingProperty[out$instancePath==""]
                                             if (is.null(field1)) field1 <- ""
                                             field[field==""] <- field1
                                             field <- gsub("\\/","",field)

                                             valid_out <- tibble::tibble(
                                               Field=field,
                                               Title=out$parentSchema$title,
                                               Keyword=out$keyword,
                                               Message=out$message,
                                               Description=out$parentSchema$description
                                             )

                                             valid_out <- valid_out |>
                                               dplyr::mutate(dplyr::across(tidyselect::everything(), ~tidyr::replace_na(.x,"")))|>
                                               dplyr::summarise(dplyr::across(tidyselect::everything(), ~paste0(unique(.x[.x!=""]),collapse = ", ")))

                                             if (nrow(valid_out)==0) {
                                               valid_out <- tibble::tibble(Field=NA_character_,
                                                                           Title=NA_character_,
                                                                           Keyword=NA_character_,
                                                                           Message=NA_character_,
                                                                           Description=NA_character_)[F,]
                                             }
                                             return(valid_out)
                                           }
                            )
    )

    out$Row <- as.numeric(out$Row) + pos - 1

    return(out)
  })

  valid_out <- readr::read_csv_chunked(x,
                                       callback = callback,
                                       show_col_types = F,
                                       #col_types = readr::cols(.default = "c"),
                                       chunk_size = .chunk_size,
                                       progress = T)

  if (nrow(valid_out)==0) {
    valid_out <- tibble::tibble(Row=NA_character_,
                                Field=NA_character_,
                                Title=NA_character_,
                                Keyword=NA_character_,
                                Message=NA_character_,
                                Description=NA_character_)[F,]
  }

  valid_out <- valid_out |>
    dplyr::group_by(Field,Title,Keyword,Message,Description) |>
    dplyr::summarise(
      Rows=paste0(unique(Row),collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::select(Rows,Field,Title,Keyword,Message,Description)

  valid_out$Rows <- as.list(valid_out$Rows)

  fr <- suppressWarnings(file.remove(x_path,showWarnings = F))

  return(valid_out)
}



