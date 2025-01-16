#' DataStream Schema Validator
#'
#' Validate supplied data frame against DataStream Schema: https://datastream.org/schema
#'
#' Function will save *x* to a temporary file internally.
#'
#' @param x a data frame or file path to a CSV to be uploaded to DataStream
#' @param ncores NULL or an integer indicating the number of cores
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

DS_validator <- function(x,
                         ncores = NULL,
                         .chunk_size=9999L){

  if (!is.null(ncores)){
    future::plan(future::multisession(workers = ncores))
  }

  x_path<-""
  if (inherits(x,"data.frame")) {
    x_path <- tempfile(fileext = ".csv")
    readr::write_csv(x,x_path)
    x <- x_path
  }

  if (!file.exists(x)) stop("File 'x' doesn't exist, or the file path is specified incorrectly")

  sc_sub1 <- .load_schema()
  sc_sub2 <- .schema_qc()

  sc_sub <- c(sc_sub1,sc_sub2)

  callback <- readr::DataFrameCallback$new(function(x, pos) {
    dt_list <- .format_data(x,sc_sub)

    nw <- future::nbrOfWorkers()

    dt_list_split <- suppressWarnings(split(dt_list, rep(1:nw,each=ceiling(length(dt_list)/nw))))
    schema_split <- lapply(sc_sub,function(x) x$schema$schema)
    schema_split <- rep(list(schema_split),nw)

    out <- furrr::future_map2_dfr(
      dt_list_split,
      schema_split,
      .progress = T,
      .options = furrr::furrr_options(globals = F,seed =T),
      function(xx,yy){
        purrr::map_dfr(
          yy,
          function(y) {

            y <- jsonvalidate::json_schema$new(
              y,
              strict = F
            )

            purrr::map_dfr(
              xx,
              .id = "Row",
              function(x){

                valid_out <- tibble::tibble(Field=NA_character_,
                                            Title=NA_character_,
                                            Keyword=NA_character_,
                                            Message=NA_character_,
                                            Description=NA_character_)[F,]

                out <- y$validate(x,verbose=T,greedy =T)
                out <- attr(out,"errors")

                if (!is.null(out)) {
                  field <- out$instancePath
                  field1 <- out$params$missingProperty[out$instancePath==""]
                  if (is.null(field1)) field1 <- ""
                  field[field==""] <- field1
                  field <- gsub("\\/","",field)

                  Title <- out$parentSchema$title
                  Title1 <- sapply(out$parentSchema,function(x) purrr::pluck(x,"title",.default=NA_character_))
                  if (!all(is.na(Title1)) & is.null(Title)) Title <- Title1

                  Description <- out$parentSchema$description
                  Description1 <- sapply(out$parentSchema,function(x) purrr::pluck(x,"description",.default=NA_character_))
                  if (!all(is.na(Description1)) & is.null(Description)) Description <- Description1

                  valid_out <- tibble::tibble(
                    Field=field,
                    Title=Title,
                    Keyword=out$keyword,
                    Message=out$message,
                    Description=Description
                  )

                  valid_out <- tidyr::fill(valid_out,
                                           tidyselect::any_of(c("Title", "Description")),
                                           .direction="downup")

                }

                return(valid_out)
              })
          })
      })

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
    dplyr::group_by(Title,Description,Message,Field,Keyword) |>
    dplyr::summarise(
      Rows=paste0(unique(Row),collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::select(Rows,Field,Title,Keyword,Message,Description)

  valid_out$Rows <- as.list(valid_out$Rows)

  valid_out <- valid_out[valid_out$Message != "must match a schema in anyOf",]
  valid_out <- valid_out[valid_out$Message != "must match \"then\" schema",]
  valid_out <- valid_out[valid_out$Keyword != "pattern",]
  valid_out <- valid_out[!grepl("OWASP ASVS",valid_out$Description),]

  fr <- suppressWarnings(file.remove(x_path,showWarnings = F))

  future::plan(future::sequential)

  return(valid_out)
}



