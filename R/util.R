.get_valid_SelFilt<-function(){
  readme_url <- "https://raw.githubusercontent.com/datastreamapp/api-docs/refs/heads/main/docs/README.md"
  readme <- httr2::request(readme_url) |>
    httr2::req_perform() |>
    httr2::resp_body_raw() |>
    rawToChar()

  medatata_info <- regmatches(readme,
                              regexec("\\*\\*GET \\/Metadata\\*\\*\\s*(.*?)\\s*\\*\\*GET \\/Locations\\*\\*", readme))[[1]][[2]]
  location_info <- regmatches(readme,
                              regexec("\\*\\*GET \\/Locations\\*\\*\\s*(.*?)\\s*\\*\\*GET \\/Observations\\*\\*", readme))[[1]][[2]]
  observation_info <- regmatches(readme,
                                 regexec("\\*\\*GET \\/Observations\\*\\*\\s*(.*?)\\s*\\*\\*GET \\/Records\\*\\*", readme))[[1]][[2]]
  records_info <- regmatches(readme,
                             regexec("\\*\\*GET \\/Records\\*\\*\\s*(.*?)\\s*## Body Object", readme))[[1]][[2]]
  records_info<-gsub("#","-",records_info)

  list_info<-list(
    metadata=medatata_info,
    locations=location_info,
    observations=observation_info,
    records=records_info
  )

  list_info <- lapply(list_info, function(x){
    fil<-regmatches(x,
                    regexec("\\- Filter By\\:\\s*(.*?)\\s\\\n\\-", x))[[1]][[2]]
    fil<-gsub("\\\n","",fil)
    fil<-strsplit(fil,", ")
    fil<-lapply(fil,function(x) gsub("[[:punct:]]", "", x))
    fil<-lapply(fil, trimws)
    fil<-lapply(fil, function(x) gsub("Normalized coordinates are in WGS84 projection|Maps to MonitoringLocationID internally","",x))
    fil<-lapply(fil, trimws)

    sel<-regmatches(x,
                    regexec("\\- Select By\\:\\s*(.*?)\\sFilter By\\:", x))[[1]][[2]]
    sel<-gsub("\\\n","",sel)
    sel<-gsub("Normalized coordinates are in WGS84 projection|Maps to MonitoringLocationID internally","",sel)
    sel<-strsplit(sel,", ")
    sel<-lapply(sel,function(x) gsub("[[:punct:]]", "", x))
    sel<-lapply(sel, trimws)
    sel<-lapply(sel, function(x) gsub("Normalized coordinates are in WGS84 projection|Maps to MonitoringLocationID internally","",x))
    sel<-lapply(sel, trimws)

    return(list(sel=sel[[1]],filt=fil[[1]]))
  })

  return(list_info)
}

.get_valid_primary<-function(field){
  ot<-try(
    suppressWarnings(
      jsonlite::read_json(
        paste0("https://raw.githubusercontent.com/datastreamapp/schema/main/schemas/data/src/values/",
               field,
               ".primary.json")
      )$enum |>
        tibble::enframe() |>
        tidyr::unnest(value) |>
        dplyr::pull(value) |>
        unique()
    ),
    silent=T)

  if (inherits(ot,"try-error")) return(NULL)
  return(ot)
}

.ds_safe_top<-function(ep=c("metadata","locations","observations","records")
                       ,...,
                       .top_list=c(10000L,7500L,5000L,2500L,1000L,500L)){
  ep<-match.arg(ep)
  stopifnot(any(is.integer(.top_list)))
  .top_list<-sort(.top_list,decreasing = T)
  ep_sel <- switch(ep,
                   metadata = datastreamr::metadata,
                   locations = datastreamr::locations,
                   observations = datastreamr::observations,
                   records = datastreamr::records,
  )
  top_sel<-.top_list[[1]]
  out_err<-T
  while(out_err){
    out <- try(
      ep_sel(
        qs_helper(top=top_sel,
                  ...)
      ),
      silent = T
    )

    if (inherits(out,"try-error")){
      .top_list<-.top_list[-c(1)]
      if (length(.top_list)==0) stop(attr(out,"condition")$message)
      top_sel<-.top_list[[1]]
    } else {
      out_err<-F
    }
  }

  return(out)
}

.load_schema<-function(){

  path <- tempfile()
  dl <- download.file("https://datastream.org/schema", path,method="auto", quiet =T)
  scm <- jsonlite::read_json(path)

  scm$allOf <- lapply(scm$allOf,
                      function(x){
                        if (!is.null(x$properties)) {
                          x$properties <- lapply(x$properties,
                                                 function(xx){
                                                   if (!is.null(xx$pattern)) {
                                                     xx <- xx[names(xx)!="pattern"]
                                                     # xx$pattern <- gsub("^\\^","",xx$pattern)
                                                     # xx$pattern <- gsub("\\$$","",xx$pattern)
                                                     # xx$pattern <- gsub("p\\{L\\}","p\\{Letter\\}",xx$pattern)
                                                     # xx$pattern <- gsub("p\\{N\\}","p\\{Number\\}",xx$pattern)
                                                     # xx$pattern <- gsub("p\\{P\\}","p\\{Punctuation\\}",xx$pattern)
                                                     # xx$pattern <- gsub("p\\{S\\}","p\\{Symbol\\}",xx$pattern)
                                                     # xx$pattern <- gsub("p\\{M\\}","p\\{Mark\\}",xx$pattern)
                                                     # xx$pattern <- gsub("p\\{Z\\}","p\\{Separator\\}",xx$pattern)
                                                     # xx$pattern <- gsub("p\\{C\\}","p\\{Other\\}",xx$pattern)
                                                   }
                                                   return(xx)
                                                 })
                        }
                        return(x)
                      })

  sub_sc <- lapply(scm$allOf,
                   function(x)
                     suppressWarnings(
                       jsonvalidate::json_schema$new(
                         jsonlite::toJSON(x,auto_unbox = T,digits = 999),
                         strict = F)
                     )
  )

  return(sub_sc)
}

.format_data<-function(x,
                       sub_sc=NULL
){

  stopifnot(inherits(x,"data.frame"))
  if (is.null(sub_sc)) sc_sub <- .load_schema()

  dt_list <- x |>
    tibble::as_tibble() |>
    dplyr::mutate(Row=dplyr::row_number()) |>
    dplyr::group_by(Row) |>
    dplyr::group_split()

  dt_list <- purrr::map(dt_list,
                        as.list)

  dt_list <- purrr::map(dt_list,
                        function(x){
                          x <- x[!(sapply(x,is.na) | sapply(x,is.null))]
                          sub_sc[[1]]$serialise(x)
                        })

  return(dt_list)
}

.schema_qc<-function(){

  substitute_refs <- function(x, defs) {
    if (is.list(x)) {
      is_ref <- names(x) == "$ref"

      if (length(is_ref) > 0 && any(is_ref)) {
        #browser()
        def_location <- x[is_ref]
        def_location <- unlist(def_location, recursive=F)

        def_idx <- gsub("#/", "/", def_location, fixed = TRUE) |>
          strsplit(split = "/") |>
          unlist() |>
          as.list()

        def_idx <- def_idx[!sapply(def_idx,identical,"")]
        def_idx <- def_idx[!sapply(def_idx,identical,".")]
        def_idx <- def_idx[!sapply(def_idx,identical,"..")]

        replace <- purrr::pluck(defs, !!!def_idx)

        replace_prev <- replace
        while (!is.null(replace)) {
          replace_prev <- replace

          def_idx2 <- gsub("#/", "/", replace, fixed = TRUE) |>
            strsplit(split = "/") |>
            unlist() |>
            as.list()

          def_idx2 <- def_idx2[!sapply(def_idx2,identical,"")]
          def_idx2 <- def_idx2[!sapply(def_idx2,identical,".")]
          def_idx2 <- def_idx2[!sapply(def_idx2,identical,"..")]

          def_idx2 <- c(def_idx[[1]],def_idx2)

          replace <- purrr::pluck(defs, !!!def_idx2)
        }

        replace <- replace_prev

        if (is.null(replace)) {
          cli::cli_warn(
            c(x = "definition for def {.val { def_location }} returning {.var NULL} ")
          )
          x
        } else {
          x <- replace
        }
      } else {
        lapply(x, substitute_refs, defs = defs)
      }
    } else {
      x
    }
  }

  path <- tempfile(fileext = ".zip")
  dl <- download.file("https://github.com/datastreamapp/schema/archive/refs/heads/main.zip", path,method="auto", quiet =T)

  path2 <- tempfile(fileext = ".zip")
  dl <- download.file("https://github.com/datastreamapp/wqx/archive/refs/heads/main.zip", path2,method="auto", quiet =T)

  unzip(path,exdir=file.path(tempdir(),"DataStream"),overwrite = TRUE)
  unzip(path2,exdir=file.path(tempdir(),"WQX"),overwrite = TRUE)

  dir.create(file.path(tempdir(),"DataStream","schema-main","schemas","data","src","node_modules","wqx"),
             recursive = T,
             showWarnings = F)

  ft <- file.copy(
    file.path(tempdir(),"WQX","wqx-main","src"),
    file.path(tempdir(),"DataStream","schema-main","schemas","data","src","node_modules","wqx"),
    recursive = T,
    overwrite = T
  )

  ft <- file.rename(
    file.path(tempdir(),"DataStream","schema-main","schemas","data","src","node_modules","wqx","src"),
    file.path(tempdir(),"DataStream","schema-main","schemas","data","src","node_modules","wqx","json-schema")
  )


  fl <- list.files(
    file.path(tempdir(),"DataStream","schema-main","schemas","data","src"),
    pattern = "\\.json$",
    full.names = T,
    recursive = T
  )

  fl2 <- list.files(
    file.path(tempdir(),"DataStream","schema-main","schemas","data","src","quality-control"),
    pattern = "\\.json$",
    full.names = T,
    recursive = T
  )

  fl2 <- fl2[!grepl("\\/partial\\/|\\/_|\\.legacy\\.",fl2)]

  rep_list <- lapply(fl[grepl("definitions",fl)],
                     jsonlite::read_json,
                     simplifyVector = TRUE,
                     simplifyDataFrame = FALSE) |>
    setNames(basename(fl[grepl("definitions",fl)]))
  rep_list2 <- list("quality-control"=lapply(fl2[grepl("quality-control",fl2)],
                     jsonlite::read_json,
                     simplifyVector = TRUE,
                     simplifyDataFrame = FALSE) |>
    setNames(basename(fl2)))
  rep_list3 <- list("logic"=lapply(fl[grepl("logic",fl)],
                      jsonlite::read_json,
                      simplifyVector = TRUE,
                      simplifyDataFrame = FALSE) |>
    setNames(fl[grepl("logic",fl)]))
  #
  # for (sc_path in fl) {
  #   scm <- jsonlite::read_json(sc_path,
  #                              #simplifyVector = TRUE,
  #                              #simplifyDataFrame = FALSE
  #   )
  #   if (!is.list(scm)) next()
  #
  #   scm <- rapply(scm, function(x) gsub("^\\.\\/","",x),how ="list")
  #   scm <- rapply(scm, function(x) gsub("^\\..\\/","",x),how ="list")
  #
  #   scm_sub <- try(substitute_refs(scm,rep_list["definitions"]),silent = T)
  #
  #   if (!inherits(scm_sub,"try-error")) scm<-scm_sub
  #
  #   jsonlite::write_json(scm,
  #                        pretty = T,
  #                        sc_path,
  #                        auto_unbox = T,
  #                        digits = 999
  #   )
  # }

  path <- file.path(tempdir(),"DataStream","schema-main","schemas","data","src","quality-control.json")
  sub_sc <- jsonlite::read_json(path)
  sub_sc <- sub_sc$allOf[[1]]
  sub_sc <- sub_sc[names(sub_sc)!="allOf"]
  sub_sc <- substitute_refs(sub_sc,rep_list)
  # sub_sc <- substitute_refs(sub_sc,rep_list2)
  # sub_sc <- substitute_refs(sub_sc,rep_list)
  # sub_sc <- substitute_refs(sub_sc,rep_list3)
  # sub_sc <- substitute_refs(sub_sc,rep_list)
  #sub_sc <- sub_sc[names(sub_sc)!="$vocabulary"]
  sub_sc <- rapply(sub_sc,
                   function(x) {
                     if (is.list(x)) return(x)
                     if (x=="TRUE") return(TRUE)
                     if (x=="FALSE") return(FALSE)
                     if (is.na(as.numeric(x))) return(x)
                     return(as.numeric(x))
                   },
                   how ="list")
  sub_sc <- jsonlite::toJSON(sub_sc,auto_unbox = T,digits = 999)
  json1 <- jsonvalidate::json_schema$new(
    sub_sc,
    strict = F
  )

  sub_sc <- lapply(fl2,function(path){
    sub_sc <- jsonlite::read_json(path)
    sub_sc <- substitute_refs(sub_sc,rep_list)
    sub_sc <- rapply(sub_sc,
                     function(x) {
                       if (x=="TRUE") return(TRUE)
                       if (x=="FALSE") return(FALSE)
                       if (is.na(as.numeric(x))) return(x)
                       return(as.numeric(x))
                     },
                     how ="list")
    sub_sc <- jsonlite::toJSON(sub_sc,auto_unbox = T,digits = 999)

    jsonvalidate::json_schema$new(
      sub_sc,
      strict = F
    )
  })

  return(sub_sc)

}
