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
