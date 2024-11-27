#' Query DataStream API
#'
#' @param ep one of "metadata","locations","observations", or "records". Corresponding to \link[datastreamr]{metadata}, \link[datastreamr]{locations}, \link[datastreamr]{observations}, and \link[datastreamr]{records}.
#' @param select a vector of fields to request from the API.
#' @param filter a named list of fields to filter by from the API. Each element in the list should be a list or vector in which the first element represents a R operator ('==','%in%','>=', etc.), and subsequent elements representing the criteria.
#' @param count a logical value indicating whether to return the number of observations rather than the data itself.
#' @param .max_filt an integer specifying the maximum number of filtered elements to include in each query.
#' @param .top_list a vector of integers applied to `$top`. The first element is selected for each query, and if the payload is too large, the next biggest value is used.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' library(datastreamr.helper)
#'
#' loc <- DS_helper(
#'   ep="locations",
#'   filter=list(
#'     DOI=c("==","10.25976/ori9-w562"),
#'     CharacteristicName=c("%in%","Specific conductance","pH"),
#'     ActivityStartYear=c(">=",2020)
#'   )
#' )
#'
#' rec <- DS_helper(
#'   ep="records",
#'   select=c("MonitoringLocationID",
#'            "ActivityStartDate",
#'            "ActivityStartTime",
#'            "CharacteristicName",
#'            "ResultUnit",
#'            "ResultValue",
#'            "ResultDetectionCondition",
#'            "ResultStatusID",
#'            "ResultComment"),
#'   filter=list(
#'     DOI=c("==","10.25976/ori9-w562"),
#'     CharacteristicName=c("%in%","Specific conductance","pH"),
#'     ActivityStartYear=c(">=",2020),
#'     LocationId=c("%in%", loc$Id) # Note 'Id' used here instead of 'ID'
#'   )
#' )
#'
#' }

DS_helper <- function(ep=c("metadata","locations","observations","records"),
                      select=NULL,
                      filter=NULL,
                      count=NULL,
                      .max_filt=10L,
                      .top_list=c(10000L,7500L,5000L,2500L,1000L,500L)){

  ep<-match.arg(ep)
  stopifnot(length(.max_filt)==1)
  stopifnot(is.integer(.max_filt))
  stopifnot(any(is.integer(.top_list)))

  valid_SelFilt<-.get_valid_SelFilt()

  if (!is.null(select)){
    stopifnot(is.vector(select))
    select<-unique(select)
    bad_sel<-select[!select %in% valid_SelFilt[[ep]]$sel]
    if (length(bad_sel)>0){
      bad_sel<-paste0("'",paste0(bad_sel,collapse = ", '"),"'")
      stop(
        paste0("`",ep,"`"," `select` cannot include: ",paste0(bad_sel,collapse = ", "))
      )
    }
  }

  if (!is.null(count)){
    stopifnot(is.logical(count),length(count)==1)
  }

  if (!is.null(filter)){
    stopifnot(is.list(filter),
              !is.null(names(filter))
    )

    # if (any(duplicated(names(filter)))) stop("Duplicate filters are not allowed")

    filter<-filter[!sapply(filter,function(x) is.na(x[1]))]
    filter<-filter[!sapply(filter,function(x) is.null(x[2]))]
    filter<-filter[!sapply(filter,function(x) is.na(x[2]))]
    filter<-filter[!sapply(filter,is.null)]
    filter<-filter[!sapply(names(filter),is.na)]
    filter<-filter[!sapply(names(filter),is.null)]

    bad_filt<-names(filter)
    bad_filt<-bad_filt[!bad_filt %in% valid_SelFilt[[ep]]$filt]
    if (length(bad_filt)>0){
      bad_filt<-paste0("'",paste0(bad_filt,collapse = ", '"),"'")
      stop(
        paste0("`",ep,"`"," `filter` cannot include: ",paste0(bad_filt,collapse = ", "))
      )
    }

    filter<-lapply(filter,trimws)
    filter<-lapply(filter,function(x) lapply(x,trimws))
    filter<-purrr::map2(filter,names(filter),function(x,y) {
      opr<-x[1]
      ot<-x[-c(1)]

      bad_lu<-.get_valid_primary(y)

      if (length(bad_lu)>0){
        bad_lu<-ot[!ot %in% bad_lu]
        if (length(bad_lu)>0){
          bad_lu<-paste0("'",paste0(bad_lu,collapse = ", '"),"'")
          stop(
            paste0("`",ep,"`"," `filter` '",y,"' cannot include: ",paste0(bad_lu,collapse = ", "))
          )
        }
      }

      ot<-split(ot, ceiling(seq_along(ot)/.max_filt))
      ot<-lapply(ot,function(xx) unlist(c(list(opr),xx)))
      return(ot)
    })

    filter<-expand.grid(filter, KEEP.OUT.ATTRS = FALSE)
    filter<-split(filter,1:nrow(filter))
    filter<-lapply(filter,as.list)
    filter<-lapply(filter,function(xx) lapply(xx,function(xxx) unname(unlist(xxx))) )
  }

  out_list<-list()
  for (i in filter){
    out <- try(
      .ds_safe_top(
        ep,
        filter=i,
        select=select,
        count=count,
        .top_list=.top_list
      ),
      silent=F)
    if (inherits(out,"try-error")){
      qs<-qs_helper(filter=i,
                    select=select,
                    count=count)
      qs<-purrr::map2(qs,names(qs),~paste(.y,.x))
      warning(paste0("The following filters returned an error:",
                     paste0(qs,
                            collapse=" | ")))
    } else {
      out_list<-c(out_list,list(out))
    }
  }

  out<-dplyr::bind_rows(out_list)

  return(out)
}
