#' Format inputs for *datastreamr*
#'
#' For details see: \href{v}{datastreamr}
#'
#' @param select a vector of fields to request from the API.
#' @param filter a named list of fields to filter by from the API. Each element in the list should be a list or vector in which the first element represents a R operator ('==','%in%','>=', etc.), and subsequent elements representing the criteria.
#' @param top an integer representing the number of rows query at a time (max = 10000).
#' @param count a logical value indicating whether to return the number of observations rather than the data itself.
#'
#' @return a list formatted for input as the `qs` argument (i.e., \link[datastreamr]{metadata})
#' @export
#'
#' @examples
#'
#' library(datastrear.helper)
#' qs_helper(
#'   select=c("ActivityStartDate",
#'            "ActivityStartTime",
#'            "CharacteristicName",
#'            "ResultUnit",
#'            "ResultValue",
#'            "ResultDetectionCondition",
#'            "ResultStatusID",
#'            "ResultComment",
#'            "CreateTimestamp"),
#'   filter=list(
#'            DOI=c("==","10.25976/0gvo-9d12"),
#'            CharacteristicName=c("%in%","Specific conductance","pH"),
#'            ActivityStartYear=c(">",2010),
#'            LocationId=c("==","862774")
#'            ),
#'   top=5000L
#'   )
#'
qs_helper<-function(select=NULL,
                    filter=NULL,
                    top=NULL,
                    count=NULL){

  replace_table<-data.frame(
    r=c(">=","<=","!=",">","<", "=","==", "%in%"),
    api=c(" gte "," lte "," ne "," gt "," lt "," eq "," eq ", " in ")
  )

  out<-list()

  if (!is.null(select)){
    select <- lapply(select,trimws)
    out$`$select` <- paste0(select,collapse = ", ")
  }

  if (!is.null(top)){
    stopifnot(is.integer(top),length(top)==1)
    out$`$top` <- top
  }

  if (!is.null(count)){
    stopifnot(is.logical(count),length(count)==1)
    out$`$count` <- tolower(as.character(count))
  }

  if (!is.null(filter)){
    stopifnot(is.list(filter),
              !is.null(names(filter))
    )

    filter<-filter[!sapply(filter,function(x) is.na(x[[1]]))]
    filter<-filter[!sapply(filter,function(x) is.na(x[[2]])) &
                     !sapply(filter,is.null) &
                     !sapply(names(filter),is.na) &
                     !sapply(names(filter),is.null)]

    filter<-lapply(filter,trimws)
    filter<-lapply(filter,function(x) lapply(x,trimws))

    filter<-lapply(filter,
                   function(fl) {
                     opr <- replace_table$api[replace_table$r==fl[[1]]]
                     if (is.null(opr) || length(opr)==0) stop("Invalid operator supplied to 'filter'")
                     val <- lapply(fl[-c(1)],trimws)
                     val <- val[!sapply(val,is.na)]
                     val <- val[!sapply(val,function(x) x=="NA")]
                     if (is.null(val) || length(val)==0) return(NULL)
                     val <- paste0("'",val,"'")

                     if (length(val)==1 & opr == " in ") opr <- " eq "

                     if (opr == " in "){
                       out <- paste0("(",paste0(val,collapse = ", "),")")
                     } else {
                       out <- val
                     }

                     paste0(opr,out)
                   })

    filter<-filter[!sapply(filter,is.null) &
                     !sapply(names(filter),is.na) &
                     !sapply(names(filter),is.null)]

    filter<-paste(names(filter),filter,collapse = " and ")
    filter<-gsub("  "," ",filter)

    out$`$filter` <- as.character(filter)
  }

  return(out)
}
