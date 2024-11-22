#' Available and required columns in the DataStream Schema
#'
#' @return a list of available and required column names
#' @export
#'
#' @examples
#' library(datastreamr.helper)
#' DS_schema_cols()
DS_schema_cols<-function(){
  link <- "https://datastream.org/schema"
  schema <- rlist::list.load(link)
  schema_cols<- names(schema$allOf[[1]]$properties)
  cols_required <- schema$allOf[[1]]$required

  return(
    list(
      all=schema_cols,
      required=cols_required
    )
  )
}
