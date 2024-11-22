#' Valid fields for DataStream API queries
#'
#' @return a list of valid fields for `select` and `filter`
#' @export
#'
#' @examples
#' library(datastreamr.helper)
#' DS_valid <- DS_valid_vals()
#'
#' # Valid fields to select
#' DS_valid$valid_select
#'
#' # Valid fields to filter
#' DS_valid$valid_filter
#'
#' # valid values for lookups
#' DS_valid$valid_lookup$ActivityMediaName

DS_valid_vals<-function(){
  val_SelFilt <- .get_valid_SelFilt()

  val_lookup <- lapply(val_SelFilt, function(x) x$filt)
  val_lookup <- unique(unlist(val_lookup))
  names(val_lookup) <- val_lookup
  val_lookup <- lapply(val_lookup, .get_valid_primary)
  val_lookup <- val_lookup[!sapply(val_lookup,is.null)]

  return(
    list(
      valid_select=lapply(val_SelFilt, function(x) x$sel),
      valid_filter=lapply(val_SelFilt, function(x) x$filt),
      valid_lookup=val_lookup
    )
  )
}
