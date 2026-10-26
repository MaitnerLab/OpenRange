#'Download license information
#'
#'OpenRange_get_license provides information on model licenses.
#' @param ... Additional arguments passed to internal functions.
#' @note This function relies on an Internet resource.  If that resource is
#'   unavailable, NULL will be returned along with an informative message.
#' @return Data.frame containing information on the license associated with the
#'   maps, or NULL if the database is unavailable.
#' @examples \donttest{
#'
#' range_license_info <- OpenRange_get_license()
#'
#' # OpenRange functions return NULL if the data source is unavailable,
#' # so check the output before using it.
#'
#' if(!is.null(range_license_info)){
#'
#'   head(range_license_info)
#'
#' }
#'
#'}
#' @family range functions
#' @family metadata functions
#' @export
OpenRange_get_license <- function(...){

    return(ranges_sql(query = "select * from ranges.license ;", ...))


}
