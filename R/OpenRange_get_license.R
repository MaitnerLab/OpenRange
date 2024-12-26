#'Download license information
#'
#'OpenRange_get_license provides information on model licenses.
#' @param ... Additional arguments passed to internal functions.
#' @return Data.frame containing information on the license associated with the maps.
#' @examples \donttest{
#'
#' range_license_info <- OpenRange_get_license()
#'
#'}
#' @family range functions
#' @family metadata functions
#' @export
OpenRange_get_license <- function(...){
  
    return(ranges_sql(query = "select * from ranges.license ;", ...))
    
  
}