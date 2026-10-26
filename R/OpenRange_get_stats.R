
#'Download model performance statistics
#'
#'OpenRange_get_stats provides information on model performance.
#' @param ... Additional arguments passed to internal functions.
#' @note This function relies on an Internet resource.  If that resource is
#'   unavailable, NULL will be returned along with an informative message.
#' @return List object containing one data.frame for each of the three modeling
#'   frameworks, or NULL if the database is unavailable.
#' @examples \donttest{
#'
#' range_stats <- OpenRange_get_stats()
#'
#' # OpenRange functions return NULL if the data source is unavailable,
#' # so check the output before using it.
#'
#' if(!is.null(range_stats)){
#'
#'   names(range_stats)
#'
#' }
#'
#'}
#' @family range functions
#' @family metadata functions
#' @export
OpenRange_get_stats <- function(...){

  # Each query returns NULL (with a message) if the database is unavailable.
  # Check the first query before issuing the others.

  rangebagging <- ranges_sql(query = "select * from ranges.statistics_rangebag ;", ...)

  if(is.null(rangebagging)){return(invisible(NULL))}

  ppm <- ranges_sql(query = "select * from ranges.statistics_ppm ;", ...)

  if(is.null(ppm)){return(invisible(NULL))}

  points <- ranges_sql(query = "select * from ranges.statistics_points ;", ...)

  if(is.null(points)){return(invisible(NULL))}

  out <-
  list(rangebagging = rangebagging,
       ppm = ppm,
       points = points)

  return(out)

}
