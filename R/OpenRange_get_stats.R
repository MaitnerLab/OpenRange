
#'Download model performance statistics
#'
#'OpenRange_get_stats provides information on model performance.
#' @param ... Additional arguments passed to internal functions.
#' @return List object containing one data.frame for each of the three modelling frameworks.
#' @examples \donttest{
#'
#' range_stats <- OpenRange_get_stats()
#'
#'}
#' @family range functions
#' @family metadata functions
#' @export
OpenRange_get_stats <- function(...){
  
  out <- 
  list(rangebagging = ranges_sql(query = "select * from ranges.statistics_rangebag ;",...),
       ppm = ranges_sql(query = "select * from ranges.statistics_ppm ;",...),
       points = ranges_sql(query = "select * from ranges.statistics_points ;",...))
  

  
  
}