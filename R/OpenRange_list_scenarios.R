#'Download information on available climate change scenarios
#'
#'OpenRange_list_scenarios provides information on climate scenarios with range projections available.
#' @param ... Additional arguments passed to internal functions.
#' @note Details on the construction of BIEN range maps is available at http://bien.nceas.ucsb.edu/bien/biendata/bien-3/
#' @return Data.frame containing climate change scenarios available in BIEN.
#' @examples \dontrun{
#'
#' cc_scenarios <- OpenRange_list_scenarios()
#'
#'}
#' @family range functions
#' @family metadata functions
#' @export
OpenRange_list_scenarios <- function(...){
  
  
  # set the query
  query <- paste("SELECT DISTINCT  scenario_id, scenario, scenario_filecode, time_period, climate_model, rcp  
                    FROM ranges.range ;")
  
  
  return(ranges_sql(query = query))
  
  
}
