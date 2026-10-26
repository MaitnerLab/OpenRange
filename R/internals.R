#Internal functions

#########################################
#Value checkers
#########################################
##################################
#'Check that value is logical
#'
#'Helper function to check data format.
#' @keywords internal
.is_log <- function(x) {
  if (!inherits(x, 'logical')) {
    stop(sys.call()[-1], " should be logical", call. = FALSE)
  }
}


##################################
#'Check that value is logical or null
#'
#'Helper function to check data format.
#' @keywords internal
.is_log_or_null <- function(x) {
  if (!inherits(x, c('logical','NULL'))) {
    stop(sys.call()[-1], " should be logical or NULL", call. = FALSE)
  }
}

###################################
#'Check that value is character
#'
#'Helper function to check data format.
#' @keywords internal
.is_char <- function(x) {
  if (!inherits(x ,c("character","NULL"))) {
    stop(sys.call()[-1]," should be character", call. = FALSE)
  }
}

###################################

#'Check that value is numeric
#'
#'Helper function to check data format.
#' @keywords internal
.is_num <- function(x) {
  if (!inherits(x ,'numeric')) {
    stop(sys.call()[-1]," should be numeric", call. = FALSE)
  }
}

#################################
#################################

#########################################
#Graceful handling of Internet resources
#########################################
###################################

#'Report that an Internet resource is unavailable
#'
#'Helper function used to fail gracefully, returning NULL along with an
#'informative message rather than throwing an error, when a remote resource
#'cannot be reached or does not return what was expected.
#' @param resource Character. A short description of the resource that could not be reached.
#' @param details Character. Optional details (e.g. the underlying error message).
#' @return NULL, invisibly.
#' @keywords internal
.resource_unavailable <- function(resource,
                                 details = NULL) {

  message("The ", resource, " could not be reached.")
  message("The resource may be unavailable, overloaded, or may have changed. ",
          "Please check your internet connection and try again later.")

  if (!is.null(details) && any(nzchar(details))) {
    message("Details: ", paste(details, collapse = " "))
  }

  return(invisible(NULL))

}

###################################

#'Query the OpenRange API
#'
#'Helper function used to query the OpenRange API.  If the API cannot be
#'reached, returns an error status, or returns content that cannot be
#'interpreted, NULL is returned along with an informative message rather than
#'throwing an error.
#' @param url Character. The URL to be queried.
#' @return The parsed content of the API response, or NULL if the API could not
#'   be reached or its response could not be interpreted.
#' @keywords internal
#' @importFrom httr GET http_error status_code
#' @importFrom jsonlite fromJSON
.api_query <- function(url) {

  resource <- "OpenRange API (https://biendata.org)"

  # Download the data from the API

  req <- tryCatch(expr = GET(url = url),
                  error = function(e) {.resource_unavailable(resource = resource,
                                                             details = conditionMessage(e))},
                  warning = function(w) {.resource_unavailable(resource = resource,
                                                               details = conditionMessage(w))})

  if (is.null(req)) {return(invisible(NULL))}

  # check the http status

  if (http_error(req)) {

    return(.resource_unavailable(resource = resource,
                                 details = paste("HTTP status", status_code(req))))

  }

  # convert to raw

  raw_content <- tryCatch(expr = rawToChar(req$content),
                          error = function(e) {.resource_unavailable(resource = resource,
                                                                     details = conditionMessage(e))})

  if (is.null(raw_content)) {return(invisible(NULL))}

  # convert raw content from JSON to R object

  df <- tryCatch(expr = fromJSON(raw_content),
                 error = function(e) {.resource_unavailable(resource = resource,
                                                            details = conditionMessage(e))})

  if (is.null(df)) {return(invisible(NULL))}

  return(df)

}

###################################

#'Convert records returned by a remote resource to an sf object
#'
#'Helper function used to convert the records returned by the OpenRange
#'database or API into an sf object.  If the records cannot be interpreted as
#'spatial data (for example because the remote resource has changed), NULL is
#'returned along with an informative message rather than throwing an error.
#' @param df A data.frame returned by the OpenRange database or API.
#' @param crs Character. The coordinate reference system of the records.
#' @param resource Character. A short description of the resource the records came from.
#' @return An sf object, or NULL if the records could not be converted.
#' @keywords internal
#' @importFrom sf st_as_sf
.as_sf <- function(df,
                  crs = "epsg:4326",
                  resource = "OpenRange database") {

  out <- tryCatch(expr = st_as_sf(x = df,
                                  wkt = "st_astext",
                                  crs = crs),
                  error = function(e) {

                    message("The records returned by the ", resource,
                            " could not be interpreted as spatial data.")
                    message("The resource may have changed. ",
                            "Please try again later, and consider reporting this if it persists.")
                    message("Details: ", conditionMessage(e))

                    return(invisible(NULL))

                  })

  return(out)

}

#################################
#################################
