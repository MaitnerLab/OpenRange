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
